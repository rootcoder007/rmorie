# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Shared input-validation helpers for the statistical estimators.
# These consolidate the length / type / missing-data assertions that
# the rOpenSci statistical-software standards (srr, "G2" input
# structure) call for, so every public estimator can validate its
# inputs the same way instead of re-implementing ad-hoc checks. The
# `aaa_` filename prefix guarantees these load before any caller.

# ---------------------------------------------------------------------------
# G2 input-structure standards.  Tags are placed here, at the shared
# validator, rather than repeated at every estimator: srr requires each
# standard to be addressed somewhere in the source, and this is the one
# place the behaviour lives.
# ---------------------------------------------------------------------------

#' @srrstats {G2.0} `.morie_check_scalar()` asserts single-valued
#'   inputs; `.morie_check_numvec(min_len=)` asserts vector lengths.
#' @srrstats {G2.0a} Length expectations are documented on each
#'   estimator's `@param` entries (single-value parameters such as
#'   `alpha`, `seed`, `outcome` column names).
#' @srrstats {G2.1} `.morie_check_scalar(type=)` asserts input types.
#' @srrstats {G2.1a} Expected types are documented per `@param`.
#' @srrstats {G2.2} `.morie_check_scalar()` rejects multivariate input
#'   to parameters expected to be univariate (length != 1 errors).
#' @srrstats {G2.3} For univariate character input:
#' @srrstats {G2.3a} Estimators use `match.arg()` for enumerated
#'   character parameters (e.g. `backend`, `method`, `weights`).
#' @srrstats {G2.3b} Enumerated character parameters are matched with
#'   `match.arg()` and are documented as case-sensitive by design.
#' @srrstats {G2.4} Type conversions are performed explicitly:
#' @srrstats {G2.4a} Integer conversion via `as.integer()`.
#' @srrstats {G2.4b} Continuous conversion via `as.numeric()`
#'   (`.morie_check_numvec()`).
#' @srrstats {G2.4c} Character conversion via `as.character()`.
#' @srrstats {G2.4d} Factor handling uses `droplevels()` /
#'   `stats::model.matrix()`; see `.viable_terms()` in `study_core.R`.
#' @srrstats {G2.4e} Conversion from factor uses explicit `as.*()`
#'   rather than relying on implicit coercion.
#' @srrstats {G2.6} `.morie_check_numvec()` coerces one-dimensional
#'   input to a plain numeric vector regardless of class attributes.
#' @srrstats {G2.7} `.morie_check_data()` accepts any data.frame-like
#'   tabular input (data.frame, tibble, data.table, matrix).
#' @srrstats {G2.8} `.morie_check_data()` returns a plain data.frame so
#'   downstream sub-functions receive a single defined class.
#' @srrstats {G2.9} `.morie_check_data()` errors (rather than silently
#'   converting) on unsupported list-columns, so no information is lost
#'   without the caller being told.
#' @srrstats {G2.10} Column extraction uses `[[` throughout, giving
#'   consistent single-column behaviour across tabular classes.
#' @srrstats {G2.12} `.morie_check_data()` rejects list-columns with an
#'   informative error.
#' @srrstats {G2.13} `.morie_check_data(check_na=)` reports missing data
#'   in required columns as part of pre-processing.
#' @srrstats {G2.14} Missing-data handling is explicit per estimator:
#' @srrstats {G2.14a} `.morie_check_data(check_na = "error")` refuses to
#'   proceed when a required column carries NA, for estimators whose
#'   result would be meaningless on incomplete rows.
#' @srrstats {G2.14b} The `.morie_*_drop_na()` helpers drop incomplete
#'   rows with a recorded row count (ignore-with-message).
#' @srrstats {G2.14c} `morie_impute_column()` offers median / mean /
#'   mode / LOCF imputation as an explicit caller choice, and reports
#'   `n_imputed` so the cost stays visible.
#' @srrstats {G2.15} Estimators pass `na.rm = TRUE` or drop NA explicitly
#'   before calling base routines; none is called with the default
#'   `na.rm = FALSE` on data that may contain NA.
#' @srrstats {G2.16} `.morie_check_numvec(finite = TRUE)` rejects
#'   `NaN`/`Inf`/`-Inf` where undefined values are not meaningful.
#' @srrstats {G2.5} `.morie_check_factor(ordered=)` asserts whether a
#'   factor input must be ordered or unordered, erroring otherwise; it
#'   gates the ordinal outcome in `mrm_threshold_specific_ordinal()`,
#'   where an unordered factor's alphabetical levels would silently be
#'   read as the ordinal scale.
#' @srrstats {G2.11} `.morie_coerce_units()` accepts columns with a
#'   non-standard class but numeric storage (such as `units`-package
#'   columns), coercing them to plain numeric rather than erroring; it
#'   pre-processes every covariate column in
#'   `mrm_threshold_specific_ordinal()`.
#' @noRd
NULL

# Assert a data.frame-like table with the required columns present.
# Coerces tibbles / data.tables / matrices to a plain data.frame and
# rejects list-columns. Optionally reports missing values in `required`.
#' Internal helper: Morie Check Data
#' @noRd
.morie_check_data <- function(data, required = character(), arg = "data",
                              check_na = FALSE) {
  if (is.matrix(data)) data <- as.data.frame(data)
  if (!is.data.frame(data)) {
    stop(sprintf("`%s` must be a data.frame-like table, not %s.",
                 arg, class(data)[1L]), call. = FALSE)
  }
  data <- as.data.frame(data)                       # tibble/dt -> df (G2.7/G2.8)
  listcols <- names(data)[vapply(data, is.list, logical(1))]
  if (length(listcols)) {                           # G2.12
    stop(sprintf("`%s` has unsupported list-column(s): %s.",
                 arg, paste(listcols, collapse = ", ")), call. = FALSE)
  }
  missing_cols <- setdiff(required, names(data))
  if (length(missing_cols)) {
    stop(sprintf("`%s` is missing required column(s): %s.",
                 arg, paste(missing_cols, collapse = ", ")), call. = FALSE)
  }
  # G2.13 / G2.14: `check_na` selects the missing-data policy.
  #   TRUE / "message" -- report and let the estimator drop rows (G2.14b)
  #   "error"          -- refuse to proceed (G2.14a), for estimators whose
  #                       result would be meaningless on incomplete rows
  if (!isFALSE(check_na) && length(required)) {
    policy <- if (isTRUE(check_na)) "message" else match.arg(
      as.character(check_na), c("message", "error"))
    na_cols <- required[vapply(required,
                               function(c) anyNA(data[[c]]), logical(1))]
    if (length(na_cols)) {
      if (policy == "error") {
        stop(sprintf("`%s`: missing values in %s; this estimator cannot ",
                     arg, paste(na_cols, collapse = ", ")),
             "proceed on incomplete rows. Drop or impute them first ",
             "(see `morie_impute_column()`).", call. = FALSE)
      }
      message(sprintf("`%s`: missing values present in %s; ",
                      arg, paste(na_cols, collapse = ", ")),
              "incomplete rows are dropped by the estimator.")
    }
  }
  data
}

#' Impute missing values in a column
#'
#' Fills `NA` by a stated rule so an estimator that cannot proceed on
#' incomplete rows has an explicit alternative to dropping them
#' (G2.14c). The rule is always the caller's choice -- nothing is
#' imputed implicitly anywhere in the package.
#'
#' `"median"` and `"mean"` suit a roughly symmetric or skewed numeric
#' column respectively. `"mode"` is the only sensible option for a
#' categorical column. `"locf"` (last observation carried forward)
#' assumes the rows are in a meaningful order -- time, usually -- and is
#' wrong if they are not.
#'
#' Imputing narrows the apparent spread of a variable: the imputed
#' values carry no information but are counted as if they did, so any
#' downstream standard error is optimistic. `n_imputed` is returned so
#' that cost stays visible.
#'
#' @param x A vector with missing values.
#' @param method One of `"median"`, `"mean"`, `"mode"`, `"locf"`.
#' @return A list with `values` (the filled vector), `n_imputed` and
#'   `method`.
#' @examples
#' morie_impute_column(c(1, 2, NA, 4), "median")$values
#' @export
morie_impute_column <- function(x, method = c("median", "mean", "mode",
                                              "locf")) {
  method <- match.arg(method)
  miss <- is.na(x)
  n_imputed <- sum(miss)
  if (n_imputed == 0L) {
    return(list(values = x, n_imputed = 0L, method = method))
  }
  if (method %in% c("median", "mean")) {
    if (!is.numeric(x)) {
      stop(sprintf("`method = \"%s\"` needs a numeric column, got %s.",
                   method, class(x)[1L]), call. = FALSE)
    }
    fill <- if (method == "median") stats::median(x, na.rm = TRUE)
            else mean(x, na.rm = TRUE)
    x[miss] <- fill
  } else if (method == "mode") {
    tab <- table(x[!miss])
    if (!length(tab)) stop("`x` is entirely missing.", call. = FALSE)
    x[miss] <- if (is.factor(x)) names(tab)[which.max(tab)] else
      methods::as(names(tab)[which.max(tab)], class(x)[1L])
  } else {
    # locf: a leading NA has nothing to carry forward, so it stays NA
    # rather than being back-filled from the future.
    for (i in which(miss)) if (i > 1L) x[i] <- x[i - 1L]
  }
  list(values = x, n_imputed = as.integer(n_imputed), method = method)
}

# Assert a single-valued input of the given type.
#' Internal helper: Morie Check Scalar
#' @noRd
.morie_check_scalar <- function(x,
                                type = c("numeric", "character",
                                         "logical", "integer"),
                                arg = "x") {
  type <- match.arg(type)
  if (length(x) != 1L) {                            # G2.0 / G2.2
    stop(sprintf("`%s` must be a single value, got length %d.",
                 arg, length(x)), call. = FALSE)
  }
  ok <- switch(type,                                # G2.1
    numeric   = is.numeric(x),
    integer   = is.numeric(x),
    character = is.character(x),
    logical   = is.logical(x))
  if (!ok) stop(sprintf("`%s` must be %s.", arg, type), call. = FALSE)
  x
}

# Coerce to a finite numeric vector of at least `min_len` elements.
#' Internal helper: Morie Check Numvec
#' @noRd
.morie_check_numvec <- function(x, arg = "x", finite = TRUE, min_len = 1L) {
  x <- as.numeric(x)                                # G2.4b / G2.6
  if (length(x) < min_len) {
    stop(sprintf("`%s` must have length >= %d, got %d.",
                 arg, min_len, length(x)), call. = FALSE)
  }
  if (isTRUE(finite) && any(!is.finite(x))) {       # G2.16
    stop(sprintf("`%s` contains non-finite values (NA/NaN/Inf).", arg),
         call. = FALSE)
  }
  x
}

# Assert a factor input's ordered-ness (G2.5). `ordered = TRUE`/`FALSE`
# requires an ordered / unordered factor respectively; NA accepts either.
#' Internal helper: Morie Check Factor
#' @noRd
.morie_check_factor <- function(x, ordered = NA, arg = "x") {
  if (!is.factor(x)) {
    stop(sprintf("`%s` must be a factor.", arg), call. = FALSE)
  }
  if (!is.na(ordered)) {
    if (isTRUE(ordered) && !is.ordered(x)) {
      stop(sprintf("`%s` must be an *ordered* factor.", arg), call. = FALSE)
    }
    if (isFALSE(ordered) && is.ordered(x)) {
      stop(sprintf("`%s` must be an *unordered* factor.", arg), call. = FALSE)
    }
  }
  x
}

# Coerce a column that carries a non-standard class but is atomically
# numeric (e.g. a `units`-package column) to a plain numeric vector
# (G2.11), preserving the numeric values. Errors on a genuinely
# non-coercible column.
#' Internal helper: Morie Coerce Units
#' @noRd
.morie_coerce_units <- function(x, arg = "x") {
  if (is.numeric(x) && is.null(attr(x, "class"))) return(as.numeric(x))
  v <- tryCatch(as.numeric(x), warning = function(w) NULL,
                error = function(e) NULL)
  if (is.null(v) || all(is.na(v)) && !all(is.na(x))) {
    stop(sprintf("`%s` has a non-standard class that is not numeric-coercible.",
                 arg), call. = FALSE)
  }
  v
}

# Extended-test gate (G5.10-G5.12): TRUE only when the environment
# variable MORIE_EXTENDED_TESTS is set to a truthy value. Slow or
# data-dependent tests are wrapped in `if (.morie_extended_tests())`.
#' Internal helper: Morie Extended Tests
#' @noRd
.morie_extended_tests <- function() {
  v <- tolower(Sys.getenv("MORIE_EXTENDED_TESTS", ""))
  v %in% c("1", "true", "yes", "on")
}

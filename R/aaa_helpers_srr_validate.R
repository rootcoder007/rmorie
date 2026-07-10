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
#' @srrstats {G2.14a} Validators error on missing data where an estimator
#'   cannot proceed (e.g. calibration in `tox.R`).
#' @srrstats {G2.14b} The `.morie_*_drop_na()` helpers drop incomplete
#'   rows with a recorded row count (ignore-with-message).
#' @srrstats {G2.14c} Imputation is offered where appropriate
#'   (`morie_tox_left_censor_impute()`, `validation.R` imputers).
#' @srrstats {G2.15} Estimators pass `na.rm = TRUE` or drop NA explicitly
#'   before calling base routines; none is called with the default
#'   `na.rm = FALSE` on data that may contain NA.
#' @srrstats {G2.16} `.morie_check_numvec(finite = TRUE)` rejects
#'   `NaN`/`Inf`/`-Inf` where undefined values are not meaningful.
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
  if (isTRUE(check_na) && length(required)) {        # G2.13
    na_cols <- required[vapply(required,
                               function(c) anyNA(data[[c]]), logical(1))]
    if (length(na_cols)) {
      message(sprintf("`%s`: missing values present in %s; ",
                      arg, paste(na_cols, collapse = ", ")),
              "incomplete rows are dropped by the estimator.")
    }
  }
  data
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

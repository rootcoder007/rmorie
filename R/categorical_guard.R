# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Categorical-integrity guards (feat/native-specializations,
# module 25). Category-mapping errors are among the most damaging
# silent failures in applied statistics: numeric-coded categoricals
# imported from other packages (SPSS/Stata/SAS) invite
# `as.numeric(factor)` level-INDEX coercion, positional recodes
# reassign groups wholesale, and alphabetical releveling silently
# changes the reference category — any of which can relabel entire
# demographic groups and multiply reported odds ratios severalfold
# without a single warning. Published analyses have carried exactly
# this class of error for years before correction.
#
# rmorie's answer: (1) explicit name-based recoding that ERRORS on
# anything unmapped, (2) an import audit that flags every known
# hazard, (3) a before/after cross-tabulation verifier that proves a
# recode did what it claims, and (4) a hard error (not silent
# coercion) whenever a factor reaches a numeric treatment slot.

#' Recode a categorical column by explicit name-to-name mapping
#'
#' The only recode rmorie endorses: every mapping is written
#' \code{old_label = new_label} by NAME, positional and index-based
#' recoding are impossible, and any value not covered by the mapping
#' is an ERROR (never a silent \code{NA} or pass-through) unless
#' explicitly listed in \code{keep}.
#'
#' @param x A factor or character vector.
#' @param mapping Named character vector:
#'   \code{c(old_label = "new_label", ...)}.
#' @param keep Optional character vector of labels allowed to pass
#'   through unchanged (everything else must be mapped).
#' @return A character vector (with an \code{morie_recode_audit}
#'   attribute recording the applied mapping) — convert with
#'   \code{\link{morie_safe_factor}} to fix levels explicitly.
#' @examples
#' x <- c("W", "B", "O", "W")
#' morie_safe_recode(x, c(W = "White", B = "Black", O = "Other"))
#' @seealso \code{\link{morie_crosstab_verify}} to prove the recode.
#' @export
morie_safe_recode <- function(x, mapping, keep = character()) {
  if (is.factor(x)) x <- as.character(x)
  stopifnot(is.character(mapping), !is.null(names(mapping)),
            all(nzchar(names(mapping))))
  seen <- unique(x[!is.na(x)])
  unmapped <- setdiff(seen, c(names(mapping), keep))
  if (length(unmapped)) {
    stop("morie_safe_recode: values with NO mapping: ",
         paste(sQuote(unmapped), collapse = ", "),
         ". Every observed category must be mapped by name (or ",
         "listed in `keep`); silent pass-through is how group ",
         "labels get corrupted.", call. = FALSE)
  }
  out <- ifelse(is.na(x), NA_character_,
                ifelse(x %in% names(mapping),
                       unname(mapping[x]), x))
  attr(out, "morie_recode_audit") <- list(
    mapping = mapping, kept = keep,
    checksum = .rmorie_sha256_hex_impl(
      paste(names(mapping), mapping, sep = "=", collapse = ";")))
  out
}

#' Build a factor with explicit, verified levels
#'
#' \code{factor(x)} orders levels alphabetically, which silently
#' decides the reference category of every downstream regression.
#' This constructor requires the level set to be written out, errors
#' on values outside it, and records the declared reference level.
#'
#' @param x Character (or factor) vector.
#' @param levels Complete character vector of allowed levels, in the
#'   intended order — the FIRST is the reference category.
#' @param reference Optional; assert which level is the reference
#'   (must equal \code{levels[1]}).
#' @return A factor with exactly the declared levels.
#' @examples
#' morie_safe_factor(c("White", "Black", "White"),
#'                   levels = c("White", "Black"))
#' @export
morie_safe_factor <- function(x, levels, reference = NULL) {
  if (is.factor(x)) x <- as.character(x)
  stray <- setdiff(unique(x[!is.na(x)]), levels)
  if (length(stray)) {
    stop("morie_safe_factor: values outside the declared levels: ",
         paste(sQuote(stray), collapse = ", "), call. = FALSE)
  }
  if (!is.null(reference) && !identical(reference, levels[1L])) {
    stop("morie_safe_factor: declared reference ", sQuote(reference),
         " is not levels[1] (", sQuote(levels[1L]), "); reorder ",
         "`levels` so the reference is explicit and first.",
         call. = FALSE)
  }
  factor(x, levels = levels)
}

#' Audit the categorical columns of a data frame for coding hazards
#'
#' One call after every import, before any model. Reports, per
#' categorical column: storage, levels in order, counts, the
#' reference level R would use, and flags for the known hazards —
#' numeric-looking labels (the signature of codes imported from
#' SPSS/Stata without their value labels), still-labelled foreign
#' columns (\code{haven_labelled}), case-variant duplicate labels,
#' unused levels, and high-cardinality accidents.
#'
#' @param data A data frame.
#' @param cols Columns to audit (default: every factor/character/
#'   labelled column).
#' @return An object of class \code{morie_category_audit}: a data
#'   frame with one row per column (\code{column}, \code{storage},
#'   \code{n_levels}, \code{levels}, \code{reference},
#'   \code{hazards}) plus a \code{clean} attribute. Its print method
#'   shouts the hazards.
#' @examples
#' df <- data.frame(race = factor(c("1", "2", "2", "3")),
#'                  city = c("Toronto", "toronto", "Ottawa", "Ottawa"))
#' morie_audit_categories(df)
#' @export
morie_audit_categories <- function(data, cols = NULL) {
  stopifnot(is.data.frame(data))
  is_cat <- function(v) {
    is.factor(v) || is.character(v) ||
      inherits(v, c("haven_labelled", "labelled"))
  }
  if (is.null(cols)) cols <- names(data)[vapply(data, is_cat,
                                                logical(1))]
  rows <- lapply(cols, function(cn) {
    v <- data[[cn]]
    hazards <- character(0)
    if (inherits(v, c("haven_labelled", "labelled"))) {
      hazards <- c(hazards,
                   "still carries foreign value labels: decode to ",
                   "labels BEFORE analysis (the numeric codes are ",
                   "NOT the categories)")
      v <- as.character(v)
    }
    lv <- if (is.factor(v)) levels(v) else
      as.character(sort(unique(v[!is.na(v)])))
    obs <- unique(as.character(v[!is.na(v)]))
    if (length(lv) && all(grepl("^[0-9.]+$", lv))) {
      hazards <- c(hazards, paste0(
        "all labels numeric-looking (", paste(utils::head(lv, 4),
                                              collapse = ","),
        "...): likely imported CODES whose value labels were lost; ",
        "as.numeric() on this column returns level INDICES, not ",
        "data"))
    }
    lc <- tolower(lv)
    if (anyDuplicated(lc)) {
      dup <- lv[lc %in% lc[duplicated(lc)]]
      hazards <- c(hazards, paste0("case-variant duplicate labels: ",
                                   paste(sQuote(dup),
                                         collapse = ", ")))
    }
    if (is.factor(v) && length(setdiff(lv, obs))) {
      hazards <- c(hazards, paste0(
        "unused levels: ", paste(sQuote(setdiff(lv, obs)),
                                 collapse = ", ")))
    }
    if (length(lv) > 50L) {
      hazards <- c(hazards, paste0(length(lv),
                                   " levels: identifier mistaken ",
                                   "for a category?"))
    }
    data.frame(column = cn,
               storage = paste(class(data[[cn]]), collapse = "/"),
               n_levels = length(lv),
               levels = paste(utils::head(lv, 8), collapse = "|"),
               reference = if (length(lv)) lv[1] else NA_character_,
               hazards = if (length(hazards))
                 paste(hazards, collapse = " ;; ") else "",
               stringsAsFactors = FALSE)
  })
  out <- if (length(rows)) do.call(rbind, rows) else
    data.frame(column = character(), storage = character(),
               n_levels = integer(), levels = character(),
               reference = character(), hazards = character())
  attr(out, "clean") <- !any(nzchar(out$hazards))
  class(out) <- c("morie_category_audit", "data.frame")
  out
}

#' @examples
#' \donttest{
#' df <- data.frame(race = factor(c("1", "2", "2", "3")),
#'                  city = c("Toronto", "toronto", "Ottawa", "Ottawa"))
#' obj <- morie_audit_categories(df)
#' print(obj)
#' }
#' @export
print.morie_category_audit <- function(x, ...) {
  cat("Categorical audit:", nrow(x), "column(s)\n")
  for (i in seq_len(nrow(x))) {
    cat(sprintf("  %-16s %-10s %d level(s), reference %s\n",
                x$column[i], x$storage[i], x$n_levels[i],
                sQuote(x$reference[i])))
    if (nzchar(x$hazards[i])) {
      for (h in strsplit(x$hazards[i], " ;; ", fixed = TRUE)[[1]]) {
        cat("    !! HAZARD:", h, "\n")
      }
    }
  }
  if (isTRUE(attr(x, "clean"))) cat("  no hazards detected\n")
  invisible(x)
}

#' Prove a recode with a before/after cross-tabulation
#'
#' The check that catches category-mapping corruption on the day it
#' happens instead of years later: cross-tabulates the original
#' against the recoded values and ERRORS unless the realized mapping
#' is a function (each old category to exactly one new category)
#' that matches the declared mapping, with no rows lost.
#'
#' @param original,recoded Parallel vectors (before / after).
#' @param declared Named character vector: the mapping the analyst
#'   CLAIMS was applied (\code{c(old = "new", ...)}). Identity is
#'   assumed for old values absent from \code{declared}.
#' @return Invisibly, the cross-tabulation (as a data frame), if and
#'   only if every check passes.
#' @examples
#' x <- c("W", "B", "W"); y <- c("White", "Black", "White")
#' morie_crosstab_verify(x, y, c(W = "White", B = "Black"))
#' @export
morie_crosstab_verify <- function(original, recoded, declared) {
  if (is.factor(original)) original <- as.character(original)
  if (is.factor(recoded)) recoded <- as.character(recoded)
  if (length(original) != length(recoded)) {
    stop("morie_crosstab_verify: length mismatch (", length(original),
         " vs ", length(recoded), "): rows were lost or duplicated ",
         "during the recode.", call. = FALSE)
  }
  if (!identical(is.na(original), is.na(recoded))) {
    stop("morie_crosstab_verify: missingness changed during the ",
         "recode (values silently became NA, or NAs were filled).",
         call. = FALSE)
  }
  ok <- !is.na(original)
  tab <- table(original = original[ok], recoded = recoded[ok])
  fan_out <- rowSums(tab > 0)
  if (any(fan_out > 1L)) {
    bad <- names(fan_out)[fan_out > 1L]
    stop("morie_crosstab_verify: original category mapped to ",
         "MULTIPLE new categories: ", paste(sQuote(bad),
                                            collapse = ", "),
         ". The recode is not a function of the category label.",
         call. = FALSE)
  }
  realized <- apply(tab, 1L, function(r)
    colnames(tab)[which(r > 0)])
  for (old in names(realized)) {
    expected <- if (old %in% names(declared)) {
      unname(declared[old])
    } else {
      old
    }
    if (!identical(realized[[old]], expected)) {
      stop("morie_crosstab_verify: ", sQuote(old), " was mapped to ",
           sQuote(realized[[old]]), " but the declared mapping says ",
           sQuote(expected), ". THIS is how groups get swapped; fix ",
           "the recode before any model runs.", call. = FALSE)
    }
  }
  invisible(as.data.frame(tab))
}

#' Internal guard: refuse silent factor-to-numeric treatment coercion
#'
#' Called by the MRM pipeline (and available to every estimator):
#' when a column that must be numeric 0/1 arrives as a factor or
#' character, this ERRORS with the level-index explanation instead of
#' letting \code{as.numeric(factor)} return 1, 2, ... level codes.
#' @noRd
.morie_guard_binary_treatment <- function(x, col) {
  if (is.factor(x) || is.character(x)) {
    lv <- if (is.factor(x)) levels(x) else unique(as.character(x))
    stop("Column ", sQuote(col), " is categorical (",
         paste(sQuote(utils::head(lv, 4)), collapse = ", "),
         "...). Refusing to coerce: as.numeric() on a factor ",
         "returns level INDICES (1, 2, ...), not your data, and a ",
         "mis-ordered level silently relabels every observation. ",
         "Encode explicitly first, e.g. morie_safe_recode() + ",
         "as.integer(x == \"treated_label\").", call. = FALSE)
  }
  ux <- unique(x[!is.na(x)])
  if (!all(ux %in% c(0, 1))) {
    stop("Column ", sQuote(col), " must be binary 0/1 (saw: ",
         paste(utils::head(ux, 5), collapse = ", "), ").",
         call. = FALSE)
  }
  invisible(TRUE)
}

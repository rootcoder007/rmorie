# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Categorical-integrity guards (feat/native-specializations,
# module 25). Category-mapping errors are among the most damaging
# silent failures in applied statistics: numeric-coded categoricals
# imported from other packages (SPSS/Stata/SAS) invite
# `as.numeric(factor)` level-INDEX coercion, positional recodes
# reassign groups wholesale, and alphabetical releveling silently
# changes the reference category -- any of which can relabel entire
# demographic groups and multiply reported odds ratios severalfold
# without a single warning. The Ontario Human Rights Commission's 2020
# report *A Disparate Impact* carried exactly this error for two and a half
# years: the four race codes rotated one place between the SPSS file and the
# R analysis, and reported odds ratios of 30 to 58 corrected to 4 to 5
# (OHRC correction, 26 January 2023; Jung 2022 independent review).
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
#'   attribute recording the applied mapping) -- convert with
#'   \code{\link{morie_safe_factor}} to fix levels explicitly.
#' @examples
#' x <- c("W", "B", "O", "W")
#' morie_safe_recode(x, c(W = "White", B = "Black", O = "Other"))
#' @seealso \code{\link{morie_crosstab_verify}} to prove the recode.
#' @export
morie_safe_recode <- function(x, mapping, keep = character()) {
  if (is.factor(x)) x <- as.character(x)
  stopifnot(
    is.character(mapping), !is.null(names(mapping)),
    all(nzchar(names(mapping)))
  )
  seen <- unique(x[!is.na(x)])
  unmapped <- setdiff(seen, c(names(mapping), keep))
  if (length(unmapped)) {
    stop("morie_safe_recode: values with NO mapping: ",
      paste(sQuote(unmapped), collapse = ", "),
      ". Every observed category must be mapped by name (or ",
      "listed in `keep`); silent pass-through is how group ",
      "labels get corrupted.",
      call. = FALSE
    )
  }
  out <- ifelse(is.na(x), NA_character_,
    ifelse(x %in% names(mapping),
      unname(mapping[x]), x
    )
  )
  attr(out, "morie_recode_audit") <- list(
    mapping = mapping, kept = keep,
    checksum = .rmorie_sha256_hex_impl(
      paste(names(mapping), mapping, sep = "=", collapse = ";")
    )
  )
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
#'   intended order -- the FIRST is the reference category.
#' @param reference Optional; assert which level is the reference
#'   (must equal \code{levels[1]}).
#' @return A factor with exactly the declared levels.
#' @examples
#' morie_safe_factor(c("White", "Black", "White"),
#'   levels = c("White", "Black")
#' )
#' @export
morie_safe_factor <- function(x, levels, reference = NULL) {
  if (is.factor(x)) x <- as.character(x)
  stray <- setdiff(unique(x[!is.na(x)]), levels)
  if (length(stray)) {
    stop("morie_safe_factor: values outside the declared levels: ",
      paste(sQuote(stray), collapse = ", "),
      call. = FALSE
    )
  }
  if (!is.null(reference) && !identical(reference, levels[1L])) {
    stop("morie_safe_factor: declared reference ", sQuote(reference),
      " is not levels[1] (", sQuote(levels[1L]), "); reorder ",
      "`levels` so the reference is explicit and first.",
      call. = FALSE
    )
  }
  factor(x, levels = levels)
}

#' Audit the categorical columns of a data frame for coding hazards
#'
#' One call after every import, before any model. Reports, per
#' categorical column: storage, levels in order, counts, the
#' reference level R would use, and flags for the known hazards --
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
#' df <- data.frame(
#'   race = factor(c("1", "2", "2", "3")),
#'   city = c("Toronto", "toronto", "Ottawa", "Ottawa")
#' )
#' morie_audit_categories(df)
#' @export
morie_audit_categories <- function(data, cols = NULL) {
  stopifnot(is.data.frame(data))
  is_cat <- function(v) {
    is.factor(v) || is.character(v) ||
      inherits(v, c("haven_labelled", "labelled"))
  }
  if (is.null(cols)) {
    cols <- names(data)[vapply(
      data, is_cat,
      logical(1)
    )]
  }
  rows <- lapply(cols, function(cn) {
    v <- data[[cn]]
    hazards <- character(0)
    if (inherits(v, c("haven_labelled", "labelled"))) {
      hazards <- c(
        hazards,
        "still carries foreign value labels: decode to ",
        "labels BEFORE analysis (the numeric codes are ",
        "NOT the categories)"
      )
      v <- as.character(v)
    }
    lv <- if (is.factor(v)) {
      levels(v)
    } else {
      as.character(sort(unique(v[!is.na(v)]), method = "radix"))
    }
    obs <- unique(as.character(v[!is.na(v)]))
    if (length(lv) && all(grepl("^[0-9.]+$", lv))) {
      hazards <- c(hazards, paste0(
        "all labels numeric-looking (", paste(utils::head(lv, 4),
          collapse = ","
        ),
        "...): likely imported CODES whose value labels were lost; ",
        "as.numeric() on this column returns level INDICES, not ",
        "data"
      ))
    }
    if (length(lv) && all(grepl("^[0-9]+[.):]? ?[A-Za-z]", lv))) {
      hazards <- c(hazards, paste0(
        "labels carry code prefixes (", paste(utils::head(lv, 3), collapse = ","),
        "...): the code is part of the string, so the level order is the CODE ",
        "order; any positional relabel with labels in another order rotates ",
        "the groups. Decode by code (morie_safe_recode / morie_safe_relabel), never by position"))
    }
    inv <- "[\\s\u00a0\u1680\u2000-\u200b\u2028\u2029\u202f\u205f\u3000\ufeff]"
    padded <- lv[grepl(paste0("^", inv, "|", inv, "$"), lv, perl = TRUE)]
    if (length(padded)) {
      hazards <- c(hazards, paste0(
        "labels with leading/trailing whitespace (incl. non-breaking): ",
        paste(sQuote(padded), collapse = ", "),
        ": a space splits one category into two"))
    }
    core <- gsub(paste0("^", inv, "+|", inv, "+$"), "", lv, perl = TRUE)
    if (anyDuplicated(core)) {
      hazards <- c(hazards, paste0(
        "whitespace-variant duplicate labels: ",
        paste(sQuote(lv[core %in% core[duplicated(core)]]), collapse = ", ")))
    }
    if (any(!nzchar(core))) {
      hazards <- c(hazards,
        "empty-string label \"\": missingness stored as a category")
    }
    sentinels <- c("NA", "N/A", "NAN", "NULL", "NONE", ".", "-", "?")
    sentinel <- lv[toupper(core) %in% sentinels]
    if (length(sentinel)) {
      hazards <- c(hazards, paste0("missing-value sentinel stored as a label: ",
                                   paste(sQuote(sentinel), collapse = ", ")))
    }
    if (length(lv) && (!nzchar(core[1]) || lv[1] != core[1] ||
                         lv[1] %in% sentinel)) {
      hazards <- c(hazards, paste0(
        "the REFERENCE level ", sQuote(lv[1]),
        " is empty, a sentinel, or differs from a real label only by ",
        "invisible characters: every model on this column is baselined on it"))
    }
    lc <- tolower(core)
    # a case-variant pair is two DIFFERENT trimmed labels that agree once
    # lower-cased; a pair that differs only by whitespace was reported above
    case_groups <- split(core, lc)
    case_groups <- case_groups[vapply(case_groups, function(g) length(unique(g)) > 1L, TRUE)]
    if (length(case_groups)) {
      cv <- lv[lc %in% names(case_groups)]
      hazards <- c(hazards, paste0(
        "case-variant duplicate labels: ",
        paste(sQuote(cv), collapse = ", ")
      ))
    }
    if (is.factor(v) && length(setdiff(lv, obs))) {
      hazards <- c(hazards, paste0(
        "unused levels: ", paste(sQuote(setdiff(lv, obs)),
          collapse = ", "
        )
      ))
    }
    if (length(lv) > 50L) {
      hazards <- c(hazards, paste0(
        length(lv),
        " levels: identifier mistaken ",
        "for a category?"
      ))
    }
    data.frame(
      column = cn,
      storage = paste(class(data[[cn]]), collapse = "/"),
      n_levels = length(lv),
      levels = paste(utils::head(lv, 8), collapse = "|"),
      reference = if (length(lv)) lv[1] else NA_character_,
      hazards = if (length(hazards)) {
        paste(hazards, collapse = " ;; ")
      } else {
        ""
      },
      stringsAsFactors = FALSE
    )
  })
  out <- if (length(rows)) {
    do.call(rbind, rows)
  } else {
    data.frame(
      column = character(), storage = character(),
      n_levels = integer(), levels = character(),
      reference = character(), hazards = character()
    )
  }
  attr(out, "clean") <- !any(nzchar(out$hazards))
  class(out) <- c("morie_category_audit", "data.frame")
  out
}

#' Print method for \code{morie_category_audit} objects
#'
#' @param x A \code{morie_category_audit} object.
#' @param ... Ignored; accepted for S3 consistency.
#' @return The value of `invisible`.
#' @examples
#' \donttest{
#' df <- data.frame(
#'   race = factor(c("1", "2", "2", "3")),
#'   city = c("Toronto", "toronto", "Ottawa", "Ottawa")
#' )
#' obj <- morie_audit_categories(df)
#' print(obj)
#' }
#' @export
#' @keywords internal
print.morie_category_audit <- function(x, ...) {
  cat("Categorical audit:", nrow(x), "column(s)\n")
  for (i in seq_len(nrow(x))) {
    cat(sprintf(
      "  %-16s %-10s %d level(s), reference %s\n",
      x$column[i], x$storage[i], x$n_levels[i],
      sQuote(x$reference[i])
    ))
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
#' x <- c("W", "B", "W")
#' y <- c("White", "Black", "White")
#' morie_crosstab_verify(x, y, c(W = "White", B = "Black"))
#' @export
morie_crosstab_verify <- function(original, recoded, declared) {
  if (is.factor(original)) original <- as.character(original)
  if (is.factor(recoded)) recoded <- as.character(recoded)
  if (length(original) != length(recoded)) {
    stop("morie_crosstab_verify: length mismatch (", length(original),
      " vs ", length(recoded), "): rows were lost or duplicated ",
      "during the recode.",
      call. = FALSE
    )
  }
  if (!identical(is.na(original), is.na(recoded))) {
    stop("morie_crosstab_verify: missingness changed during the ",
      "recode (values silently became NA, or NAs were filled).",
      call. = FALSE
    )
  }
  ok <- !is.na(original)
  tab <- table(original = original[ok], recoded = recoded[ok])
  fan_out <- rowSums(tab > 0)
  if (any(fan_out > 1L)) {
    bad <- names(fan_out)[fan_out > 1L]
    stop("morie_crosstab_verify: original category mapped to ",
      "MULTIPLE new categories: ", paste(sQuote(bad),
        collapse = ", "
      ),
      ". The recode is not a function of the category label.",
      call. = FALSE
    )
  }
  realized <- apply(tab, 1L, function(r) {
    colnames(tab)[which(r > 0)]
  })
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
        "the recode before any model runs.",
        call. = FALSE
      )
    }
  }
  invisible(as.data.frame(tab))
}

#' Verify recoded category counts against the counts a release published
#'
#' The check that would have caught a swapped race label on the day: after
#' a recode, the number of rows per label must equal the counts the source
#' published for those labels. If they do not, the function also tries
#' every permutation of the labels and says whether the observed counts
#' match the published ones under some relabelling, which is the
#' signature of labels attached to the wrong groups.
#'
#' @param x A character or factor vector after recoding.
#' @param published Named numeric vector: label = published count.
#' @param tolerance Absolute count tolerance per label (default 0).
#' @param strict When \code{TRUE} (the default) a mismatch is an error, so a
#'   pipeline stops on the day. When \code{FALSE} the result is returned with
#'   \code{ok = FALSE}, the \code{permutation} that would explain the counts
#'   and the \code{message} the error would have carried.
#' @return Invisibly, a list with \code{counts} (observed), \code{published},
#'   \code{ok}, and \code{permutation} (the relabelling under which the
#'   observed counts match the published ones, or \code{NULL}). Errors
#'   when the counts disagree.
#' @examples
#' x <- c("White", "White", "Black", "Indigenous", "White", "Black")
#' morie_marginals_verify(x, c(White = 3, Black = 2, Indigenous = 1))
#' @export
morie_marginals_verify <- function(x, published, tolerance = 0,
                                   strict = TRUE) {
  if (is.factor(x)) x <- as.character(x)
  stopifnot(is.numeric(published), !is.null(names(published)),
            all(nzchar(names(published))))
  labs <- names(published)
  obs <- vapply(labs, function(l) sum(!is.na(x) & x == l), numeric(1))
  extra <- setdiff(unique(x[!is.na(x)]), labs)
  if (length(extra)) {
    msg <- paste0("morie_marginals_verify: labels present in the data but ",
                  "not in the published counts: ",
                  paste(sQuote(extra), collapse = ", "))
    if (strict) stop(msg, call. = FALSE)
    return(list(counts = obs, published = published[labs], ok = FALSE,
                permutation = NULL, message = msg))
  }
  ok <- all(abs(obs - published[labs]) <= tolerance)
  perm <- NULL
  if (!ok && length(labs) <= 7L) {
    for (p in .morie_label_perms(labs)) {
      relabelled <- stats::setNames(obs, p)[labs]
      if (all(abs(relabelled - published[labs]) <= tolerance) &&
          !identical(p, labs)) {
        perm <- stats::setNames(p, labs)
        break
      }
    }
  }
  out <- list(counts = obs, published = published[labs], ok = ok,
              permutation = perm, message = NULL)
  if (!ok) {
    detail <- paste(sprintf("%s: observed %s, published %s", labs, obs,
                            published[labs]), collapse = "; ")
    hint <- if (!is.null(perm)) {
      moved <- names(perm)[perm != names(perm)]
      paste0(" The observed counts match the published ones if the labels ",
             "are permuted (", paste(sprintf("%s -> %s", moved, perm[moved]),
                                     collapse = ", "),
             "): the labels are attached to the wrong groups.")
    } else {
      ""
    }
    out$message <- paste0("morie_marginals_verify: recoded counts do not ",
                          "match the published counts. ", detail, ".", hint)
    if (strict) stop(out$message, call. = FALSE)
    return(out)
  }
  invisible(out)
}

#' Check reported odds ratios against a labelled table, under every relabelling
#'
#' Given a k-by-2 table of counts (rows = groups, columns = outcome absent /
#' present), a reference group and the odds ratios a report states for the
#' other groups, this recomputes the odds ratios from the table and then
#' recomputes them under every permutation of the row labels (and with the
#' outcome columns swapped). It says whether the reported values follow
#' from the table as labelled, and if not, which relabelling reproduces
#' them. A four-fold odds ratio that a report gives as thirty-six-fold is
#' typically reproduced exactly by one such permutation.
#'
#' @param counts Numeric matrix with row names (groups) and two columns
#'   (outcome absent, outcome present), in that order.
#' @param reference Row name of the reference group.
#' @param reported Named numeric vector of the reported odds ratios, one per
#'   non-reference row.
#' @param tolerance Relative tolerance for a match (default 0.05).
#' @return A list: \code{computed} (odds ratios from the table as labelled),
#'   \code{reported}, \code{consistent} (the reported values follow from the
#'   labels as given), \code{matches} (a data frame of the relabellings that
#'   reproduce the reported values, possibly none), and \code{verdict}.
#' @examples
#' tab <- matrix(c(900, 100, 700, 300, 400, 600), ncol = 2, byrow = TRUE,
#'               dimnames = list(c("A", "B", "C"), c("no", "yes")))
#' morie_odds_ratio_check(tab, "A", c(B = 3.857, C = 13.5))
#' @export
morie_odds_ratio_check <- function(counts, reference, reported,
                                   tolerance = 0.05) {
  counts <- as.matrix(counts)
  if (ncol(counts) != 2L || is.null(rownames(counts))) {
    stop("morie_odds_ratio_check: `counts` must be a k-by-2 matrix with ",
         "row names (groups) and columns outcome-absent, outcome-present.",
         call. = FALSE)
  }
  labs <- rownames(counts)
  if (!reference %in% labs) {
    stop("morie_odds_ratio_check: reference ", sQuote(reference),
         " is not a row of `counts`.", call. = FALSE)
  }
  others <- setdiff(labs, reference)
  if (is.null(names(reported)) || !all(others %in% names(reported))) {
    stop("morie_odds_ratio_check: `reported` must be named by every ",
         "non-reference row: ", paste(sQuote(others), collapse = ", "),
         call. = FALSE)
  }
  reported <- reported[others]
  or_of <- function(m) {
    ref_odds <- m[reference, 2L] / m[reference, 1L]
    vapply(others, function(r) (m[r, 2L] / m[r, 1L]) / ref_odds, numeric(1))
  }
  close_to <- function(a, b) {
    all(is.finite(a) & is.finite(b)) &&
      all(abs(a - b) <= tolerance * pmax(abs(b), .Machine$double.eps))
  }
  computed <- or_of(counts)
  consistent <- close_to(computed, reported)
  rows <- list()
  if (length(labs) <= 7L) {
    for (p in .morie_label_perms(labs)) {
      for (swap_cols in c(FALSE, TRUE)) {
        m <- counts
        if (swap_cols) m <- m[, 2:1, drop = FALSE]
        rownames(m) <- p
        m <- m[labs, , drop = FALSE]
        if (identical(p, labs) && !swap_cols) next
        if (close_to(or_of(m), reported)) {
          moved <- labs[p != labs]
          rows[[length(rows) + 1L]] <- data.frame(
            relabelling = if (length(moved)) {
              paste(sprintf("%s -> %s", moved, p[p != labs]), collapse = ", ")
            } else {
              "none"
            },
            outcome_columns_swapped = swap_cols,
            stringsAsFactors = FALSE)
        }
      }
    }
  }
  matches <- if (length(rows)) do.call(rbind, rows) else
    data.frame(relabelling = character(), outcome_columns_swapped = logical(),
               stringsAsFactors = FALSE)
  verdict <- if (consistent) {
    "the reported odds ratios follow from the table as labelled"
  } else if (nrow(matches)) {
    paste0("the reported odds ratios do NOT follow from the table as ",
           "labelled; they are reproduced under a relabelling (",
           matches$relabelling[1L],
           if (matches$outcome_columns_swapped[1L]) "; outcome columns swapped" else "",
           "): the groups were mislabelled, not the software")
  } else {
    "the reported odds ratios follow from no relabelling of this table"
  }
  list(computed = computed, reported = reported, consistent = consistent,
       matches = matches, verdict = verdict)
}

# All permutations of a character vector, in lexicographic order of index
# (the same order Python's itertools.permutations produces).
.morie_label_perms <- function(v) {
  n <- length(v)
  if (n <= 1L) return(list(v))
  out <- list()
  for (i in seq_len(n)) {
    for (rest in .morie_label_perms(v[-i])) out[[length(out) + 1L]] <- c(v[i], rest)
  }
  out
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
      "as.integer(x == \"treated_label\").",
      call. = FALSE
    )
  }
  ux <- unique(x[!is.na(x)])
  if (!all(ux %in% c(0, 1))) {
    stop("Column ", sQuote(col), " must be binary 0/1 (saw: ",
      paste(utils::head(ux, 5), collapse = ", "), ").",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

# ---- positional relabels, labelled imports, and forensics ------------------
# The documented case (OHRC, Correction to "A Disparate Impact", 26 January
# 2023): codes 1 = White, 2 = Black, 3 = Other, 4 = Unknown; the labels in
# alphabetical order are Black, Other, Unknown, White; assigning them by
# position gives exactly the reported rotation (White read as Black, Black
# as Other, Other as Unknown, Unknown as White). Nothing in a file transfer
# picks the sort order of your labels. A positional relabel does.

#' Relabel a categorical variable by name, never by position
#'
#' A relabel is only safe when every old level is mapped BY NAME to its new
#' label. Assigning a vector of labels by position (the `levels<-` idiom,
#' or `factor(x, labels = ...)` with labels in a different order from the
#' codes) is the mechanism behind the documented four-way rotation in the
#' OHRC 2023 correction, so this function refuses unnamed mappings outright.
#'
#' @param x A factor or character vector.
#' @param mapping Named character vector, old label to new label. Every
#'   observed level must appear as a name unless listed in `keep`.
#' @param keep Levels allowed to pass through unchanged.
#' @return A factor with the new labels, levels in the order of `mapping`,
#'   carrying a `morie_recode_audit` attribute (see
#'   \code{\link{morie_safe_recode}}).
#' @seealso \code{\link{morie_safe_recode}},
#'   \code{\link{morie_relabel_forensics}},
#'   \code{\link{morie_decode_labelled}}
#' @examples
#' f <- factor(c("W", "B", "O", "W"))
#' morie_safe_relabel(f, c(W = "White", B = "Black", O = "Other"))
#' try(morie_safe_relabel(f, c("Black", "Other", "White")))
#' @export
morie_safe_relabel <- function(x, mapping, keep = character()) {
  if (is.null(names(mapping)) || !all(nzchar(names(mapping)))) {
    stop("morie_safe_relabel: `mapping` must be NAMED (old label = new label). ",
         "Assigning labels by POSITION is how four race codes were rotated ",
         "in a published analysis (OHRC correction, 26 January 2023): the ",
         "labels were in alphabetical order, the codes were not.",
         call. = FALSE)
  }
  old <- if (is.factor(x)) levels(x) else as.character(sort(unique(x[!is.na(x)]), method = "radix"))
  unmapped <- setdiff(old, c(names(mapping), keep))
  if (length(unmapped)) {
    stop("morie_safe_relabel: level(s) with NO mapping: ", paste(paste0("'", unmapped, "'"), collapse = ", "),
         ". Name every level or list it in `keep`.", call. = FALSE)
  }
  out <- morie_safe_recode(as.character(x), mapping, keep = keep)
  lev <- unique(c(unname(mapping), keep))
  f <- factor(as.character(out), levels = lev[lev %in% as.character(out) | lev %in% unname(mapping)])
  attr(f, "morie_recode_audit") <- attr(out, "morie_recode_audit")
  f
}

#' Decode a labelled import by its value labels, by code
#'
#' Vectors read from SPSS, Stata or SAS carry their categories as numeric
#' codes with a `labels` attribute (label = code). The categories are the
#' labels looked up BY CODE; the codes themselves, their order, and the
#' alphabetical order of the labels are all irrelevant, and treating any of
#' them as the category is the documented failure. This function looks each
#' code up in the attribute and refuses codes that have no label.
#'
#' @param x A vector with a `labels` attribute (as produced by haven), or a
#'   plain numeric vector with `value_labels` supplied.
#' @param value_labels Optional named vector, code = label, used when `x`
#'   carries no attribute.
#' @return A factor whose levels are the labels in CODE order (not
#'   alphabetical), with a `morie_recode_audit` attribute.
#' @seealso \code{\link{morie_safe_recode}}, \code{\link{morie_safe_relabel}}
#' @examples
#' x <- structure(c(1, 2, 2, 4), labels = c(White = 1, Black = 2, Other = 3, Unknown = 4))
#' morie_decode_labelled(x)
#' @export
morie_decode_labelled <- function(x, value_labels = NULL) {
  lab <- attr(x, "labels", exact = TRUE)
  if (!is.null(lab)) {
    if (is.null(names(lab))) stop("morie_decode_labelled: the `labels` attribute has no names", call. = FALSE)
    value_labels <- stats::setNames(names(lab), as.character(unname(lab)))
  }
  if (is.null(value_labels)) {
    stop("morie_decode_labelled: `x` carries no `labels` attribute and `value_labels` was not ",
         "supplied; the codes alone are NOT the categories", call. = FALSE)
  }
  codes <- as.character(unclass(x))
  out <- morie_safe_recode(codes, value_labels)
  f <- factor(as.character(out), levels = unname(value_labels[order(suppressWarnings(as.numeric(names(value_labels))), names(value_labels))]))
  attr(f, "morie_recode_audit") <- attr(out, "morie_recode_audit")
  f
}

#' Name the mechanical step that reproduces a label permutation
#'
#' When categories came out permuted and the transfer between two programs
#' is being blamed, the question is which deterministic step, applied to
#' the code book, yields exactly the observed permutation. This function
#' tries the known ones: labels sorted alphabetically (or reversed, or
#' case-insensitively) and assigned by code position, labels reversed,
#' every rotation, codes sorted as strings, and labels ordered by frequency
#' when counts are supplied. A match is a reconstruction, not a proof of
#' intent; but a transfer fault has no reason to select the sort order of
#' the labels, so a match on a sort-based mechanism exonerates the software.
#'
#' @param value_labels Named character vector, code = true label, in code
#'   order.
#' @param observed Named character vector, true label = label it was seen
#'   under.
#' @param counts Optional named numeric vector of frequencies per true
#'   label, enabling the frequency-order mechanism.
#' @return A data frame with one row per mechanism (`mechanism`,
#'   `permutation`, `matches`) and a `verdict` attribute.
#' @examples
#' vl <- c("1" = "White", "2" = "Black", "3" = "Other", "4" = "Unknown")
#' obs <- c(White = "Black", Black = "Other", Other = "Unknown", Unknown = "White")
#' attr(morie_relabel_forensics(vl, obs), "verdict")
#' @export
morie_relabel_forensics <- function(value_labels, observed, counts = NULL) {
  if (is.null(names(value_labels)) || is.null(names(observed))) {
    stop("morie_relabel_forensics: `value_labels` (code = label) and `observed` (true = seen) ",
         "must both be named", call. = FALSE)
  }
  L <- unname(value_labels)
  k <- length(L)
  if (!setequal(names(observed), L) || !setequal(unname(observed), L)) {
    stop("morie_relabel_forensics: `observed` must be a permutation of the labels in `value_labels`",
         call. = FALSE)
  }
  obs <- unname(observed[L])
  mech <- list(
    "labels sorted alphabetically, assigned by code position" = sort(L, method = "radix"),
    "labels sorted case-insensitively, assigned by code position" = L[order(tolower(L), method = "radix")],
    "labels sorted in reverse, assigned by code position" = rev(sort(L, method = "radix")),
    "labels reversed" = rev(L),
    "codes sorted as strings, labels assigned in that order" =
      L[order(as.character(names(value_labels)), method = "radix")])
  for (r in seq_len(k - 1L)) {
    mech[[sprintf("rotation by %d position(s)", r)]] <- L[((seq_len(k) - 1L + r) %% k) + 1L]
  }
  if (!is.null(counts)) {
    cnt <- counts[L]
    mech[["labels ordered by decreasing frequency, assigned by code position"]] <-
      L[order(-as.numeric(cnt), method = "radix")]
    mech[["labels ordered by increasing frequency, assigned by code position"]] <-
      L[order(as.numeric(cnt), method = "radix")]
  }
  rows <- lapply(names(mech), function(nm) {
    perm <- mech[[nm]]
    data.frame(mechanism = nm,
               permutation = paste(paste0(L, " -> ", perm), collapse = ", "),
               matches = identical(perm, obs), stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, rows)
  if (identical(obs, L)) {
    out$matches[] <- FALSE
    attr(out, "verdict") <- paste0(
      "The labels are in place: every label was seen under itself, so ",
      "there is no permutation to explain.")
    class(out) <- c("morie_relabel_forensics_result", "data.frame")
    return(out)
  }
  hit <- out$mechanism[out$matches]
  attr(out, "verdict") <- if (length(hit)) {
    paste0("The observed permutation is reproduced EXACTLY by: ",
           paste(hit, collapse = "; "), ". No import routine (haven, foreign, pandas, ",
           "pyreadstat) reorders value labels; each carries them keyed by code. A ",
           "transfer fault does not select the sort order of the labels. The step ",
           "that did this was a positional relabel in the analysis, and it is ",
           "reproducible from the code book alone.")
  } else {
    paste0("No positional or sort-based mechanism reproduces the observed permutation. ",
           "Look at merges/joins on the category column, manual edits, and the file ",
           "itself before blaming either program.")
  }
  class(out) <- c("bricklayer_morie_relabel_forensics", "data.frame")
  out
}

#' @export
print.bricklayer_morie_relabel_forensics <- function(x, ...) {
  m <- x[x$matches, , drop = FALSE]
  if (nrow(m)) {
    cat("Mechanism(s) reproducing the observed permutation:\n")
    for (i in seq_len(nrow(m))) cat("  * ", m$mechanism[i], "\n      ", m$permutation[i], "\n", sep = "")
  } else {
    cat("No mechanism among ", nrow(x), " reproduces the observed permutation.\n", sep = "")
  }
  cat("\n", attr(x, "verdict"), "\n", sep = "")
  invisible(x)
}

#' Verify a categorical variable that crossed from one program to another
#'
#' The transfer that matters is SPSS/Stata/SAS to R or Python: the source
#' program stores codes plus value labels, the destination is handed the
#' codes, and the labels are re-attached in the analysis. This function
#' takes what the source program printed for that variable (its frequency
#' table, label = count, and optionally its code book, code = label) and
#' refuses to continue unless the imported vector reproduces it exactly.
#' Rotated, swapped or positionally relabelled groups fail here, on the day
#' of the import, with the permutation named.
#'
#' @param imported The vector as it arrived: a haven-style labelled vector,
#'   plain codes (with `code_book`), or already-decoded labels.
#' @param source_counts Named numeric vector, label = count, as printed by
#'   the source program (SPSS FREQUENCIES, Stata tabulate).
#' @param code_book Optional named character vector, code = label, from the
#'   source program's variable view. When `imported` carries a `labels`
#'   attribute the two are compared and any disagreement is an error.
#' @param tolerance Passed to \code{\link{morie_marginals_verify}}.
#' @param strict When \code{TRUE} (the default) any disagreement is an error.
#'   When \code{FALSE} the result comes back with \code{ok = FALSE},
#'   \code{reasons}, and \code{marginals$permutation} ready for
#'   \code{\link{morie_relabel_forensics}}.
#' @return A list with `ok`, `decoded` (a factor in code order), `marginals`
#'   (the \code{\link{morie_marginals_verify}} result) and `code_book_ok`.
#' @seealso \code{\link{morie_decode_labelled}},
#'   \code{\link{morie_marginals_verify}},
#'   \code{\link{morie_relabel_forensics}}
#' @examples
#' x <- structure(c(1, 1, 2, 4, 1), labels = c(White = 1, Black = 2, Other = 3, Unknown = 4))
#' morie_transfer_verify(x, c(White = 3, Black = 1, Unknown = 1))$ok
#' @export
morie_transfer_verify <- function(imported, source_counts, code_book = NULL,
                                  tolerance = 0, strict = TRUE) {
  lab <- attr(imported, "labels", exact = TRUE)
  code_book_ok <- NA
  if (!is.null(lab) && !is.null(code_book)) {
    got <- stats::setNames(names(lab), as.character(unname(lab)))
    want <- stats::setNames(unname(code_book), as.character(names(code_book)))
    bad <- setdiff(union(names(got), names(want)),
                   names(got)[names(got) %in% names(want) & got == want[names(got)]])
    code_book_ok <- !length(bad)
    if (!code_book_ok && strict) {
      stop("morie_transfer_verify: the value labels that arrived disagree with the source code ",
           "book at code(s) ", paste(bad, collapse = ", "), ": arrived ",
           paste(paste0(bad, "=", got[bad]), collapse = ", "), "; source ",
           paste(paste0(bad, "=", want[bad]), collapse = ", "), call. = FALSE)
    }
  }
  decoded <- if (!is.null(lab)) {
    morie_decode_labelled(imported)
  } else if (!is.null(code_book)) {
    morie_decode_labelled(imported, value_labels = code_book)
  } else {
    # keep every label that arrived: one absent from the source counts must
    # be reported by the marginals check, never dropped as NA
    lab_all <- as.character(imported)
    factor(lab_all, levels = unique(c(names(source_counts), lab_all[!is.na(lab_all)])))
  }
  m <- morie_marginals_verify(as.character(decoded), source_counts,
                              tolerance = tolerance, strict = strict)
  ok <- isTRUE(m$ok) && !isFALSE(code_book_ok)
  reasons <- c(
    if (isFALSE(code_book_ok)) "value labels disagree with the source code book",
    if (!isTRUE(m$ok)) m$message
  )
  list(ok = ok, decoded = decoded, marginals = m, code_book_ok = code_book_ok,
       reasons = reasons)
}

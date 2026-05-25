# SPDX-License-Identifier: AGPL-3.0-or-later
#' Baseline-conditional gentrification coding (MRM primitive)
#'
#' Three-level categorical coding of tract-level gentrification,
#' mirroring the Python module \code{morie.mrm_primitives.gentrification}.
#' Adapted from Laniyonu (2018) Urban Affairs Review 54(5):898-930,
#' which itself adapts Chapple / Freeman / Maciag.
#'
#' The key insight: continuous gentrification indices conflate two
#' distinct populations -- already-affluent tracts (immune to
#' gentrification by construction) and marginalised tracts that DID or
#' DID NOT change. The cleanest comparator is the marginalised-but-
#' did-not-gentrify tract, so this primitive emits a 3-level factor:
#'
#' \itemize{
#'   \item \code{ineligible} -- tract was above the baseline-
#'     marginalisation cutoff (top-50\% on income AND rent) at t=0, so
#'     cannot meaningfully "gentrify". Drop from analyses that want
#'     the gentrification comparator.
#'   \item \code{eligible} -- tract was below the cutoff at t=0 AND
#'     did NOT cross the gentrification threshold by t=1. This is the
#'     control: marginalised, did-not-change.
#'   \item \code{gentrified} -- tract was below the cutoff at t=0 AND
#'     DID cross the gentrification threshold (top-tercile growth in
#'     college share AND top-tercile growth in median rent).
#' }
#'
#' @name mrm_gentrification
NULL


# ---------------------------------------------------------------------------
# Internal helper (shared wrapper for MRM primitives)
# ---------------------------------------------------------------------------

.mrm_result <- function(title, call, summary_lines = list(),
                        warnings = character(0),
                        interpretation = "",
                        ...) {
  out <- list(
    title = title,
    call = call,
    summary_lines = summary_lines,
    warnings = warnings,
    interpretation = interpretation,
    ...
  )
  class(out) <- c("morie_mrm_result", "morie_rich_result", "list")
  out
}


# ---------------------------------------------------------------------------
# mrm_gentrification_panel
# ---------------------------------------------------------------------------

#' Construct a baseline-conditional 3-level gentrification factor
#'
#' Implements the Laniyonu (2018) operationalisation:
#'
#' \enumerate{
#'   \item Tract is \emph{eligible} to gentrify iff baseline income
#'     AND baseline rent are at or below
#'     \code{baseline_marginalisation_quantile} of the panel.
#'   \item Among the eligible, the tract is \emph{gentrified} iff
#'     growth-in-college-share AND growth-in-rent are at or above
#'     \code{gentrification_growth_quantile}.
#'   \item Everything above the baseline cut is \emph{ineligible}.
#' }
#'
#' @param df A \code{data.frame} with one row per tract; must contain
#'   the four named columns.
#' @param baseline_income_col Character. Column carrying baseline
#'   (period t=0) income.
#' @param baseline_rent_col Character. Column carrying baseline rent.
#' @param growth_college_col Character. Column carrying college /
#'   BA-share growth between baseline and follow-up.
#' @param growth_rent_col Character. Column carrying median-rent growth
#'   between baseline and follow-up.
#' @param baseline_marginalisation_quantile Numeric in (0, 1); default
#'   0.5. Tract is eligible if baseline income AND rent are
#'   \eqn{\le}{<=} this quantile.
#' @param gentrification_growth_quantile Numeric in (0, 1); default
#'   0.667. Tract gentrifies if college growth AND rent growth are
#'   \eqn{\ge}{>=} this quantile.
#' @return A named list with classes \code{morie_mrm_result},
#'   \code{morie_rich_result}, \code{list}. Carries \code{labels}
#'   (character vector of length \code{nrow(df)}), \code{thresholds}
#'   (list of four cut-points), \code{counts} (table of label levels),
#'   plus \code{interpretation} + \code{warnings}.
#' @examples
#' set.seed(1)
#' df <- data.frame(
#'   inc0  = runif(50, 20000, 80000),
#'   rent0 = runif(50, 500, 2000),
#'   coll_g = rnorm(50),
#'   rent_g = rnorm(50)
#' )
#' res <- mrm_gentrification_panel(
#'   df,
#'   baseline_income_col = "inc0",
#'   baseline_rent_col   = "rent0",
#'   growth_college_col  = "coll_g",
#'   growth_rent_col     = "rent_g"
#' )
#' table(res$labels)
#' @export
mrm_gentrification_panel <- function(df,
                                     baseline_income_col,
                                     baseline_rent_col,
                                     growth_college_col,
                                     growth_rent_col,
                                     baseline_marginalisation_quantile = 0.5,
                                     gentrification_growth_quantile = 0.667) {
  stopifnot(is.data.frame(df),
            is.character(baseline_income_col),
            is.character(baseline_rent_col),
            is.character(growth_college_col),
            is.character(growth_rent_col))

  call_str <- sprintf(
    "mrm_gentrification_panel(df=<%dr>, baseline_q=%.3f, growth_q=%.3f)",
    nrow(df), baseline_marginalisation_quantile, gentrification_growth_quantile
  )

  required <- c(baseline_income_col, baseline_rent_col,
                growth_college_col, growth_rent_col)
  missing <- setdiff(required, names(df))
  if (length(missing) > 0L) {
    return(.mrm_result(
      "MRM Gentrification Panel",
      call_str,
      warnings = sprintf("Required column(s) missing: %s",
                         paste(missing, collapse = ", ")),
      interpretation = sprintf(
        "No analysis: required column(s) %s absent from the supplied dataframe.",
        paste(missing, collapse = ", ")
      ),
      n = 0L
    ))
  }

  if (!is.numeric(baseline_marginalisation_quantile) ||
      baseline_marginalisation_quantile <= 0 ||
      baseline_marginalisation_quantile >= 1) {
    stop("baseline_marginalisation_quantile must be in (0, 1).")
  }
  if (!is.numeric(gentrification_growth_quantile) ||
      gentrification_growth_quantile <= 0 ||
      gentrification_growth_quantile >= 1) {
    stop("gentrification_growth_quantile must be in (0, 1).")
  }

  inc       <- as.numeric(df[[baseline_income_col]])
  rent      <- as.numeric(df[[baseline_rent_col]])
  coll_g    <- as.numeric(df[[growth_college_col]])
  rent_g    <- as.numeric(df[[growth_rent_col]])

  warnings <- character(0)

  inc_q       <- stats::quantile(inc,    probs = baseline_marginalisation_quantile,
                                 na.rm = TRUE, names = FALSE, type = 7)
  rent_q      <- stats::quantile(rent,   probs = baseline_marginalisation_quantile,
                                 na.rm = TRUE, names = FALSE, type = 7)
  coll_q      <- stats::quantile(coll_g, probs = gentrification_growth_quantile,
                                 na.rm = TRUE, names = FALSE, type = 7)
  growth_rent_q <- stats::quantile(rent_g, probs = gentrification_growth_quantile,
                                   na.rm = TRUE, names = FALSE, type = 7)

  n_total <- nrow(df)
  n_na <- sum(is.na(inc) | is.na(rent) | is.na(coll_g) | is.na(rent_g))
  if (n_na > 0L) {
    warnings <- c(warnings, sprintf(
      "%d row(s) carry NA in one or more conditioning columns and will be coded NA.",
      n_na
    ))
  }

  eligible_mask <- (inc <= inc_q) & (rent <= rent_q)
  growth_mask   <- (coll_g >= coll_q) & (rent_g >= growth_rent_q)

  # Match Python: ~eligible_mask -> "ineligible";
  # else if growth_mask -> "gentrified" else "eligible".
  labels <- ifelse(
    is.na(eligible_mask),
    NA_character_,
    ifelse(!eligible_mask,
           "ineligible",
           ifelse(is.na(growth_mask),
                  NA_character_,
                  ifelse(growth_mask, "gentrified", "eligible")))
  )
  names(labels) <- rownames(df)

  thresholds <- list(
    baseline_income_cut = as.numeric(inc_q),
    baseline_rent_cut   = as.numeric(rent_q),
    growth_college_cut  = as.numeric(coll_q),
    growth_rent_cut     = as.numeric(growth_rent_q)
  )

  lvl <- c("ineligible", "eligible", "gentrified")
  counts <- table(factor(labels, levels = lvl), useNA = "no")
  n_codeable <- sum(counts)
  share_gentrified <- if (n_codeable > 0L) {
    as.numeric(counts["gentrified"]) / n_codeable
  } else NA_real_
  share_eligible <- if (n_codeable > 0L) {
    as.numeric(counts["eligible"]) / n_codeable
  } else NA_real_

  if (n_codeable < 30L) {
    warnings <- c(warnings, sprintf(
      "Only %d codeable tract(s); the 3-level panel is descriptive at best below n=30.",
      n_codeable
    ))
  }
  if (as.numeric(counts["eligible"]) == 0L) {
    warnings <- c(warnings,
      "Zero tracts coded 'eligible' (marginalised did-not-change). The Laniyonu comparator group is empty; revisit the baseline-marginalisation quantile.")
  }
  if (as.numeric(counts["gentrified"]) == 0L) {
    warnings <- c(warnings,
      "Zero tracts coded 'gentrified'. Revisit the growth quantile or check that the growth columns carry change (not levels).")
  }

  gentr_txt <- if (is.na(share_gentrified)) {
    "Gentrification share is undefined."
  } else {
    sprintf(
      "Of the %d codeable tract(s), %s are coded 'gentrified' and %s are the marginalised did-not-change comparator ('eligible').",
      n_codeable,
      sprintf("%.2f%%", 100 * share_gentrified),
      sprintf("%.2f%%", 100 * share_eligible)
    )
  }
  cut_txt <- sprintf(
    "Cut-points: baseline income <= %.4g, baseline rent <= %.4g, growth-college >= %.4g, growth-rent >= %.4g.",
    thresholds$baseline_income_cut, thresholds$baseline_rent_cut,
    thresholds$growth_college_cut, thresholds$growth_rent_cut
  )
  rec_txt <- "Drop 'ineligible' tracts before fitting the gentrification effect on outcomes; compare 'gentrified' to 'eligible' (the marginalised did-not-change control) to avoid conflating affluence with change."

  interp <- paste(gentr_txt, cut_txt, rec_txt, sep = " ")

  .mrm_result(
    "MRM Gentrification Panel",
    call_str,
    summary_lines = list(
      `Tracts (n)`            = n_total,
      `Codeable tracts`       = as.integer(n_codeable),
      `Ineligible`            = as.integer(counts["ineligible"]),
      `Eligible (control)`    = as.integer(counts["eligible"]),
      `Gentrified`            = as.integer(counts["gentrified"]),
      `Baseline-marg. quantile` = baseline_marginalisation_quantile,
      `Growth quantile`        = gentrification_growth_quantile
    ),
    warnings = warnings,
    interpretation = interp,
    n = n_total,
    n_codeable = as.integer(n_codeable),
    labels = labels,
    counts = as.list(counts),
    thresholds = thresholds,
    share_gentrified = share_gentrified,
    share_eligible = share_eligible,
    baseline_marginalisation_quantile = baseline_marginalisation_quantile,
    gentrification_growth_quantile = gentrification_growth_quantile,
    value = share_gentrified
  )
}


# ---------------------------------------------------------------------------
# Print method (shared across MRM primitives)
# ---------------------------------------------------------------------------

#' @export
print.morie_mrm_result <- function(x, ...) {
  cat(x$title, "\
", strrep("=", nchar(x$title)), "\
", sep = "")
  if (!is.null(x$call) && nzchar(x$call)) {
    cat("Call:", x$call, "\
\
", sep = " ")
  }
  if (length(x$summary_lines) > 0L) {
    nms <- names(x$summary_lines)
    label_w <- max(nchar(nms))
    for (i in seq_along(x$summary_lines)) {
      v <- x$summary_lines[[i]]
      if (is.numeric(v) && length(v) == 1L && is.finite(v)) {
        v <- format(v, digits = 5)
      }
      cat(sprintf("  %-*s  %s\
", label_w, nms[i], format(v)))
    }
    cat("\
")
  }
  if (length(x$warnings) > 0L) {
    for (w in x$warnings) cat("Warning:", w, "\
")
    cat("\
")
  }
  if (nzchar(x$interpretation)) {
    cat(x$interpretation, "\
")
  }
  invisible(x)
}

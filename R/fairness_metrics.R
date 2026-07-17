# SPDX-License-Identifier: AGPL-3.0-or-later
#' Group-disparity metrics for auditing classification systems
#'
#' R port of \code{morie.fairness.metrics}. Each callable is an
#' *audit* measure: given decisions a system made (and, where
#' available, the realised ground truth) plus a protected attribute,
#' it quantifies whether outcomes differ across groups. None of these
#' functions make predictions; they only measure disparity in
#' predictions that already exist.
#'
#' Functions
#' ---------
#' \itemize{
#'   \item \code{\link{morie_fairness_disparate_impact}}: the four-fifths
#'     rule.
#'   \item \code{\link{morie_fairness_demographic_parity}}:
#'     favourable-rate gap.
#'   \item \code{\link{morie_fairness_equalized_odds}}: TPR/FPR gaps
#'     (needs ground truth).
#'   \item \code{\link{morie_fairness_average_odds_difference}}: mean
#'     TPR+FPR gap.
#'   \item \code{\link{morie_fairness_gini}}: concentration of a score
#'     distribution.
#'   \item \code{\link{morie_fairness_bias_amplification}}: composite
#'     of parity gap and inequality.
#' }
#'
#' Prior art: the COMPAS
#' fairness audit in pbiecek's \emph{XAI Stories} and IBM's AI Fairness
#' 360 definitions; the predictive-policing disparity framing of the
#' SciencesPo \emph{Predictive-policing-Chicago} project (Lacherade,
#' Szabo, Krikava & Aeby, 2021) and Barman & Barman, arXiv:2603.18987.
#'
#' @name fairness_metrics
NULL


.MORIE_FAIRNESS_FOUR_FIFTHS <- 0.8  # EEOC four-fifths threshold


# ---------------------------------------------------------------------------
# Internal helpers
# ---------------------------------------------------------------------------

.morie_fairness_result <- function(title, summary_lines = list(),
                                   tables = list(), sections = list(),
                                   warnings = character(0),
                                   interpretation = "",
                                   payload = list()) {
  # Splice the payload fields up to the top level so the flat contract
  # (res$value, res$per_group, res$adverse_impact, ...) that callers and
  # tests rely on lives alongside the rich sections/tables view.
  out <- c(
    list(
      title = title,
      summary_lines = summary_lines,
      tables = tables,
      sections = sections,
      warnings = warnings,
      interpretation = interpretation,
      payload = payload
    ),
    payload
  )
  class(out) <- c("morie_fairness_result", "morie_rich_result", "list")
  out
}




# ---------------------------------------------------------------------------
# print
# ---------------------------------------------------------------------------

#' @return Invisibly returns \code{x} unchanged.
#' @examples
#' \donttest{
#' d <- morie_fairness_simulate_biased_crime_data(n = 100L, seed = 1L)
#' head(d)
#' print(d)
#' }
#' @export
print.morie_fairness_result <- function(x, ...) {
  cat(x$title, "\
", strrep("=", nchar(x$title)), "\
", sep = "")
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
  if (nzchar(x$interpretation)) cat(x$interpretation, "\
")
  invisible(x)
}

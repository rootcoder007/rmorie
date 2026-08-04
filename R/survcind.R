# SPDX-License-Identifier: AGPL-3.0-or-later
#' Harrell concordance index for censored survival data
#'
#' c = (concordant + 0.5 tied) / comparable over subject pairs whose earlier
#' time is an observed event.  Source consulted: Harrell, Califf, Pryor, Lee
#' and Rosati (1982), Evaluating the yield of medical tests, JAMA 247(18),
#' 2543-2546.
#'
#' @param time numeric observed follow-up times.
#' @param event 1 for an observed event, 0 for right censoring.
#' @param predicted_risk numeric risk score, higher meaning shorter survival.
#' @return list: statistic, estimate, concordant, discordant, tied,
#'   comparable, n, method.
#' @keywords internal
#' @examples
#' survcind(c(1, 2, 3), c(1, 1, 1), c(3, 2, 1))
#' @export
survcind <- function(time, event, predicted_risk) {
  t <- as.numeric(time); e <- as.numeric(event); r <- as.numeric(predicted_risk)
  n <- min(length(t), length(e), length(r))
  conc <- 0; disc <- 0; tied <- 0; comp <- 0
  if (n > 1) for (i in seq_len(n - 1)) for (j in (i + 1):n) {
    if (t[i] != t[j]) {
      lo <- if (t[i] < t[j]) i else j
      hi <- if (t[i] < t[j]) j else i
      if (e[lo] == 1) {
        comp <- comp + 1
        if (r[lo] > r[hi]) conc <- conc + 1
        else if (r[lo] < r[hi]) disc <- disc + 1
        else tied <- tied + 1
      }
    }
  }
  cc <- if (comp > 0) (conc + 0.5 * tied) / comp else NA_real_
  list(statistic = as.numeric(cc), estimate = as.numeric(cc),
       concordant = as.numeric(conc), discordant = as.numeric(disc),
       tied = as.numeric(tied), comparable = as.numeric(comp),
       n = as.integer(n),
       method = "Harrell concordance index (Harrell et al. 1982)")
}

# CANONICAL TEST
# r <- survcind(c(1, 2, 3), c(1, 1, 1), c(3, 2, 1))
# stopifnot(abs(r$statistic - 1) < 1e-12)

# NOTE: no morie_survival_concordance alias is defined here.  That name is
# already exported by R/survival.R, where it points at morie_concordance_index
# -- an independent c-index implementation that additionally counts tied-time
# pairs in which both subjects had events.  Defining it twice would clash in
# NAMESPACE.  The duplication is reported upstream rather than resolved here,
# since R/survival.R is outside this batch.

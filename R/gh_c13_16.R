# SPDX-License-Identifier: AGPL-3.0-or-later
#' Bayesian bootstrap for censored data
#'
#' Letting alpha tend to zero in the censored-data DP posterior gives
#' Lo's censored Bayesian bootstrap, and its product-limit factors reduce
#' to the Kaplan-Meier factors (R_j - d_j) / R_j exactly.  The
#' vanishing-prior limit of the Bayesian procedure IS the classical
#' estimator, which is the cleanest statement of why Kaplan-Meier needs
#' no Bayesian apology.
#'
#' Formula: S(t) = prod over events t_j <= t of (R_j - 1) / R_j.
#'
#' @param times Observed follow-up times.
#' @param events Event indicators, 1 for an event and 0 for censoring.
#' @param t_query Time at which the survival is reported.
#' @return List with \code{estimate}, \code{km_survival},
#'   \code{method}.
#' @references Ghosal & van der Vaart (2017), Fundamentals of
#'   Nonparametric Bayesian Inference, CUP, section 13.7.1.
#' @export
Ghosalbbcensored <- function(times, events, t_query) {
  ts <- as.numeric(times); ev <- as.numeric(events)
  if (length(ts) == 0L) stop("times must be non-empty")
  if (length(ev) != length(ts))
    stop("times and events must have the same length")
  ord <- order(ts)
  surv <- 1
  at_risk <- length(ts)
  for (i in ord) {
    if (ts[i] > t_query) break
    if (ev[i] > 0) surv <- surv * (at_risk - 1) / at_risk
    at_risk <- at_risk - 1
  }
  .t1_result(estimate = surv, km_survival = surv,
             method = "censored Bayesian bootstrap = Kaplan-Meier limit (GvdV 2017 sec. 13.7.1)")
}

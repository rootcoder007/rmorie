# SPDX-License-Identifier: AGPL-3.0-or-later
#' Dirichlet-process posterior with right-censored data
#'
#' Right censoring destroys the conjugacy that makes the DP posterior a
#' DP: what survives is a product-limit form in which each risk set
#' contributes one Beta-posterior factor.  That product is the Bayesian
#' analogue of Kaplan-Meier, and the prior enters only through the
#' prior-mean survival S0 attached to each factor.
#'
#' Formula: S(t) = prod over events t_j <= t of
#'   (alpha S0(t_j) + R_j - 1) / (alpha S0(t_j) + R_j),
#'   with S0 the unit-exponential survival and R_j the risk set size.
#'
#' @param times Observed follow-up times.
#' @param events Event indicators, 1 for an event and 0 for censoring.
#' @param t_query Time at which the survival is reported.
#' @param alpha DP concentration, positive.
#' @return List with \code{estimate}, \code{survival_at_t},
#'   \code{method}.
#' @references Ghosal & van der Vaart (2017), Fundamentals of
#'   Nonparametric Bayesian Inference, CUP, section 13.2.
#' @export
Ghosalsurvdppost <- function(times, events, t_query, alpha = 2) {
  ts <- as.numeric(times)
  ev <- as.numeric(events)
  if (length(ts) == 0L) stop("times must be non-empty")
  if (length(ev) != length(ts))
    stop("times and events must have the same length")
  if (alpha <= 0) stop("alpha must be positive")
  ord <- order(ts)
  surv <- 1
  at_risk <- length(ts)
  for (i in ord) {
    if (ts[i] > t_query) break
    S0 <- exp(-ts[i])
    if (ev[i] > 0)
      surv <- surv * (alpha * S0 + at_risk - 1) / (alpha * S0 + at_risk)
    at_risk <- at_risk - 1
  }
  .t1_result(estimate = surv, survival_at_t = surv,
             method = "censored DP posterior (GvdV 2017 sec. 13.2)")
}

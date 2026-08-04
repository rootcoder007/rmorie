# SPDX-License-Identifier: AGPL-3.0-or-later
#' Bayesian online changepoint detection (re-export).
#'
#' Formula: see bocpd
#'
#' @param y Observed univariate series.
#' @param hazard Constant hazard of the geometric run-length prior.
#' @param mu0 Prior mean.
#' @param kappa0 Prior mean precision.
#' @param alpha0 Prior shape.
#' @param beta0 Prior scale.

#' @return List with the payload of :func:`morie.fn.bocpd.bocpd`.
#' @references Adams and MacKay (2007), Bayesian Online Changepoint Detection, arXiv:0710.3742. Equations (2)-(5) for the recursion and the changepoint prior, Section 2.3 and Algorithm 1 for the conjugate-exponential update of the run-specific sufficient statistics. Verified against the paper.
#' @export
Bayesocp <- function(y, hazard = 0.004, mu0 = 0, kappa0 = 1, alpha0 = 1, beta0 = 1) {
  Bocpd(y, hazard = hazard, mu0 = mu0, kappa0 = kappa0,
        alpha0 = alpha0, beta0 = beta0)
}

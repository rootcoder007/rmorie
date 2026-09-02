# SPDX-License-Identifier: AGPL-3.0-or-later
#' Propensity-only, outcome-only and doubly robust DiD side by side
#'
#' Sant'Anna and Zhao (2020), Journal of Econometrics 219(1), 101-122
#' (arXiv:1812.01723 -- FETCHED).  The paper's whole argument is a
#' comparison of weighting strategies, so all three are computed: the
#' outcome regression of Heckman, Ichimura and Todd (1997), Review of
#' Economic Studies 64(4), 605-654, tau = E[(D/E\[D\])(dY - mu_0(X))]; the
#' inverse propensity estimator of Abadie (2005), Review of Economic
#' Studies 72(1), 1-19, tau = E[(D - pi(X)) dY / (E[D](1 - pi(X)))]; and
#' the DR estimator, eq. (2.6).  The first is consistent only if mu_0 is
#' right, the second only if pi is, the third if either is -- so a large
#' gap between the first two is the diagnostic the paper is built around.
#' The 1997 and 2005 papers are paywalled; both estimands are restated in
#' section 2 of Sant'Anna and Zhao, which was fetched.
#'
#' @param y outcome change, or period-1 outcome when y0 is given.
#' @param D treatment indicator.
#' @param X covariates.
#' @param y0 period-0 outcome.
#' @return list: estimate, tau_reg, tau_ipw, tau_dr, spread, se, n, method.
#' @keywords internal
#' @examples
#' Drweights(c(1, 2, 0, 3), c(1, 0, 1, 0))$spread
#' @export
Drweights <- function(y, D, X = NULL, y0 = NULL) {
  dy <- .s03vec(y)
  if (!is.null(y0)) dy <- dy - .s03vec(y0)
  d <- .s03vec(D); n <- length(dy)
  fit <- .s03drdid(dy, d, X)
  pi_ <- fit$pi; mu0 <- fit$mu0
  ed <- 0
  for (x in d) ed <- ed + x / n
  treg <- 0
  for (i in seq_len(n)) treg <- treg + (if (ed > 0) (d[i] / (n * ed)) * (dy[i] - mu0[i]) else 0)
  tipw <- 0
  for (i in seq_len(n)) {
    if (ed > 0) tipw <- tipw + ((d[i] - pi_[i]) / (ed * (1 - pi_[i]))) * dy[i] / n
  }
  tdr <- fit$tau
  vals <- c(treg, tipw, tdr)
  list(estimate = tdr, tau_reg = treg, tau_ipw = tipw, tau_dr = tdr,
       spread = max(vals) - min(vals), se = fit$se, n = n,
       method = "Outcome-regression (Heckman et al. 1997), IPW (Abadie 2005) and DR (Sant'Anna and Zhao 2020) ATT")
}

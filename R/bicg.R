# SPDX-License-Identifier: AGPL-3.0-or-later
#' Schwarz's Bayesian information criterion
#'
#' Schwarz, G. (1978), Estimating the dimension of a model, The Annals of
#' Statistics 6(2), 461-464, derives the criterion as the leading terms of
#' the Laplace approximation to the log marginal likelihood.  Written as a
#' deviance to be minimised it is BIC = -2 log L + p log n.  The 1978
#' Annals paper is paywalled, so the equation is quoted in its standard
#' published form, which is unambiguous.
#'
#' @param log_lik maximised log-likelihood.
#' @param n_params number of freely estimated parameters, p.
#' @param n_obs number of observations, n.
#' @return list: estimate (BIC), aic, penalty, log_lik, n_params, n, method.
#' @keywords internal
#' @examples
#' Bic(-120.5, 4, 100)$estimate
#' @export
Bic <- function(log_lik, n_params, n_obs) {
  ll <- as.numeric(log_lik)
  p <- as.numeric(n_params)
  n <- as.numeric(n_obs)
  penalty <- if (n > 0) p * log(n) else NaN
  bic <- -2 * ll + penalty
  aic <- -2 * ll + 2 * p
  list(estimate = bic, aic = aic, penalty = penalty, log_lik = ll,
       n_params = p, n = n,
       method = "Schwarz (1978) Bayesian information criterion")
}

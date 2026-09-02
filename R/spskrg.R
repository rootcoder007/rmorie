# SPDX-License-Identifier: AGPL-3.0-or-later
#' Simple kriging: the mean is known
#'
#' With Z(s) = mu(s) + e(s), e ~ (0, Sigma), the linear predictor
#' minimising the mean-squared prediction error is
#' p(Z; s0) = mu(s0) + sigma' Sigma^-1 (Z(s) - mu(s)), with kriging
#' variance sigma^2 - sigma' Sigma^-1 sigma.
#'
#' Simple kriging is an EXACT interpolator: predicting at an observed
#' location returns that observation with zero variance, because sigma
#' becomes a column of Sigma. The book calls this honouring the data.
#'
#' @param coords Observation coordinates (n by d).
#' @param z Observed values, length n.
#' @param target Prediction locations (m by d).
#' @param cov_model List with `model`, `nugget`, `sill`, `range`.
#' @param mu Known mean. Defaults to the sample mean of `z`, which makes
#'   this simple kriging with an estimated rather than known mean.
#' @return Named list: prediction, variance, weights, mu.
#' @references Schabenberger & Gotway (2005), Sec 5.2.1, eqs (5.10)-(5.11),
#'   pp. 223-224.
#' @examples
#' co <- matrix(runif(40), 20, 2) * 5
#' spskrg(co, rnorm(20), matrix(c(2, 2), 1, 2),
#'        list(model = "exponential", sill = 1, range = 2))
#' @export
spskrg <- function(coords, z, target, cov_model = NULL, mu = NULL) {
  .sp_simple_kriging(coords, z, target, cov_model, mu)
}

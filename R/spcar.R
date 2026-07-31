# SPDX-License-Identifier: AGPL-3.0-or-later
#' Conditional autoregressive model: the conditional specification.
#'
#' Rather than specifying one multivariate model, the CAR approach models
#' each conditional distribution f(Z(s_i) | Z(s_j), s_j in N_i):
#'
#'   E[Z(s_i) | Z(s)_-i] = x(s_i)' beta + sum_j c_ij (Z(s_j) - x(s_i)' beta)
#'   Var[Z(s_i) | Z(s)_-i] = sigma_i^2
#'
#' The Hammersley-Clifford theorem gives the conditions under which those
#' conditionals define a valid joint distribution; in the Gaussian case
#' they do, with Sigma_CAR = (I - C)^-1 Sigma_c.
#'
#' Same estimator as [sgcar()]; this delegates rather than carrying a
#' second implementation.
#'
#' @param z Response, length n.
#' @param w Adjacency weights (n by n).
#' @param covariates Covariates (n by p); an intercept when NULL.
#' @return The result of `sgcar()`.
#' @references Schabenberger & Gotway (2005), Sec 6.2.2.2, eqs
#'   (6.43)-(6.45), pp. 338-339.
#' @examples
#' n <- 20
#' W <- matrix(0, n, n); W[cbind(1:(n - 1), 2:n)] <- 1; W <- W + t(W)
#' spcar(rnorm(n), W)
#' @export
spcar <- function(z, w, covariates = NULL) {
  sgcar(z, w, covariates)
}

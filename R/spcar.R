# SPDX-License-Identifier: AGPL-3.0-or-later
#' Conditional autoregressive model: the conditional specification
#'
#' Rather than one multivariate model, the CAR approach models each
#' conditional distribution f(Z(s_i) | Z(s_j), s_j in N_i). By
#' Hammersley-Clifford these generate a valid joint Gaussian with
#' Sigma_CAR = (I - C)^-1 Sigma_c.
#'
#' Same estimator as [sgcar()]; this delegates rather than carrying a
#' second implementation.
#'
#' @param z Response, length n.
#' @param w Symmetric adjacency weights (n by n).
#' @param covariates Covariates (n by p); an intercept when NULL.
#' @param parameterization "weighted" (conditional variance sigma^2/d_i)
#'   or "identity" (constant). These are different models; forwarded to
#'   [sgcar()].
#' @return The result of `sgcar()`.
#' @references Schabenberger & Gotway (2005), Sec 6.2.2.2, eqs
#'   (6.43)-(6.45), pp. 338-339.
#' @examples
#' n <- 20
#' W <- matrix(0, n, n); W[cbind(1:(n - 1), 2:n)] <- 1; W <- W + t(W)
#' spcar(sin(seq_len(n) * 0.7), W)
#' @export
spcar <- function(z, w, covariates = NULL, parameterization = "weighted") {
  sgcar(z, w, covariates, parameterization)
}

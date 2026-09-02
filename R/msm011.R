# SPDX-License-Identifier: AGPL-3.0-or-later
#' Marginal likelihood of the linear mixed model
#'
#' Formula: L(beta, D, R; y) = |V|^(-1/2)(2 pi)^(-n/2)
#' exp(-1/2 (y - X beta)' V^-1 (y - X beta)) with V = Z D Z' + R (eq. 5.2);
#' the log is returned. With restricted = TRUE the REML objective of p.146 is
#' returned instead, adding the -1/2 log|X'V^-1X| term that removes the ML bias.
#'
#' @param X Fixed-effects design matrix.
#' @param Z Random-effects design matrix.
#' @param y Response vector.
#' @param D Random-effects covariance matrix.
#' @param R Residual covariance matrix; identity when NULL.
#' @param beta Fixed effects; GLS estimate plugged in when NULL.
#' @param restricted If TRUE return the REML objective.
#' @return List with estimate, loglik, beta, restricted, method.
#' @references Montesinos Lopez, Montesinos Lopez & Crossa (2022),
#'   Multivariate Statistical Machine Learning Methods for Genomic Prediction,
#'   Springer, eq. (5.2) p.142 and sec. 5.2.1.2 p.146.
#'   DOI 10.1007/978-3-030-89010-0.
#' @export
#' @examples
#' Msm011(X = 5L, Z = 5L, y = c(1, 2, 3, 4, 5, 6, 7, 8), D = 5L)
Msm011 <- function(X, Z, y, D, R = NULL, beta = NULL, restricted = FALSE) {
  f <- if (restricted) .gpremlloglik(X, Z, y, D, R)
       else .gplmmloglik(X, Z, y, D, beta = beta, R = R)
  list(estimate = f$value, loglik = f$value, beta = f$beta,
       restricted = restricted,
       method = "LMM marginal likelihood (MVSML 2022 eq. 5.2)")
}

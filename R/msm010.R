# SPDX-License-Identifier: AGPL-3.0-or-later
#' General univariate linear mixed model.
#'
#' Formula: Y = X beta + Z b + eps (eq. 5.1) with b ~ N(0, D), eps ~ N(0, R)
#' and Cov(eps, b) = 0, so E(Y) = X beta and Var(Y) = Z D Z' + R. Returns the
#' GLS/BLUE of beta, the BLUP of b and the marginal variance.
#'
#' @param X Fixed-effects design matrix.
#' @param Z Random-effects design matrix.
#' @param y Response vector.
#' @param D Random-effects covariance matrix.
#' @param R Residual covariance matrix; identity when NULL.
#' @return List with estimate, beta, blup, V, loglik, method.
#' @references Montesinos Lopez, Montesinos Lopez & Crossa (2022),
#'   Multivariate Statistical Machine Learning Methods for Genomic Prediction,
#'   Springer, eq. (5.1) p.141. DOI 10.1007/978-3-030-89010-0.
#' @export
Msm010 <- function(X, Z, y, D, R = NULL) {
  bb <- .gpblueblup(X, Z, y, D, R)
  V <- .gplmmV(Z, D, R)
  ll <- .gplmmloglik(X, Z, y, D, beta = bb$beta, R = R)
  list(estimate = bb$beta[1L], beta = bb$beta, blup = bb$u, V = V,
       loglik = ll$value,
       method = "general linear mixed model (MVSML 2022 eq. 5.1)")
}

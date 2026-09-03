# SPDX-License-Identifier: AGPL-3.0-or-later
#' Henderson's mixed model equations for the BLUE and the BLUP
#'
#' Formula: [\[X'R^-1 X, X'R^-1 Z\], \[Z'R^-1 X, Z'R^-1 Z + Sigma^-1\]] \[beta; u\] =
#' \[X'R^-1 y; Z'R^-1 y\]
#'
#' @param X Design matrix of fixed effects.
#' @param Z Design matrix of random effects.
#' @param y Response vector of length n.
#' @param Sigma_inv Inverse of the random-effect variance-covariance matrix.
#' @param R_inv Inverse residual variance-covariance matrix; None uses the identity.
#'
#' @return List with ``beta``, ``u``, ``fitted``, ``n``, ``p``, ``q``.
#' @references Montesinos Lopez, Montesinos Lopez and Crossa (2022), Multivariate
#' Statistical Machine Learning Methods for Genomic Prediction, Springer,
#' doi:10.1007/978-3-030-89010-0.  Chapter 2, Eq. (2.2) p. 37: Henderson's mixed model
#' equations, whose solution for beta is the BLUE and for u is the BLUP.  Delegates to
#' the chapter-2 MME solver already verified against the book for this shelf.  Read from
#' the chapter PDF, not recalled.
#' @export
#' @examples
#' set.seed(1)
#' r <- Hendmme(X = rnorm(10), Z = rnorm(10), y = rnorm(10), Sigma_inv = 0.5); TRUE
Hendmme <- function(X, Z, y, Sigma_inv, R_inv = NULL) {
  out <- morie_mme(X, Z, y, Sigma_inv, R_inv = R_inv)
  b <- out$blue
  uu <- out$blup
  Xm <- .t1_mat(X)
  Zm <- .t1_mat(Z)
  .t1_result(beta = b, u = uu,
             fitted = as.numeric(Xm %*% b) + as.numeric(Zm %*% uu),
             n = nrow(Xm), p = ncol(Xm), q = ncol(Zm),
             method = "Henderson mixed model equations, MVSML Eq. (2.2)")
}

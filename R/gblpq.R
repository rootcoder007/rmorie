# SPDX-License-Identifier: AGPL-3.0-or-later
#' Cholesky re-parameterization that makes GBLUP an ordinary mixed model
#'
#' Formula: G = L L';  Zstar = Z L;  then Y = X beta + Zstar ustar + e with ustar ~ N(0, sigma2_g I) has the same marginal variance as u ~ N(0, sigma2_g G)
#'
#' @param Z Design matrix of lines.
#' @param G Genomic relationship matrix; must be positive definite.
#' @param sigma2_g Genomic variance component.
#'
#' @return List with ``Zstar``, ``L``, ``V_original``, ``V_reparameterized``, ``max_gap``, ``n``, ``q``.
#' @references Montesinos Lopez, Montesinos Lopez and Crossa (2022), Multivariate Statistical Machine Learning Methods for Genomic Prediction, Springer, doi:10.1007/978-3-030-89010-0.  Chapter 2, p. 46: the GBLUP model can be expressed equivalently as Y = X beta + Z* u + e with Z* = Z L', G = L'L the Cholesky decomposition of G, and u ~ N(0, sigma2_g I_q).  The book's L is the upper factor returned by R's chol(); this implementation uses the lower factor L with G = L L' and Z* = Z L in BOTH language arms, which gives the identical marginal variance sigma2_g Z G Z' -- ``max_gap`` reports the largest entry-wise difference between the two marginal variances, and is zero up to rounding.  Read from the chapter PDF, not recalled.
#' @export
#' @examples
#' set.seed(1)
#' Z <- matrix(rbinom(50, 1, 0.5), 10, 5)
#' G <- diag(5)
#' Gblupeq(Z, G, sigma2_g = 1)
Gblupeq <- function(Z, G, sigma2_g) {
  Zm <- .t1_mat(Z); Gm <- .t1_mat(G); s2 <- as.numeric(sigma2_g)
  n <- nrow(Zm); q <- ncol(Zm)
  if (nrow(Gm) != q || ncol(Gm) != q)
    stop("G must be q by q with q the number of columns of Z")
  if (s2 < 0) stop("sigma2_g must be non-negative")
  L <- t(chol(Gm))
  Zs <- Zm %*% L
  V0 <- s2 * (Zm %*% Gm %*% t(Zm))
  V1 <- s2 * (Zs %*% t(Zs))
  .t1_result(Zstar = Zs, L = L, V_original = V0, V_reparameterized = V1,
             max_gap = max(abs(V0 - V1)), n = n, q = q,
             method = "GBLUP Cholesky re-parameterization, MVSML Chap. 2 p. 46")
}

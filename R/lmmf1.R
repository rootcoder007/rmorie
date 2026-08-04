# SPDX-License-Identifier: AGPL-3.0-or-later
#' Linear mixed model in the form Y = X beta + Z u + e, with its two means and marginal variance.
#'
#' Formula: Y = X beta + Z u + e,  u ~ N(0, Sigma), e ~ N(0, R);  E(Y) = X beta, E(Y|u) = X beta + Z u, Var(Y) = Z Sigma Z' + R
#'
#' @param X Design matrix of fixed effects.
#' @param beta Fixed-effect coefficients, length p.
#' @param Z Design matrix of random effects.
#' @param u Realized random effects, length q.
#' @param Sigma Variance-covariance matrix of the random effects.
#' @param R Residual variance-covariance matrix; None uses the identity.
#'
#' @return List with ``mean_marginal``, ``mean_conditional``, ``signal``, ``V``, ``n``, ``p``, ``q``.
#' @references Montesinos Lopez, Montesinos Lopez and Crossa (2022), Multivariate Statistical Machine Learning Methods for Genomic Prediction, Springer, doi:10.1007/978-3-030-89010-0.  Chapter 2, Eq. (2.1) p. 37 and the paragraph beneath it: Y is n x 1, X is n x p, Z is n x q, u ~ N(0, Sigma) with Sigma of order q x q and e ~ N(0, R) with R of order n x n; the unconditional mean is E(Y) = X beta and the conditional mean given the random effects is E(Y|u) = X beta + Z u.  The marginal variance Z Sigma Z' + R is the V that Sect. 2.2 uses for the BLUE and BLUP.  Read from the chapter PDF, not recalled.
#' @export
Lmmform <- function(X, beta, Z, u, Sigma, R = NULL) {
  Xm <- .t1_mat(X); Zm <- .t1_mat(Z)
  b <- .t1_vec(beta); uu <- .t1_vec(u)
  n <- nrow(Xm); p <- ncol(Xm); q <- ncol(Zm)
  if (nrow(Zm) != n) stop("X and Z must have the same number of rows")
  if (length(b) != p || length(uu) != q)
    stop("beta and u must match the columns of X and Z")
  S <- .t1_mat(Sigma)
  if (nrow(S) != q || ncol(S) != q) stop("Sigma must be q by q")
  Rm <- if (is.null(R)) diag(n) else .t1_mat(R)
  if (nrow(Rm) != n || ncol(Rm) != n) stop("R must be n by n")
  xb <- as.numeric(Xm %*% b); zu <- as.numeric(Zm %*% uu)
  V <- Zm %*% S %*% t(Zm) + Rm
  .t1_result(mean_marginal = xb, mean_conditional = xb + zu, signal = zu,
             V = V, n = n, p = p, q = q,
             method = "Linear mixed model, MVSML Eq. (2.1)")
}

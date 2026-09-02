# SPDX-License-Identifier: AGPL-3.0-or-later
#' Genomic BLUP
#'
#' Henderson's mixed model equations give the fixed and genomic effects
#' jointly.  With G the identity the random block is ridge regression
#' with penalty sigma_e^2/sigma_u^2, which is the closed form the tests
#' check; as the penalty tends to zero the system tends to ordinary
#' least squares on the concatenated design.  G is ridged before
#' inversion because VanRaden's G is singular whenever markers
#' outnumber individuals.
#'
#' Formula: \[X'X, X'Z; Z'X, Z'Z + G^{-1} k\] \[beta; u\] = \[X'y; Z'y\],
#'   k = sigma_e^2 / sigma_u^2.
#'
#' @param y Response vector.
#' @param X Fixed-effect design matrix.
#' @param Z Random-effect incidence matrix.
#' @param G Genomic relationship matrix.
#' @param var_u,var_e Variance components, positive.
#' @param ridge Ridge added to G before inversion.
#' @return List with \code{estimate}, \code{beta}, \code{u},
#'   \code{fitted}, \code{residual_ss}, \code{lambda}, \code{n},
#'   \code{method}.
#' @references VanRaden (2008), Efficient methods to compute genomic
#'   predictions, Journal of Dairy Science 91(11):4414-4423.
#'   \doi{10.3168/jds.2007-0980}
#' @export
#' @examples
#' set.seed(1)
#' n <- 30; q <- 5
#' X <- cbind(1, rnorm(n))
#' Z <- matrix(rbinom(n * q, 1, 0.5), n, q)
#' G <- diag(q)
#' u <- rnorm(q)
#' y <- X %*% c(1, 0.5) + Z %*% u + rnorm(n)
#' Gblupr(y, X, Z, G)
Gblupr <- function(y, X, Z, G, var_u = 1, var_e = 1, ridge = 1e-8) {
  yv <- .s03vec(y); Xm <- .s03mat(X); Zm <- .s03mat(Z); Gm <- .s03mat(G)
  n <- length(yv)
  if (n == 0L) stop("gblup_estimator: y is empty")
  if (nrow(Xm) != n || nrow(Zm) != n) stop("gblup_estimator: X and Z must have one row per observation")
  p <- ncol(Xm); q <- ncol(Zm)
  if (nrow(Gm) != q || ncol(Gm) != q) stop("gblup_estimator: G must be q x q")
  vu <- as.numeric(var_u); ve <- as.numeric(var_e)
  if (vu <= 0 || ve <= 0) stop("gblup_estimator: variance components must be positive")
  k <- ve / vu
  Ginv <- solve(Gm + diag(as.numeric(ridge), q))
  A <- matrix(0, p + q, p + q)
  A[seq_len(p), seq_len(p)] <- t(Xm) %*% Xm
  A[seq_len(p), p + seq_len(q)] <- t(Xm) %*% Zm
  A[p + seq_len(q), seq_len(p)] <- t(Zm) %*% Xm
  A[p + seq_len(q), p + seq_len(q)] <- t(Zm) %*% Zm + Ginv * k
  b <- c(as.numeric(t(Xm) %*% yv), as.numeric(t(Zm) %*% yv))
  sol <- as.numeric(solve(A, b))
  beta <- sol[seq_len(p)]; u <- sol[p + seq_len(q)]
  fitted <- as.numeric(Xm %*% beta + Zm %*% u)
  resid <- yv - fitted
  .t1_result(estimate = u[1], beta = beta, u = u, fitted = fitted,
             residual_ss = sum(resid * resid), lambda = k, n = n,
             method = "Henderson mixed model equations with G from VanRaden (2008)")
}

# SPDX-License-Identifier: AGPL-3.0-or-later
#' Genomic heritability from a relationship matrix
#'
#' The restricted log-likelihood profiled over the total variance
#' depends only on h^2, so the estimate is found by evaluating that
#' profile on a deterministic grid and refining the grid maximum by
#' golden-section search.  sigma_e^2 = 0 forces h^2 = 1 exactly, which
#' is the degenerate case the tests use.
#'
#' Formula: h^2 = sigma_g^2 / (sigma_g^2 + sigma_e^2); profile
#'   -0.5 \[log|V| + log(1'V^{-1}1) + (n - 1) log(y' P y)\] with
#'   V = h^2 K + (1 - h^2) I.
#'
#' @param y Phenotypes.
#' @param K Genomic relationship matrix, n x n.
#' @param grid Number of grid points on (0, 1).
#' @param refine Golden-section refinement steps.
#' @return List with \code{estimate}, \code{h2}, \code{var_g},
#'   \code{var_e}, \code{total_var}, \code{loglik}, \code{grid_h2},
#'   \code{grid_loglik}, \code{n}, \code{method}.
#' @references VanRaden (2008), Journal of Dairy Science
#'   91(11):4414-4423. \doi{10.3168/jds.2007-0980}
#' @export
#' @examples
#' set.seed(1)
#' X <- matrix(rnorm(20), 10, 2)
#' K <- X %*% t(X)
#' y <- rnorm(10)
#' Hertbg(y, K)
Hertbg <- function(y, K, grid = 41, refine = 40) {
  yv <- .s03vec(y); Km <- .s03mat(K)
  n <- length(yv)
  if (n < 3L) stop("heritability: need at least three observations")
  if (nrow(Km) != n || ncol(Km) != n) stop("heritability: K must be n x n")
  g <- as.integer(grid)
  if (g < 3L) stop("heritability: grid must have at least three points")
  reml <- function(h2) {
    V <- h2 * Km + diag(1 - h2, n)
    L <- .s03chol(V)
    Vi_y <- .s03cholsolve(V, yv)
    Vi_1 <- .s03cholsolve(V, rep(1, n))
    s11 <- sum(Vi_1); s1y <- sum(Vi_y)
    yPy <- sum(yv * Vi_y) - s1y * s1y / s11
    list(ll = -0.5 * (2 * sum(log(diag(L))) + log(s11) + (n - 1) * log(yPy)), yPy = yPy)
  }
  lo <- 1e-6; hi <- 1 - 1e-6
  hs <- lo + (hi - lo) * (seq_len(g) - 1) / (g - 1)
  lls <- vapply(hs, function(h) reml(h)$ll, 0)
  bh <- hs[which.max(lls)]
  step <- (hi - lo) / (g - 1)
  a <- max(bh - step, lo); b <- min(bh + step, hi)
  for (i in seq_len(as.integer(refine))) {
    m1 <- a + (b - a) / 3; m2 <- b - (b - a) / 3
    if (reml(m1)$ll < reml(m2)$ll) a <- m1 else b <- m2
  }
  h2 <- (a + b) / 2
  r <- reml(h2)
  total <- r$yPy / (n - 1)
  .t1_result(estimate = h2, h2 = h2, var_g = h2 * total, var_e = (1 - h2) * total,
             total_var = total, loglik = r$ll, grid_h2 = hs, grid_loglik = lls,
             n = n,
             method = "REML profile in h^2 over V = h^2 K + (1 - h^2) I, golden-section refined; VanRaden (2008) G")
}

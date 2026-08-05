# SPDX-License-Identifier: AGPL-3.0-or-later
#' Gaussian-process prior draw
#'
#' f ~ GP(0, k) means every finite-dimensional marginal is multivariate
#' normal with covariance k(x_i, x_j).  Kolmogorov extension turns that
#' consistent family into a process, and the practical consequence is
#' that a draw is nothing more than a Cholesky factor times a standard
#' normal vector.
#'
#' Formula: f = L Z with L L' = K, K_ij = var exp(-(x_i - x_j)^2 /
#'   (2 length^2)), Z iid N(0, 1).
#'
#' @param x Evaluation points.
#' @param length Squared-exponential length scale, positive.
#' @param var Kernel variance, positive.
#' @param seed Seed for the deterministic draws.
#' @return List with \code{estimate} (mean of the draw), \code{f},
#'   \code{method}.
#' @references Ghosal & van der Vaart (2017), Fundamentals of
#'   Nonparametric Bayesian Inference, CUP, section 2.2.1.
#' @export
Ghosalgppriordef <- function(x, length = 0.5, var = 1, seed = 42) {
  xs <- as.numeric(x)
  n <- base::length(xs)
  if (n == 0L) stop("x must be non-empty")
  if (length <= 0) stop("length must be positive")
  if (var <= 0) stop("var must be positive")
  K <- var * exp(-0.5 * (outer(xs, xs, "-") / length)^2) + diag(1e-10, n)
  L <- t(chol(K))
  e <- .ghc_rng(seed)
  f <- as.numeric(L %*% .ghc_norm(e, n))
  .t1_result(estimate = mean(f), f = f,
             method = "GP prior draw via Cholesky (GvdV 2017 sec. 2.2.1)")
}

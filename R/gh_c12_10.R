# SPDX-License-Identifier: AGPL-3.0-or-later
#' White-noise linear-functional Bernstein-von Mises
#'
#' sqrt(n)(L(theta_post) - L(theta0)) tends to N(0, ||L||^2) for a
#' BOUNDED linear functional.  Boundedness is the whole condition: the
#' full infinite-dimensional BvM can fail while every bounded linear
#' functional of the same posterior still satisfies it, which is the
#' distinction section 12.4 is drawing.
#'
#' Formula: theta_post = Y prior_var / (prior_var + 1/n);
#'   Var(sqrt(n) L(theta_post - theta0)) -> sum_k L_k^2.
#'
#' @param L_coefs Coefficients of the linear functional.
#' @param n Precision (sample size).
#' @param prior_var Prior variance per coordinate.
#' @param n_sim Number of replicates.
#' @param seed Seed for the deterministic draws.
#' @return List with \code{estimate} (empirical variance),
#'   \code{norm2_L}, \code{gap}, \code{method}.
#' @references Ghosal & van der Vaart (2017), Fundamentals of
#'   Nonparametric Bayesian Inference, CUP, section 12.4.2.
#' @export
#' @examples
#' Ghosalwnlinbvm()
Ghosalwnlinbvm <- function(L_coefs = c(0.6, 0.8), n = 500,
                           prior_var = 50, n_sim = 500, seed = 42) {
  L <- as.numeric(L_coefs)
  n_sim <- as.integer(n_sim)
  if (length(L) != 2L)
    stop("L_coefs must have length 2 for the built-in two-coordinate truth")
  if (n <= 0) stop("n must be positive")
  if (prior_var <= 0) stop("prior_var must be positive")
  if (n_sim < 2L) stop("n_sim must be at least 2")
  e <- .ghc_rng(seed)
  L2 <- sum(L * L)
  theta0 <- c(0.3, -0.4)
  shrink <- prior_var / (prior_var + 1 / n)
  devs <- numeric(n_sim)
  for (i in seq_len(n_sim)) {
    y <- theta0 + .ghc_norm(e, 2) / sqrt(n)
    devs[i] <- sqrt(n) * sum(L * (shrink * y - theta0))
  }
  v <- sum((devs - mean(devs))^2) / (n_sim - 1)
  .t1_result(estimate = v, norm2_L = L2, gap = abs(v - L2),
             method = "linear-functional BvM (GvdV 2017 sec. 12.4.2)")
}

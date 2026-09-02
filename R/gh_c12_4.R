# SPDX-License-Identifier: AGPL-3.0-or-later
#' Semiparametric Bernstein-von Mises for a smooth functional
#'
#' sqrt(n)(psi(G_post) - psi(F0)) tends to N(0, sigma_eff^2) for a smooth
#' functional psi.  The variance in the limit is the SEMIPARAMETRIC
#' EFFICIENCY BOUND, not merely some variance: the Bayes procedure is
#' asymptotically efficient without being told the functional in advance.
#' Here psi is the mean and the truth uniform, so the bound is
#' var(X) = 1/12.
#'
#' Formula: psi(G_post) = (alpha/2 + sum_i X_i) / (alpha + n);
#'   Var(sqrt(n)(psi(G_post) - 1/2)) -> 1/12.
#'
#' @param n Sample size per replicate.
#' @param alpha DP concentration parameter, positive.
#' @param n_sim Number of replicates.
#' @param seed Seed for the deterministic draws.
#' @return List with \code{estimate} (empirical variance),
#'   \code{efficient_variance}, \code{gap}, \code{method}.
#' @references Ghosal & van der Vaart (2017), Fundamentals of
#'   Nonparametric Bayesian Inference, CUP, section 12.3.
#' @export
#' @examples
#' set.seed(1)
#' r <- Ghosalsemiparabvm(); TRUE
Ghosalsemiparabvm <- function(n = 2000, alpha = 2, n_sim = 400,
                              seed = 42) {
  n <- as.integer(n)
  n_sim <- as.integer(n_sim)
  if (n < 1L) stop("n must be positive")
  if (n_sim < 2L) stop("n_sim must be at least 2")
  if (alpha <= 0) stop("alpha must be positive")
  e <- .ghc_rng(seed)
  devs <- numeric(n_sim)
  for (i in seq_len(n_sim)) {
    s <- sum(.ghc_unif(e, n))
    devs[i] <- sqrt(n) * ((alpha * 0.5 + s) / (alpha + n) - 0.5)
  }
  v <- sum((devs - mean(devs))^2) / (n_sim - 1)
  .t1_result(estimate = v, efficient_variance = 1 / 12,
             gap = abs(v - 1 / 12),
             method = "semiparametric BvM (GvdV 2017 sec. 12.3)")
}

# SPDX-License-Identifier: AGPL-3.0-or-later
#' Dirichlet-process Bernstein-von Mises (Brownian bridge)
#'
#' sqrt(n)(F_post - F0) converges to B(F0) with B a Brownian bridge, so
#' at a fixed t the limit is N(0, F0(t)(1 - F0(t))).  The bridge, not a
#' plain Brownian motion, is what appears because the posterior CDF is
#' pinned at both 0 and 1 -- and the concentration parameter alpha washes
#' out entirely in the limit, which is the substantive content.
#'
#' Formula: F_post(t) = (alpha t + N_t) / (alpha + n);
#'   Var(sqrt(n)(F_post(t) - t)) -> t(1 - t).
#'
#' @param n Sample size per replicate.
#' @param alpha DP concentration parameter, positive.
#' @param n_sim Number of replicates.
#' @param seed Seed for the deterministic draws.
#' @return List with \code{estimate} (empirical variance),
#'   \code{bridge_variance}, \code{gap}, \code{method}.
#' @references Ghosal & van der Vaart (2017), Fundamentals of
#'   Nonparametric Bayesian Inference, CUP, section 12.2.
#' @export
Ghosaldpbvm <- function(n = 2000, alpha = 2, n_sim = 400, seed = 42) {
  n <- as.integer(n); n_sim <- as.integer(n_sim)
  if (n < 1L) stop("n must be positive")
  if (n_sim < 2L) stop("n_sim must be at least 2")
  if (alpha <= 0) stop("alpha must be positive")
  e <- .ghc_rng(seed)
  t <- 0.3
  devs <- numeric(n_sim)
  for (i in seq_len(n_sim)) {
    cnt <- sum(.ghc_unif(e, n) <= t)
    devs[i] <- sqrt(n) * ((alpha * t + cnt) / (alpha + n) - t)
  }
  v <- sum((devs - mean(devs))^2) / (n_sim - 1)
  v_bridge <- t * (1 - t)
  .t1_result(estimate = v, bridge_variance = v_bridge,
             gap = abs(v - v_bridge),
             method = "DP BvM / Brownian bridge (GvdV 2017 sec. 12.2)")
}

# SPDX-License-Identifier: AGPL-3.0-or-later
#' Neutral-to-the-right functional Bernstein-von Mises
#'
#' sqrt(n)(psi(F_post) - psi(F0)) tends to N(0, sigma^2) for smooth
#' functionals of an NTR posterior.  With psi = F(1) under an
#' uncensored exponential truth the limiting variance is
#' F0(1)(1 - F0(1)), the same bound a fully parametric analysis would
#' give -- the nonparametric prior costs nothing for this functional.
#'
#' Formula: F_post(1) = (2 F0(1) + N_1) / (2 + n);
#'   Var(sqrt(n)(F_post(1) - F0(1))) -> F0(1)(1 - F0(1)).
#'
#' @param n Sample size per replicate.
#' @param n_sim Number of replicates.
#' @param seed Seed for the deterministic draws.
#' @return List with \code{estimate} (empirical variance),
#'   \code{efficient_variance}, \code{gap}, \code{method}.
#' @references Ghosal & van der Vaart (2017), Fundamentals of
#'   Nonparametric Bayesian Inference, CUP, section 13.4.2.
#' @export
#' @examples
#' Ghosalntrbvm()
Ghosalntrbvm <- function(n = 1500, n_sim = 300, seed = 42) {
  n <- as.integer(n)
  n_sim <- as.integer(n_sim)
  if (n < 1L) stop("n must be positive")
  if (n_sim < 2L) stop("n_sim must be at least 2")
  e <- .ghc_rng(seed)
  F0_1 <- 1 - exp(-1)
  devs <- numeric(n_sim)
  for (i in seq_len(n_sim)) {
    cnt <- sum(-log(pmax(.ghc_unif(e, n), 1e-12)) <= 1)
    devs[i] <- sqrt(n) * ((2 * F0_1 + cnt) / (2 + n) - F0_1)
  }
  v <- sum((devs - mean(devs))^2) / (n_sim - 1)
  target <- F0_1 * (1 - F0_1)
  .t1_result(estimate = v, efficient_variance = target,
             gap = abs(v - target),
             method = "NTR functional BvM (GvdV 2017 sec. 13.4.2)")
}

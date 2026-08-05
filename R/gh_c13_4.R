# SPDX-License-Identifier: AGPL-3.0-or-later
#' Discrete-time beta process
#'
#' H(t) = sum_{s <= t} dH(s) with independent
#' dH(s_k) ~ Be(c_k h_k, c_k(1 - h_k)).  The Beta parameterisation is
#' chosen so that E dH(s_k) = h_k EXACTLY, whatever c is: c controls only
#' the spread, so the prior mean hazard can be specified without
#' committing to a strength.  The simulated means recover h_k.
#'
#' Formula: E Be(c h, c(1 - h)) = h.
#'
#' @param hazards0 Prior mean hazards, each strictly inside (0, 1).
#' @param c Concentration, positive.
#' @param seed Seed for the deterministic draws.
#' @return List with \code{estimate} (sum of the simulated means),
#'   \code{mean_by_time}, \code{prior_mean_gap}, \code{method}.
#' @references Ghosal & van der Vaart (2017), Fundamentals of
#'   Nonparametric Bayesian Inference, CUP, section 13.3.1.
#' @export
Ghosalbpdiscrete <- function(hazards0 = c(0.1, 0.2, 0.3), c = 4,
                             seed = 42) {
  h0 <- as.numeric(hazards0)
  if (length(h0) == 0L) stop("hazards0 must be non-empty")
  if (any(h0 <= 0 | h0 >= 1))
    stop("every hazard must lie strictly between 0 and 1")
  if (c <= 0) stop("c must be positive")
  e <- .ghc_rng(seed)
  n_sim <- 2000L
  means <- numeric(length(h0))
  for (it in seq_len(n_sim))
    for (k in seq_along(h0))
      means[k] <- means[k] + .ghc_beta1(e, c * h0[k], c * (1 - h0[k])) / n_sim
  .t1_result(estimate = sum(means), mean_by_time = means,
             prior_mean_gap = max(abs(means - h0)),
             method = "discrete beta process (GvdV 2017 sec. 13.3.1)")
}

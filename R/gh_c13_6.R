# SPDX-License-Identifier: AGPL-3.0-or-later
#' Beta-process path by Poisson jumps
#'
#' H(t) = sum over tau_k <= t of J_k with (J_k, tau_k) drawn from a
#' Poisson process carrying the beta-process Levy intensity.  Building
#' the path from its jumps rather than from grid increments is what makes
#' the pure-jump, nondecreasing structure explicit -- a beta process has
#' no continuous part at all.
#'
#' Formula: tau_k ~ U(0, t_max), J_k ~ Be(1, c) c 5 / n_jumps, and the
#'   path is the running total of the jumps in time order.
#'
#' @param c Concentration, positive.
#' @param t_max Time horizon.
#' @param n_jumps Number of jumps simulated.
#' @param seed Seed for the deterministic draws.
#' @return List with \code{estimate} (terminal value),
#'   \code{n_jumps}, \code{pure_jump_nondecreasing}, \code{method}.
#' @references Ghosal & van der Vaart (2017), Fundamentals of
#'   Nonparametric Bayesian Inference, CUP, section 13.3.3.
#' @export
Ghosalbppathgen <- function(c = 1.5, t_max = 1, n_jumps = 300,
                            seed = 42) {
  n_jumps <- as.integer(n_jumps)
  if (c <= 0) stop("c must be positive")
  if (n_jumps < 1L) stop("n_jumps must be positive")
  e <- .ghc_rng(seed)
  tau <- numeric(n_jumps); J <- numeric(n_jumps)
  for (k in seq_len(n_jumps)) {
    tau[k] <- .ghc_unif(e, 1L) * t_max
    J[k] <- .ghc_beta1(e, 1, c) / n_jumps * c * 5
  }
  ord <- order(tau)
  path <- cumsum(J[ord])
  nd <- if (n_jumps < 2L) TRUE else all(diff(path) >= 0)
  .t1_result(estimate = path[n_jumps], n_jumps = n_jumps,
             pure_jump_nondecreasing = nd,
             method = "BP Poisson-jump path (GvdV 2017 sec. 13.3.3)")
}

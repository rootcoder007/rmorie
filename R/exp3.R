# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Exp3 adversarial bandit (Exp3). Bit-identical mirror of
# src/morie/fn/exp3.py, driving the same SplitMix64 stream.

#' Exp3 exponential-weight adversarial bandit
#'
#' The Exp3 algorithm of Auer, Cesa-Bianchi, Freund and Schapire
#' (2002), figure 1: weights start at 1; at each trial
#' \eqn{p_i(t) = (1-\gamma) w_i(t) / \sum_j w_j(t) + \gamma/K,}
#' an action is drawn from p(t), and only the chosen action updates its
#' weight with the importance-weighted reward:
#' \eqn{w_i(t+1) = w_i(t) \exp(\gamma (x_i(t)/p_i(t)) / K).}
#' Theorem 3.1 of the source bounds the expected weak regret.  The draw
#' consumes exactly one uniform per trial, by inverse CDF on the
#' probabilities in index order, mirroring the Python arm bit-exactly.
#'
#' @param x Matrix (T, K): reward each action pays at each trial, in
#'   0 to 1.
#' @param gamma_ Mixing/learning parameter in (0, 1].
#' @param T Number of trials (default all rows).
#' @param seed SplitMix64 seed.
#' @return List with \code{estimate} (0-based action with the largest
#'   final weight), \code{actions}, \code{rewards}, \code{probs}
#'   ((T, K) matrix), \code{weights}, \code{total_reward},
#'   \code{method}.
#' @references Auer, P., Cesa-Bianchi, N., Freund, Y. and Schapire,
#'   R. E. (2002). The nonstochastic multiarmed bandit problem. SIAM
#'   Journal on Computing 32(1), 48-77.  Algorithm: figure 1, Section
#'   3; bound: Theorem 3.1.  Local source:
#'   fetched-wave3/auer-cesabianchi-freund-schapire-2002-exp3-nonstochastic-bandit.pdf.
#' @examples
#' x <- matrix(rep(c(1, 0), 5), ncol = 2, byrow = TRUE)
#' Exp3(x, 0.2)$weights
#' @export
Exp3 <- function(x, gamma_, T = NULL, seed = 0) {
  x <- as.matrix(x)
  storage.mode(x) <- "double"
  rows <- nrow(x)
  K <- ncol(x)
  T <- if (is.null(T)) rows else as.integer(T)
  if (T > rows) stop(sprintf("x has only %d rows", rows), call. = FALSE)
  g <- as.numeric(gamma_)
  if (g <= 0 || g > 1) stop("gamma_ must be in (0, 1]", call. = FALSE)
  e <- .ghc_rng(seed)
  w <- rep(1, K)
  probs <- matrix(0, T, K)
  actions <- numeric(T)
  rewards <- numeric(T)
  for (t in seq_len(T)) {
    tot <- 0
    for (j in seq_len(K)) tot <- tot + w[j]
    p <- (1 - g) * w / tot + g / K
    probs[t, ] <- p
    u <- .ghc_unif(e, 1L)
    cc <- 0
    i <- K
    for (j in seq_len(K)) {
      cc <- cc + p[j]
      if (u <= cc) { i <- j
      break }
    }
    r <- x[t, i]
    w[i] <- w[i] * exp(g * (r / p[i]) / K)
    actions[t] <- as.numeric(i - 1L)
    rewards[t] <- r
  }
  best <- 1L
  if (K > 1L) for (j in seq(2L, K)) if (w[j] > w[best]) best <- j
  list(estimate = as.numeric(best - 1L), actions = actions,
       rewards = rewards, probs = probs, weights = w,
       total_reward = sum(rewards),
       method = "Exp3 exponential-weight adversarial bandit")
}

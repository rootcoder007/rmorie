# SPDX-License-Identifier: AGPL-3.0-or-later
#
# UCB1 bandit policy (Ucbb). Bit-identical mirror of
# src/morie/fn/ucbb.py.

#' UCB1 index policy on a fixed reward table
#'
#' The deterministic policy UCB1 of Auer, Cesa-Bianchi and Fischer
#' (2002), figure 1: play each machine once, then always play the
#' machine maximizing
#' \eqn{\bar{x}_j + \sqrt{2 \ln n / n_j}}
#' where \eqn{\bar{x}_j} is the average reward obtained from machine j,
#' \eqn{n_j} its play count so far and n the overall number of plays
#' done so far.  The policy is deterministic given the observed
#' rewards, so it is run against a caller-supplied (T, K) table whose
#' row t holds the reward each machine would pay at play t.  Rewards
#' have support 0 to 1 in the source; Theorem 1 gives the logarithmic
#' regret bound.  Initialization plays machines in index order; ties in
#' the index break to the lowest machine.
#'
#' @param x Matrix (T, K) of realized rewards.
#' @param T Number of plays (default all rows; at least K).
#' @return List with \code{estimate} (0-based machine with the highest
#'   final average reward), \code{actions} (0-based machine per play),
#'   \code{rewards}, \code{means}, \code{counts}, \code{index} (final
#'   UCB index with n = T), \code{total_reward}, \code{method}.
#' @references Auer, P., Cesa-Bianchi, N. and Fischer, P. (2002).
#'   Finite-time analysis of the multiarmed bandit problem. Machine
#'   Learning 47, 235-256.  Policy: figure 1, p. 237; bound: Theorem 1.
#'   Local source:
#'   fetched-wave3/auer-cesabianchi-fischer-2002-ucb1-finite-time-ML47.pdf.
#' @examples
#' x <- matrix(rep(c(1, 0), 6), ncol = 2, byrow = TRUE)
#' Ucbb(x)$actions
#' @export
Ucbb <- function(x, T = NULL) {
  x <- as.matrix(x)
  storage.mode(x) <- "double"
  rows <- nrow(x)
  K <- ncol(x)
  T <- if (is.null(T)) rows else as.integer(T)
  if (T < K) stop(sprintf("need at least K = %d plays", K), call. = FALSE)
  if (T > rows) stop(sprintf("x has only %d rows", rows), call. = FALSE)
  counts <- integer(K)
  sums <- numeric(K)
  actions <- numeric(T)
  rewards <- numeric(T)
  for (t in seq_len(T)) {
    if (t <= K) {
      j <- t
    } else {
      n <- t - 1L
      best <- 1L
      bestidx <- -Inf
      for (k in seq_len(K)) {
        idx <- sums[k] / counts[k] + sqrt(2 * log(n) / counts[k])
        if (idx > bestidx) {
          bestidx <- idx
          best <- k
        }
      }
      j <- best
    }
    r <- x[t, j]
    counts[j] <- counts[j] + 1L
    sums[j] <- sums[j] + r
    actions[t] <- as.numeric(j - 1L)
    rewards[t] <- r
  }
  means <- numeric(K)
  index <- numeric(K)
  for (k in seq_len(K)) {
    means[k] <- sums[k] / counts[k]
    index[k] <- means[k] + sqrt(2 * log(T) / counts[k])
  }
  best <- 1L
  if (K > 1L) for (k in seq(2L, K)) if (means[k] > means[best]) best <- k
  list(estimate = as.numeric(best - 1L), actions = actions,
       rewards = rewards, means = means,
       counts = as.numeric(counts), index = index,
       total_reward = sum(rewards),
       method = "UCB1 index policy on a fixed reward table")
}

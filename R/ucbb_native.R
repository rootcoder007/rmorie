# Multi-armed bandit policies: UCB1, Exp3, Thompson sampling.
# Sources:
#   UCB1 -- Auer, P., Cesa-Bianchi, N. and Fischer, P. (2002),
#     Finite-time analysis of the multiarmed bandit problem, Machine
#     Learning 47, 235-256; policy in Figure 1, p. 237, regret bound
#     Theorem 1.
#   Exp3 -- Auer, P., Cesa-Bianchi, N., Freund, Y. and Schapire, R. E.
#     (2002), The nonstochastic multiarmed bandit problem, SIAM J.
#     Comput. 32(1), 48-77; Figure 1 and Theorem 3.1.
#   Thompson -- Russo, D. et al. (2018), A tutorial on Thompson
#     sampling, FnT ML 11(1), 1-96, Algorithm 3.2 (BernTS);
#     Thompson, W. R. (1933), Biometrika 25, 285-294.
# Mirrors Python morie.fn.ucbb / exp3 / thomp exactly, including the
# RNG stream: Exp3 consumes one uniform per trial by inverse CDF in
# index order; Thompson consumes one Beta draw per action (in index
# order) then one uniform for the Bernoulli reward, per period.

#' UCB1 bandit policy on a realized-reward table
#'
#' Plays each machine once, then always plays the machine maximising
#' xbar_j + sqrt(2 log(n) / n_j) (Auer et al. 2002, Figure 1).  The
#' policy is deterministic given the reward table, so no RNG is used.
#' Index ties break to the lowest machine.
#'
#' @param x Reward table (T x K); \code{x[t, j]} is what machine j
#'   pays at play t.
#' @param T Number of plays (default all rows; must be >= K).
#' @return A list with elements \code{estimate} (0-based machine with
#'   the highest final mean), \code{actions}, \code{rewards},
#'   \code{means}, \code{counts}, \code{index}, \code{total_reward},
#'   \code{method}.
#' @references Auer, P., Cesa-Bianchi, N. and Fischer, P. (2002).
#'   Finite-time analysis of the multiarmed bandit problem. Machine
#'   Learning, 47, 235-256.
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' morie_ucbb(V)
morie_ucbb <- function(x, T = NULL) {
  x <- as.matrix(x)
  rows <- nrow(x); K <- ncol(x)
  T <- if (is.null(T)) rows else as.integer(T)
  if (T < K) stop(sprintf("need at least K = %d plays", K))
  if (T > rows) stop(sprintf("x has only %d rows", rows))
  counts <- rep(0L, K); sums <- rep(0, K)
  actions <- numeric(T); rewards <- numeric(T)
  for (t in seq_len(T)) {
    if (t <= K) {
      j <- t
    } else {
      n <- t - 1L
      best <- 1L; bestidx <- -Inf
      for (k in seq_len(K)) {
        idx <- sums[k] / counts[k] + sqrt(2 * log(n) / counts[k])
        if (idx > bestidx) { bestidx <- idx; best <- k }
      }
      j <- best
    }
    r <- x[t, j]
    counts[j] <- counts[j] + 1L
    sums[j] <- sums[j] + r
    actions[t] <- j - 1L
    rewards[t] <- r
  }
  means <- sums / counts
  index <- means + sqrt(2 * log(T) / counts)
  best <- 1L
  for (k in seq_len(K)) if (k > 1L && means[k] > means[best]) best <- k
  list(estimate = best - 1, actions = actions, rewards = rewards,
       means = means, counts = as.numeric(counts), index = index,
       total_reward = sum(rewards),
       method = "UCB1 bandit policy (Auer et al. 2002, Figure 1)")
}

# SPDX-License-Identifier: AGPL-3.0-or-later
#' Epsilon-greedy action selection on a stationary bandit
#'
#' Sutton and Barto (2018), 2nd ed. (FETCHED from incompleteideas.net),
#' section 2.2: "behave greedily most of the time, but every once in a
#' while, say with small probability eps, instead select randomly from
#' among all the actions with equal probability", so P(a) = 1 - eps +
#' eps/k for the greedy action and eps/k otherwise; the values are the
#' sample averages of section 2.4, Q_(n+1) = Q_n + (1/n)(R_n - Q_n).
#'
#' Determinism: exploration decisions come from the van der Corput
#' sequence, one uniform per pull, so the long-run share of exploratory
#' pulls is eps by construction and the run reproduces in both arms.
#'
#' @param arms true mean reward of each arm.
#' @param epsilon exploration probability.
#' @param T number of pulls.
#' @param rewards optional realised reward per (pull, arm).
#' @param q0 optimistic initial value.
#' @return list: estimate, q, counts, total_reward, regret, p_greedy, n,
#'   method.
#' @keywords internal
#' @examples
#' Epsgreedy(c(0.2, 0.5, 0.1), 0.1, 50)$counts
#' @export
Epsgreedy <- function(arms, epsilon = 0.1, T = 100, rewards = NULL, q0 = 0) {
  mu <- .s03vec(arms)
  kk <- length(mu)
  e <- as.numeric(epsilon)
  n <- as.integer(T)
  q <- rep(as.numeric(q0), kk)
  cnt <- numeric(kk)
  total <- 0
  best <- 1L
  if (kk > 1L) for (a in seq(2L, kk)) if (mu[a] > mu[best]) best <- a
  for (t in seq_len(n) - 1L) {
    u <- .s03vdc(t, 2L)
    if (u < e) {
      a <- as.integer(.s03vdc(t, 3L) * kk) + 1L
      if (a > kk) a <- kk
    } else {
      a <- 1L
      if (kk > 1L) for (j in seq(2L, kk)) if (q[j] > q[a]) a <- j
    }
    rw <- if (!is.null(rewards)) as.numeric(.s03mat(rewards)[t + 1L, a]) else mu[a]
    cnt[a] <- cnt[a] + 1
    q[a] <- q[a] + (rw - q[a]) / cnt[a]
    total <- total + rw
  }
  list(estimate = if (n) total / n else NaN, q = q, counts = cnt,
       total_reward = total,
       regret = if (kk) n * mu[best] - total else NaN,
       p_greedy = if (kk) 1 - e + e / kk else NaN, n = n,
       method = "Epsilon-greedy with sample-average values (Sutton and Barto 2018, sec. 2.2-2.4)")
}

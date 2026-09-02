# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Tabular Double Q-learning (Ddqn). Bit-identical mirror of
# src/morie/fn/ddqn.py, driving the same SplitMix64 stream.

#' Tabular Double Q-learning on an explicit finite MDP
#'
#' Maintains two tables and, with probability one half per step,
#' applies the update of Sutton and Barto (2018) eq. 6.10,
#' \eqn{Q_1(S,A) \leftarrow Q_1(S,A) + \alpha \[R + \gamma Q_2(S', \arg\max_a Q_1(S',a)) - Q_1(S,A)\]}
#' else the same with the tables swapped; behaviour actions are
#' epsilon-greedy in \eqn{Q_1 + Q_2}.  This is the boxed algorithm of
#' their Section 6.7, p. 136; the estimator is van Hasselt (2010), and
#' the same selection/evaluation decoupling applied to deep Q-networks
#' is Double DQN, van Hasselt, Guez and Silver (2016), eq. 4.
#' Determinism conventions mirror the Python arm bit-exactly: per step
#' the stream yields one uniform for the epsilon test, one for the
#' random action when exploring, one for the next-state draw (inverse
#' CDF on the transition row), one for the coin (update the first
#' table when u below one half); greedy ties break to the lowest
#' action index.
#'
#' @param P List of A transition matrices, each (S, S) row-stochastic.
#' @param R Matrix (S, A) of expected rewards, or a list of A (S, S)
#'   per-transition reward matrices averaged under P.
#' @param gamma Discount factor in 0 to 1.
#' @param alpha Step size in 0 to 1.
#' @param epsilon Exploration probability.
#' @param n_episodes Number of episodes.
#' @param start 0-based start state.
#' @param terminal Integer vector of 0-based absorbing terminal states.
#' @param max_steps Step cap per episode.
#' @param seed SplitMix64 seed.
#' @return List with \code{estimate} (average of the two (S, A)
#'   tables), \code{q1}, \code{q2}, \code{policy} (0-based greedy
#'   actions on the average), \code{v}, \code{n_steps},
#'   \code{n_episodes}, \code{method}.
#' @references Sutton, R. S. and Barto, A. G. (2018). Reinforcement
#'   Learning: An Introduction, 2nd ed., MIT Press, Section 6.7, boxed
#'   algorithm p. 136, eq. 6.10.  Local source:
#'   fetched-wave3/sutton-barto-2018-reinforcement-learning-2nd-ed.pdf.
#'   van Hasselt, H. (2010). Double Q-learning. NeurIPS 23, 2613-2621.
#'   Local source: fetched-wave3/hasselt-2010-double-qlearning-neurips.pdf.
#'   van Hasselt, H., Guez, A. and Silver, D. (2016). Deep
#'   reinforcement learning with Double Q-learning. AAAI 2016
#'   (arXiv:1509.06461), eq. 4.  Local source:
#'   fetched-wave3/hasselt-guez-silver-2016-ddqn-arxiv1509.06461.pdf.
#' @examples
#' P <- list(matrix(c(0, 1, 0, 1), 2, byrow = TRUE))
#' R <- matrix(c(1, 0), 2)
#' Ddqn(P, R, 0.9, alpha = 0.5, epsilon = 0, n_episodes = 4,
#'      terminal = 1L)$estimate
#' @export
Ddqn <- function(P, R, gamma, alpha = 0.1, epsilon = 0.1,
                 n_episodes = 100L, start = 0L, terminal = integer(0),
                 max_steps = 1000L, seed = 0) {
  ar <- .w505_mdp_args(P, R)
  Pm <- ar$P; R <- ar$R; S <- ar$S; A <- ar$A
  gamma <- as.numeric(gamma); alpha <- as.numeric(alpha)
  epsilon <- as.numeric(epsilon)
  start <- as.integer(start)
  term <- as.integer(terminal)
  if (start < 0L || start >= S) stop("start out of range", call. = FALSE)
  e <- .ghc_rng(seed)
  Q1 <- matrix(0, S, A)
  Q2 <- matrix(0, S, A)
  n_steps <- 0L
  for (ep in seq_len(n_episodes)) {
    s <- start
    for (st in seq_len(max_steps)) {
      if (s %in% term) break
      u1 <- .ghc_unif(e, 1L)
      if (u1 < epsilon) {
        a <- as.integer(floor(.ghc_unif(e, 1L) * A))
        if (a >= A) a <- A - 1L
      } else {
        qs <- Q1[s + 1L, ] + Q2[s + 1L, ]
        a <- 0L
        if (A > 1L) for (j in seq(2L, A)) if (qs[j] > qs[a + 1L]) a <- j - 1L
      }
      u3 <- .ghc_unif(e, 1L)
      s2 <- .w505_sample_row(Pm[[a + 1L]][s + 1L, ], S, u3) - 1L
      r <- R[s + 1L, a + 1L]
      coin <- .ghc_unif(e, 1L)
      if (coin < 0.5) {
        nxt <- 0
        if (!(s2 %in% term)) {
          nxt <- Q2[s2 + 1L, .w505_greedy(Q1[s2 + 1L, ], A)]
        }
        Q1[s + 1L, a + 1L] <- Q1[s + 1L, a + 1L] +
          alpha * (r + gamma * nxt - Q1[s + 1L, a + 1L])
      } else {
        nxt <- 0
        if (!(s2 %in% term)) {
          nxt <- Q1[s2 + 1L, .w505_greedy(Q2[s2 + 1L, ], A)]
        }
        Q2[s + 1L, a + 1L] <- Q2[s + 1L, a + 1L] +
          alpha * (r + gamma * nxt - Q2[s + 1L, a + 1L])
      }
      n_steps <- n_steps + 1L
      s <- s2
    }
  }
  Q <- 0.5 * (Q1 + Q2)
  pol <- numeric(S); V <- numeric(S)
  for (s in seq_len(S)) {
    b <- .w505_greedy(Q[s, ], A)
    pol[s] <- as.numeric(b - 1L)
    V[s] <- Q[s, b]
  }
  list(estimate = Q, q1 = Q1, q2 = Q2, policy = pol, v = V,
       n_steps = n_steps, n_episodes = as.integer(n_episodes),
       method = "Tabular Double Q-learning (decoupled selection and evaluation)")
}

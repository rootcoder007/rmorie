# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Tabular Q-learning (Qlearn). Bit-identical mirror of
# src/morie/fn/qlearn.py, driving the same SplitMix64 stream.

#' @keywords internal
#' @noRd
.w505_greedy <- function(Qs, A) {
  b <- 1L
  if (A > 1L) for (a in seq(2L, A)) if (Qs[a] > Qs[b]) b <- a
  b
}

# Inverse-CDF draw on a stochastic row with a supplied uniform: running
# cumulative scan, exactly as the Python arm and .ghc_choice_p do it.
#' @keywords internal
#' @noRd
.w505_sample_row <- function(row, S, u) {
  cc <- 0
  for (s2 in seq_len(S)) {
    cc <- cc + row[s2]
    if (u <= cc) return(s2)
  }
  S
}

#' Tabular Q-learning on an explicit finite MDP
#'
#' The boxed algorithm of Sutton and Barto (2018), Section 6.5, p. 131,
#' with the update of their eq. 6.8:
#' \eqn{Q(S,A) \leftarrow Q(S,A) + \alpha \[R + \gamma \max_a Q(S',a) - Q(S,A)\]}
#' with epsilon-greedy behaviour.  Convergence to the optimal action
#' values is the theorem of Watkins and Dayan (1992).  Q(terminal, .)
#' stays 0.  Determinism conventions, mirrored bit-exactly against the
#' Python arm: greedy ties break to the lowest action index; each step
#' consumes, in order, one uniform for the epsilon test, one for the
#' uniform action when exploring (floor of u times A), one for the
#' next-state draw by inverse CDF on the transition row, all from the
#' shared SplitMix64 stream.
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
#' @param Q0 Optional (S, A) initial action values.
#' @return List with \code{estimate} (the learned (S, A) Q table),
#'   \code{policy} (0-based greedy actions), \code{v}, \code{n_steps},
#'   \code{n_episodes}, \code{method}.
#' @references Sutton, R. S. and Barto, A. G. (2018). Reinforcement
#'   Learning: An Introduction, 2nd ed., MIT Press, Section 6.5, boxed
#'   algorithm p. 131, eq. 6.8.  Local source:
#'   fetched-wave3/sutton-barto-2018-reinforcement-learning-2nd-ed.pdf.
#'   Watkins, C. J. C. H. and Dayan, P. (1992). Q-learning. Machine
#'   Learning 8, 279-292.  Local source:
#'   fetched-wave3/watkins-dayan-1992-qlearning-ML8.pdf.
#' @examples
#' P <- list(matrix(c(0, 1, 0, 1), 2, byrow = TRUE))
#' R <- matrix(c(1, 0), 2)
#' Qlearn(P, R, 0.9, alpha = 0.5, epsilon = 0, n_episodes = 3,
#'        terminal = 1L)$estimate
#' @export
Qlearn <- function(P, R, gamma, alpha = 0.1, epsilon = 0.1,
                   n_episodes = 100L, start = 0L, terminal = integer(0),
                   max_steps = 1000L, seed = 0, Q0 = NULL) {
  ar <- .w505_mdp_args(P, R)
  Pm <- ar$P
  R <- ar$R
  S <- ar$S
  A <- ar$A
  gamma <- as.numeric(gamma)
  alpha <- as.numeric(alpha)
  epsilon <- as.numeric(epsilon)
  start <- as.integer(start)
  term <- as.integer(terminal)
  if (start < 0L || start >= S) stop("start out of range", call. = FALSE)
  e <- .ghc_rng(seed)
  Q <- matrix(0, S, A)
  if (!is.null(Q0)) {
    Q0 <- as.matrix(Q0)
    for (s in seq_len(S)) {
      for (a in seq_len(A)) {
        Q[s, a] <- if ((s - 1L) %in% term) 0 else as.numeric(Q0[s, a])
      }
    }
  }
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
        a <- .w505_greedy(Q[s + 1L, ], A) - 1L
      }
      u3 <- .ghc_unif(e, 1L)
      s2 <- .w505_sample_row(Pm[[a + 1L]][s + 1L, ], S, u3) - 1L
      r <- R[s + 1L, a + 1L]
      nxt <- 0
      if (!(s2 %in% term)) {
        nxt <- Q[s2 + 1L, .w505_greedy(Q[s2 + 1L, ], A)]
      }
      Q[s + 1L, a + 1L] <- Q[s + 1L, a + 1L] +
        alpha * (r + gamma * nxt - Q[s + 1L, a + 1L])
      n_steps <- n_steps + 1L
      s <- s2
    }
  }
  pol <- numeric(S)
  V <- numeric(S)
  for (s in seq_len(S)) {
    b <- .w505_greedy(Q[s, ], A)
    pol[s] <- as.numeric(b - 1L)
    V[s] <- Q[s, b]
  }
  list(estimate = Q, policy = pol, v = V, n_steps = n_steps,
       n_episodes = as.integer(n_episodes),
       method = "Tabular Q-learning, epsilon-greedy off-policy TD control")
}

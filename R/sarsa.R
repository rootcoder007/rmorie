# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Tabular SARSA (Sarsa). Bit-identical mirror of src/morie/fn/sarsa.py,
# driving the same SplitMix64 stream.

#' Tabular SARSA on an explicit finite MDP
#'
#' The boxed algorithm of Sutton and Barto (2018), Section 6.4, p. 130:
#' after taking A in S and observing R and the next state, the next
#' action is chosen epsilon-greedily first and the update uses the
#' quintuple (S, A, R, S2, A2):
#' \eqn{Q(S,A) \leftarrow Q(S,A) + \alpha \[R + \gamma Q(S',A') - Q(S,A)\]}
#' The name and algorithm come from the modified connectionist
#' Q-learning of Rummery and Niranjan (1994).  Q(terminal, .) = 0.
#' Determinism conventions match the Python arm bit-exactly: greedy
#' ties to the lowest action index; the epsilon-greedy draw consumes
#' one uniform for the epsilon test plus one for the random action when
#' exploring; the next state consumes one uniform by inverse CDF on the
#' transition row; the initial action of an episode is drawn before the
#' step loop, exactly as in the source box.
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
#'   Learning: An Introduction, 2nd ed., MIT Press, Section 6.4, boxed
#'   algorithm p. 130.  Local source:
#'   fetched-wave3/sutton-barto-2018-reinforcement-learning-2nd-ed.pdf.
#'   Rummery, G. A. and Niranjan, M. (1994). On-line Q-learning using
#'   connectionist systems. Technical Report CUED/F-INFENG/TR 166,
#'   Cambridge University Engineering Department.  Local source:
#'   fetched-wave3/rummery-niranjan-1994-sarsa-tr166.pdf.
#' @examples
#' P <- list(matrix(c(0, 1, 0, 1), 2, byrow = TRUE))
#' R <- matrix(c(1, 0), 2)
#' Sarsa(P, R, 0.9, alpha = 0.5, epsilon = 0, n_episodes = 3,
#'       terminal = 1L)$estimate
#' @export
Sarsa <- function(P, R, gamma, alpha = 0.1, epsilon = 0.1,
                  n_episodes = 100L, start = 0L, terminal = integer(0),
                  max_steps = 1000L, seed = 0, Q0 = NULL) {
  ar <- .w505_mdp_args(P, R)
  Pm <- ar$P; R <- ar$R; S <- ar$S; A <- ar$A
  gamma <- as.numeric(gamma); alpha <- as.numeric(alpha)
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
  eps_greedy <- function(s) {
    u1 <- .ghc_unif(e, 1L)
    if (u1 < epsilon) {
      a <- as.integer(floor(.ghc_unif(e, 1L) * A))
      if (a >= A) a <- A - 1L
      return(a)
    }
    .w505_greedy(Q[s + 1L, ], A) - 1L
  }
  n_steps <- 0L
  for (ep in seq_len(n_episodes)) {
    s <- start
    if (s %in% term) next
    a <- eps_greedy(s)
    for (st in seq_len(max_steps)) {
      u3 <- .ghc_unif(e, 1L)
      s2 <- .w505_sample_row(Pm[[a + 1L]][s + 1L, ], S, u3) - 1L
      r <- R[s + 1L, a + 1L]
      if (s2 %in% term) {
        target <- r
        a2 <- 0L
      } else {
        a2 <- eps_greedy(s2)
        target <- r + gamma * Q[s2 + 1L, a2 + 1L]
      }
      Q[s + 1L, a + 1L] <- Q[s + 1L, a + 1L] +
        alpha * (target - Q[s + 1L, a + 1L])
      n_steps <- n_steps + 1L
      if (s2 %in% term) break
      s <- s2
      a <- a2
    }
  }
  pol <- numeric(S); V <- numeric(S)
  for (s in seq_len(S)) {
    b <- .w505_greedy(Q[s, ], A)
    pol[s] <- as.numeric(b - 1L)
    V[s] <- Q[s, b]
  }
  list(estimate = Q, policy = pol, v = V, n_steps = n_steps,
       n_episodes = as.integer(n_episodes),
       method = "Tabular SARSA, epsilon-greedy on-policy TD control")
}

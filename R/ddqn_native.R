# Tabular Double Q-learning.
# Source: van Hasselt, H. (2010), Double Q-learning, NIPS 23,
# 2613-2621 (Algorithm 1); Sutton, R. S. and Barto, A. G. (2018),
# Reinforcement Learning: An Introduction, 2nd ed., MIT Press, Sec.
# 6.7 and the boxed algorithm on p. 136.  Local source:
# fetched-wave3/sutton-barto-2018-reinforcement-learning-2nd-ed.pdf.
#
# Native implementation mirroring Python morie.fn.ddqn exactly: the
# behaviour policy is greedy in Q1 + Q2 (Sutton & Barto's p. 136 box),
# one extra uniform per step decides which table is updated, and the
# updated table's argmax is EVALUATED in the other table -- the
# decoupling that removes the maximisation bias.

#' Tabular Double Q-learning
#'
#' Maintains two independent action-value tables and, on each step,
#' updates one of them using the other for evaluation:
#' \eqn{Q_1(s,a) \leftarrow Q_1(s,a) + \alpha[r + \gamma
#' Q_2(s', \arg\max_{a'} Q_1(s',a')) - Q_1(s,a)]} (and symmetrically).
#' Because the table that selects the action is not the table that
#' evaluates it, the positive bias of the single \eqn{\max} in
#' \code{\link{morie_qlearn}} is removed (van Hasselt 2010).
#'
#' @inheritParams morie_qlearn
#' @return A list with \code{estimate} (the averaged (S, A) table),
#'   \code{q1}, \code{q2}, \code{policy}, \code{v}, \code{n_steps},
#'   \code{n_episodes} and \code{method}.
#' @references van Hasselt, H. (2010). Double Q-learning. Advances in
#'   Neural Information Processing Systems, 23, 2613-2621.
#' @export
morie_ddqn <- function(P, R, gamma, alpha = 0.1, epsilon = 0.1,
                       n_episodes = 100L, start = 0L, terminal = c(),
                       max_steps = 1000L, seed = 0) {
  z <- .mdp_args(P, R)
  Pm <- z$Pm
  R <- z$R
  S <- z$S
  A <- z$A
  gamma <- as.numeric(gamma)
  alpha <- as.numeric(alpha)
  epsilon <- as.numeric(epsilon)
  start <- as.integer(start)
  term <- .mor_rl_terminal(terminal)
  if (start < 0L || start >= S) stop("start out of range")
  e <- .ghc_rng(seed)
  Q1 <- matrix(0, S, A)
  Q2 <- matrix(0, S, A)
  n_steps <- 0L
  for (ep in seq_len(as.integer(n_episodes))) {
    s <- start + 1L
    for (k in seq_len(as.integer(max_steps))) {
      if (s %in% term) break
      u1 <- .ghc_unif(e, 1L)
      if (u1 < epsilon) {
        a <- as.integer(.ghc_unif(e, 1L) * A)
        if (a >= A) a <- A - 1L
        a <- a + 1L
      } else {
        a <- .mor_rl_greedy(Q1[s, ] + Q2[s, ], A)
      }
      u3 <- .ghc_unif(e, 1L)
      s2 <- .mor_rl_sample_row(Pm[[a]][s, ], S, u3)
      r <- R[s, a]
      coin <- .ghc_unif(e, 1L)
      if (coin < 0.5) {
        nxt <- 0
        if (!(s2 %in% term)) nxt <- Q2[s2, .mor_rl_greedy(Q1[s2, ], A)]
        Q1[s, a] <- Q1[s, a] + alpha * (r + gamma * nxt - Q1[s, a])
      } else {
        nxt <- 0
        if (!(s2 %in% term)) nxt <- Q1[s2, .mor_rl_greedy(Q2[s2, ], A)]
        Q2[s, a] <- Q2[s, a] + alpha * (r + gamma * nxt - Q2[s, a])
      }
      n_steps <- n_steps + 1L
      s <- s2
    }
  }
  Q <- 0.5 * (Q1 + Q2)
  o <- .mor_rl_out(Q, S, A)
  list(estimate = Q, q1 = Q1, q2 = Q2, policy = o$policy, v = o$v,
       n_steps = n_steps, n_episodes = as.integer(n_episodes),
       method = "Tabular Double Q-learning (decoupled selection and evaluation)")
}

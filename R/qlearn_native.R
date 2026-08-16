# Tabular Q-learning (off-policy TD control).
# Source: Sutton, R. S. and Barto, A. G. (2018), Reinforcement
# Learning: An Introduction, 2nd ed., MIT Press, Sec. 6.5, Eq. (6.8)
# and the boxed algorithm on p. 131; Watkins, C. J. C. H. and Dayan,
# P. (1992), Q-learning, Machine Learning 8, 279-292.  Local source:
# fetched-wave3/sutton-barto-2018-reinforcement-learning-2nd-ed.pdf.
#
# Native implementation mirroring Python morie.fn.qlearn exactly: the
# same three uniforms per step in the same order (epsilon coin, random
# action, transition draw), lowest-index greedy tie-breaking, and the
# inverse-CDF transition draw of _array_core Generator.choice.

# argmax with the lowest index winning ties (Python _greedy)
#' Argmax with the lowest index winning ties (Python _greedy)
#'
#' A step of the qlearn_native implementation. Called by \code{.mor_rl_eps}, \code{.mor_rl_out}, \code{morie_ddqn} and 1 others in the module.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param Qs A vector; indexed elementwise.
#' @param A See Usage.
#' @return The value of \code{b}, as built in the body.
#' @export
.mor_rl_greedy <- function(Qs, A) {
  b <- 1L
  for (a in seq.int(2L, A)) if (Qs[a] > Qs[b]) b <- a
  b
}

# inverse-CDF draw on a stochastic row from one uniform (Python
# _sample_row); returns a 1-based state index
#' Inverse-CDF draw on a stochastic row from one uniform (Python
#'
#' _sample_row); returns a 1-based state index
#'
#' @param row A vector; indexed elementwise.
#' @param S A count; the body uses it as \code{seq_len(...)}.
#' @param u See Usage.
#' @return The value of \code{S}, as built in the body.
#' @export
.mor_rl_sample_row <- function(row, S, u) {
  cc <- 0
  for (s2 in seq_len(S)) {
    cc <- cc + row[s2]
    if (u <= cc) return(s2)
  }
  S
}

# epsilon-greedy action (1-based) consuming one or two uniforms
#' Epsilon-greedy action (1-based) consuming one or two uniforms
#'
#' A step of the qlearn_native implementation. Called by \code{morie_qlearn}, \code{morie_sarsa}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param e Passed to \code{.ghc_unif}.
#' @param Qs Passed to \code{.mor_rl_greedy}.
#' @param A Numeric; combined arithmetically in the body.
#' @param epsilon See Usage.
#' @return The value of \code{.mor_rl_greedy}.
#' @export
.mor_rl_eps <- function(e, Qs, A, epsilon) {
  u1 <- .ghc_unif(e, 1L)
  if (u1 < epsilon) {
    a <- as.integer(.ghc_unif(e, 1L) * A)
    if (a >= A) a <- A - 1L
    return(a + 1L)
  }
  .mor_rl_greedy(Qs, A)
}

#' .mor_rl_terminal
#'
#' A step of the qlearn_native implementation. Called by \code{morie_ddqn}, \code{morie_qlearn}, \code{morie_sarsa}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param terminal See Usage.
#' @return A numeric value.
#' @export
.mor_rl_terminal <- function(terminal) as.integer(terminal) + 1L

#' .mor_rl_out
#'
#' A step of the qlearn_native implementation. Called by \code{morie_ddqn}, \code{morie_qlearn}, \code{morie_sarsa}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param Q A matrix; indexed by row and column.
#' @param S A count; the body uses it as \code{seq_len(...)}.
#' @param A Passed to \code{.mor_rl_greedy}.
#' @return A list with \code{policy}, \code{v}.
#' @export
.mor_rl_out <- function(Q, S, A) {
  pol <- numeric(S); V <- numeric(S)
  for (s in seq_len(S)) {
    b <- .mor_rl_greedy(Q[s, ], A)
    pol[s] <- b - 1L
    V[s] <- Q[s, b]
  }
  list(policy = pol, v = V)
}

#' Tabular Q-learning
#'
#' Off-policy temporal-difference control: behaves epsilon-greedily but
#' bootstraps from the greedy action, i.e. the update of Sutton and
#' Barto (2018), Eq. (6.8),
#' \eqn{Q(s,a) \leftarrow Q(s,a) + \alpha[r + \gamma \max_{a'}
#' Q(s',a') - Q(s,a)]}.
#'
#' @param P List of A transition matrices, each (S, S) with rows
#'   summing to 1.
#' @param R Reward, (S, A) matrix or a length-A list of (S, S)
#'   matrices; see \code{\link{morie_mdpval}}.
#' @param gamma Discount factor.
#' @param alpha Step size.
#' @param epsilon Exploration probability.
#' @param n_episodes Number of episodes.
#' @param start Start state, 0-based.
#' @param terminal Terminal states, 0-based.
#' @param max_steps Step cap per episode.
#' @param seed Seed for the reproducible generator shared with the
#'   Python arm.
#' @param Q0 Optional (S, A) initial action values; terminal rows are
#'   forced to zero.
#' @return A list with \code{estimate} (the (S, A) Q table),
#'   \code{policy} (0-based greedy actions), \code{v},
#'   \code{n_steps}, \code{n_episodes} and \code{method}.
#' @references Watkins, C. J. C. H. and Dayan, P. (1992). Q-learning.
#'   Machine Learning, 8, 279-292.
#' @export
morie_qlearn <- function(P, R, gamma, alpha = 0.1, epsilon = 0.1,
                         n_episodes = 100L, start = 0L, terminal = c(),
                         max_steps = 1000L, seed = 0, Q0 = NULL) {
  z <- .mdp_args(P, R); Pm <- z$Pm; R <- z$R; S <- z$S; A <- z$A
  gamma <- as.numeric(gamma); alpha <- as.numeric(alpha)
  epsilon <- as.numeric(epsilon)
  start <- as.integer(start)
  term <- .mor_rl_terminal(terminal)
  if (start < 0L || start >= S) stop("start out of range")
  e <- .ghc_rng(seed)
  Q <- matrix(0, S, A)
  if (!is.null(Q0)) {
    Q0 <- as.matrix(Q0)
    for (s in seq_len(S)) for (a in seq_len(A))
      Q[s, a] <- if (s %in% term) 0 else Q0[s, a]
  }
  n_steps <- 0L
  for (ep in seq_len(as.integer(n_episodes))) {
    s <- start + 1L
    for (k in seq_len(as.integer(max_steps))) {
      if (s %in% term) break
      a <- .mor_rl_eps(e, Q[s, ], A, epsilon)
      u3 <- .ghc_unif(e, 1L)
      s2 <- .mor_rl_sample_row(Pm[[a]][s, ], S, u3)
      r <- R[s, a]
      nxt <- 0
      if (!(s2 %in% term)) nxt <- Q[s2, .mor_rl_greedy(Q[s2, ], A)]
      Q[s, a] <- Q[s, a] + alpha * (r + gamma * nxt - Q[s, a])
      n_steps <- n_steps + 1L
      s <- s2
    }
  }
  o <- .mor_rl_out(Q, S, A)
  list(estimate = Q, policy = o$policy, v = o$v,
       n_steps = n_steps, n_episodes = as.integer(n_episodes),
       method = "Tabular Q-learning, epsilon-greedy off-policy TD control")
}

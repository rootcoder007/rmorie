# Tabular SARSA (on-policy TD control).
# Source: Sutton, R. S. and Barto, A. G. (2018), Reinforcement
# Learning: An Introduction, 2nd ed., MIT Press, Sec. 6.4, Eq. (6.7)
# and the boxed algorithm on p. 130; Rummery, G. A. and Niranjan, M.
# (1994), On-line Q-learning using connectionist systems, CUED
# technical report CUED/F-INFENG/TR 166.  Local source:
# fetched-wave3/sutton-barto-2018-reinforcement-learning-2nd-ed.pdf.
#
# Native implementation mirroring Python morie.fn.sarsa exactly,
# including the on-policy quintuple (s, a, r, s', a') ordering of the
# random draws: the next action is chosen (and its uniforms consumed)
# only when s' is non-terminal.

#' Tabular SARSA
#'
#' On-policy temporal-difference control: the bootstrap uses the action
#' the epsilon-greedy policy actually takes next, giving Sutton and
#' Barto (2018), Eq. (6.7),
#' \eqn{Q(s,a) \leftarrow Q(s,a) + \alpha[r + \gamma Q(s',a') -
#' Q(s,a)]}.  Contrast \code{\link{morie_qlearn}}, which bootstraps
#' off-policy from the greedy action.
#'
#' @inheritParams morie_qlearn
#' @return A list with \code{estimate} (the (S, A) Q table),
#'   \code{policy}, \code{v}, \code{n_steps}, \code{n_episodes} and
#'   \code{method}.
#' @references Rummery, G. A. and Niranjan, M. (1994). On-line
#'   Q-learning using connectionist systems. Cambridge University
#'   Engineering Department, CUED/F-INFENG/TR 166.
#' @export
morie_sarsa <- function(P, R, gamma, alpha = 0.1, epsilon = 0.1,
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
    if (s %in% term) next
    a <- .mor_rl_eps(e, Q[s, ], A, epsilon)
    for (k in seq_len(as.integer(max_steps))) {
      u3 <- .ghc_unif(e, 1L)
      s2 <- .mor_rl_sample_row(Pm[[a]][s, ], S, u3)
      r <- R[s, a]
      if (s2 %in% term) {
        target <- r
        a2 <- 1L
      } else {
        a2 <- .mor_rl_eps(e, Q[s2, ], A, epsilon)
        target <- r + gamma * Q[s2, a2]
      }
      Q[s, a] <- Q[s, a] + alpha * (target - Q[s, a])
      n_steps <- n_steps + 1L
      if (s2 %in% term) break
      s <- s2
      a <- a2
    }
  }
  o <- .mor_rl_out(Q, S, A)
  list(estimate = Q, policy = o$policy, v = o$v,
       n_steps = n_steps, n_episodes = as.integer(n_episodes),
       method = "Tabular SARSA, epsilon-greedy on-policy TD control")
}

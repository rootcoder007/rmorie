# Finite-MDP value iteration and policy iteration.
# Source: Sutton, R. S. and Barto, A. G. (2018), Reinforcement
# Learning: An Introduction, 2nd ed., MIT Press, Sec. 4.4 boxed
# algorithm p. 83 (value iteration, their Eq. 4.10) and Sec. 4.3
# boxed algorithm p. 80 (policy iteration); Howard (1960); Puterman
# (1994) Sec. 6.3.  Local source: fetched-wave3/
# sutton-barto-2018-reinforcement-learning-2nd-ed.pdf.
# Mirrors Python morie.fn.mdpval / morie.fn.mdppol exactly: same
# in-place sweep order s = 1..S, same lowest-index tie-breaking,
# same 1e-12 policy-stability slack.

.mdp_args <- function(P, R) {
  Pm <- lapply(P, function(Pa) as.matrix(Pa))
  A <- length(Pm)
  if (A < 1 || nrow(Pm[[1]]) != ncol(Pm[[1]]))
    stop("P must be a sequence of square (S, S) matrices")
  S <- nrow(Pm[[1]])
  for (a in seq_len(A)) {
    if (!all(dim(Pm[[a]]) == c(S, S))) stop(sprintf("P[%d] is not (S, S)", a))
    for (s in seq_len(S))
      if (abs(sum(Pm[[a]][s, ]) - 1) > 1e-8)
        stop(sprintf("P[%d] row %d does not sum to 1", a, s))
  }
  if (is.list(R) && length(R) == A && is.matrix(as.matrix(R[[1]]))) {
    Rsa <- matrix(0, S, A)
    for (a in seq_len(A)) {
      Ra <- as.matrix(R[[a]])
      for (s in seq_len(S)) Rsa[s, a] <- sum(Pm[[a]][s, ] * Ra[s, ])
    }
    R <- Rsa
  } else {
    R <- as.matrix(R)
    if (!all(dim(R) == c(S, A)))
      stop("R must be (S, A) or a length-A list of (S, S)")
  }
  list(Pm = Pm, R = R, S = S, A = A)
}

#' Value iteration for a finite Markov decision process
#'
#' Repeats the Bellman optimality sweep
#' V(s) <- max_a sum_s2 P(s2|s,a) [ r(s,a) + gamma V(s2) ] until the
#' largest change in a sweep falls below \code{tol}, then returns the
#' deterministic greedy policy with ties broken to the lowest action
#' index (Sutton & Barto 2018, Sec. 4.4 boxed algorithm, p. 83).
#'
#' @param P List of A transition matrices, each (S, S), rows summing
#'   to 1; \code{P[[a]][s, s2] = P(s2 | s, a)}.
#' @param R Reward: (S, A) matrix of r(s, a), or a list of A (S, S)
#'   per-transition reward matrices averaged under P.
#' @param gamma Discount factor.
#' @param tol Sweep-change threshold.
#' @param max_iter Maximum sweeps.
#' @param V0 Optional initial value vector.
#' @return A list with elements \code{estimate} (V), \code{policy}
#'   (0-based actions), \code{q}, \code{n_iter}, \code{delta},
#'   \code{converged}, \code{method}.
#' @references Sutton, R. S. and Barto, A. G. (2018). Reinforcement
#'   Learning: An Introduction, 2nd ed. MIT Press, Section 4.4.
#' @export
morie_mdpval <- function(P, R, gamma, tol = 1e-10, max_iter = 100000,
                         V0 = NULL) {
  z <- .mdp_args(P, R); Pm <- z$Pm; R <- z$R; S <- z$S; A <- z$A
  gamma <- as.numeric(gamma); tol <- as.numeric(tol)
  V <- rep(0, S)
  if (!is.null(V0)) V <- as.numeric(V0)[seq_len(S)]
  delta <- Inf; it <- 0L
  while (it < as.integer(max_iter)) {
    it <- it + 1L
    delta <- 0
    for (s in seq_len(S)) {
      v <- V[s]
      best <- -Inf
      for (a in seq_len(A)) {
        q <- R[s, a] + gamma * sum(Pm[[a]][s, ] * V)
        if (q > best) best <- q
      }
      V[s] <- best
      d <- abs(v - best)
      if (d > delta) delta <- d
    }
    if (delta < tol) break
  }
  Q <- matrix(0, S, A); pol <- numeric(S)
  for (s in seq_len(S)) {
    for (a in seq_len(A))
      Q[s, a] <- R[s, a] + gamma * sum(Pm[[a]][s, ] * V)
    b <- 1L
    for (a in seq_len(A)) if (a > 1L && Q[s, a] > Q[s, b]) b <- a
    pol[s] <- b - 1L
  }
  list(estimate = V, policy = pol, q = Q, n_iter = it, delta = delta,
       converged = delta < tol,
       method = "Value iteration (Bellman optimality sweeps) for a finite MDP")
}

#' Howard policy iteration for a finite Markov decision process
#'
#' Alternates iterative policy evaluation (in-place sweeps until the
#' largest change is below \code{tol}) with greedy policy
#' improvement, stopping when the policy is stable.  Ties go to the
#' lowest action index and a state counts as stable when the
#' incumbent action value is within 1e-12 of the maximum (Sutton &
#' Barto 2018, Sec. 4.3 boxed algorithm, p. 80; Howard 1960).
#'
#' @param P List of A transition matrices, each (S, S).
#' @param R Reward: (S, A) matrix, or list of A (S, S) matrices.
#' @param gamma Discount factor.
#' @param tol Policy-evaluation threshold.
#' @param max_eval Cap on evaluation sweeps per round.
#' @param max_improve Cap on improvement rounds.
#' @param pi0 Optional initial deterministic policy (0-based).
#' @return A list with elements \code{estimate}, \code{policy},
#'   \code{q}, \code{n_improve}, \code{n_eval}, \code{policy_stable},
#'   \code{method}.
#' @references Sutton, R. S. and Barto, A. G. (2018). Reinforcement
#'   Learning: An Introduction, 2nd ed. MIT Press, Section 4.3.
#'   Howard, R. A. (1960). Dynamic Programming and Markov Processes.
#' @export

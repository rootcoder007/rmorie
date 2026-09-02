# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Policy iteration for a finite MDP (Mdppol). Bit-identical mirror of
# src/morie/fn/mdppol.py.

#' Policy iteration for a finite MDP
#'
#' Alternates iterative policy evaluation, sweeps of
#' \eqn{V(s) \leftarrow \sum_{s'} P(s'|s,\pi(s)) \[r + \gamma V(s')\]}
#' until the largest change is below \code{tol}, with greedy policy
#' improvement, stopping when the policy is stable.  This is the boxed
#' algorithm in Sutton and Barto (2018), Section 4.3, p. 80; policy
#' iteration is due to Howard (1960).  Ties break to the lowest action
#' index and a state counts as stable when the incumbent action value
#' is within 1e-12 of the maximum, which removes the tie-flipping
#' non-termination of the literal box, per their Exercise 4.4.
#'
#' @param P List of A transition matrices, each (S, S) row-stochastic.
#' @param R Matrix (S, A) of expected rewards, or a list of A (S, S)
#'   per-transition reward matrices averaged under P.
#' @param gamma Discount factor in 0 to 1.
#' @param tol Policy-evaluation sweep threshold.
#' @param max_eval Cap on evaluation sweeps per improvement round.
#' @param max_improve Cap on improvement rounds.
#' @param pi0 Optional initial 0-based deterministic policy, length S.
#' @return List with \code{estimate} (values of the final policy),
#'   \code{policy} (0-based actions), \code{q}, \code{n_improve},
#'   \code{n_eval}, \code{policy_stable}, \code{method}.
#' @references Sutton, R. S. and Barto, A. G. (2018). Reinforcement
#'   Learning: An Introduction, 2nd ed., MIT Press, Section 4.3, boxed
#'   algorithm p. 80.  Local source:
#'   fetched-wave3/sutton-barto-2018-reinforcement-learning-2nd-ed.pdf.
#'   Howard, R. A. (1960). Dynamic Programming and Markov Processes,
#'   MIT Press.
#' @examples
#' P <- list(matrix(c(1, 0, 0, 1), 2, byrow = TRUE),
#'           matrix(c(0, 1, 0, 1), 2, byrow = TRUE))
#' R <- matrix(c(1, 0, 0, 0), 2, byrow = TRUE)
#' Mdppol(P, R, 0.5)$policy
#' @export
Mdppol <- function(P, R, gamma, tol = 1e-12, max_eval = 100000L,
                   max_improve = 1000L, pi0 = NULL) {
  ar <- .w505_mdp_args(P, R)
  Pm <- ar$P
  R <- ar$R
  S <- ar$S
  A <- ar$A
  gamma <- as.numeric(gamma)
  tol <- as.numeric(tol)
  pol <- integer(S)
  if (!is.null(pi0)) {
    for (s in seq_len(S)) {
      a <- as.integer(pi0[s])
      if (a < 0L || a >= A) stop("pi0 out of range", call. = FALSE)
      pol[s] <- a
    }
  }
  V <- numeric(S)
  n_eval <- 0L
  stable <- FALSE
  rounds <- 0L
  for (rounds in seq_len(max_improve)) {
    for (k in seq_len(max_eval)) {
      n_eval <- n_eval + 1L
      delta <- 0
      for (s in seq_len(S)) {
        v <- V[s]
        a <- pol[s] + 1L
        Vs <- R[s, a] + gamma * sum(Pm[[a]][s, ] * V)
        V[s] <- Vs
        d <- abs(v - Vs)
        if (d > delta) delta <- d
      }
      if (delta < tol) break
    }
    stable <- TRUE
    for (s in seq_len(S)) {
      old <- pol[s] + 1L
      qs <- numeric(A)
      for (a in seq_len(A)) qs[a] <- R[s, a] + gamma * sum(Pm[[a]][s, ] * V)
      b <- 1L
      if (A > 1L) for (a in seq(2L, A)) if (qs[a] > qs[b]) b <- a
      if (qs[b] > qs[old] + 1e-12) {
        pol[s] <- b - 1L
        stable <- FALSE
      }
    }
    if (stable) break
  }
  Q <- matrix(0, S, A)
  for (s in seq_len(S)) {
    for (a in seq_len(A)) Q[s, a] <- R[s, a] + gamma * sum(Pm[[a]][s, ] * V)
  }
  list(estimate = V, policy = as.numeric(pol), q = Q, n_improve = rounds,
       n_eval = n_eval, policy_stable = stable,
       method = "Policy iteration (iterative policy evaluation + greedy improvement)")
}

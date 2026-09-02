# Soft policy iteration: the exact tabular core of soft actor-critic.
# Source: Haarnoja, T., Zhou, A., Abbeel, P. and Levine, S. (2018),
# Soft actor-critic: off-policy maximum entropy deep reinforcement
# learning with a stochastic actor, ICML 2018, PMLR 80, 1861-1870,
# Sec. 4.1 (soft policy evaluation, their eqs (2)-(3), soft policy
# improvement, their eq (4), and Theorem 1 / Lemma 2); Ziebart, M. D.
# (2010) for the maximum-entropy objective.
#
# Native implementation mirroring Python morie.fn.sacc exactly: same
# sweep order, same max-subtracted softmax, same tol on both the
# evaluation residual and the policy movement.

#' Soft policy iteration (tabular soft actor-critic core)
#'
#' Alternates soft policy evaluation, in which the soft state value is
#' \eqn{V(s) = \sum_a \pi(a|s)[Q(s,a) - \tau \log \pi(a|s)]} and the
#' Bellman backup is \eqn{Q(s,a) = r(s,a) + \gamma \sum_{s'}
#' P(s'|s,a) V(s')}, with soft policy improvement, in which the policy
#' is set to the softmax \eqn{\pi(\cdot|s) \propto \exp(Q(s,\cdot)/
#' \tau)} (Haarnoja et al. 2018, Sec. 4.1, eqs (2)-(4)).  Theorem 1 of
#' that paper gives convergence to the optimal maximum-entropy policy
#' in the tabular case computed here.
#'
#' @param P List of A transition matrices, each (S, S).
#' @param R Reward, (S, A) matrix or length-A list of (S, S) matrices.
#' @param gamma Discount factor.
#' @param temp Entropy temperature \eqn{\tau}; must be positive.  As
#'   \eqn{\tau \to 0} the policy tends to the greedy one of
#'   \code{\link{morie_mdpval}}.
#' @param tol Convergence threshold, applied to both the evaluation
#'   residual and the largest policy movement.
#' @param max_eval Maximum evaluation sweeps per improvement round.
#' @param max_improve Maximum improvement rounds.
#' @return A list with \code{estimate} (soft values V), \code{policy}
#'   (the (S, A) stochastic policy), \code{q}, \code{entropy},
#'   \code{n_improve}, \code{n_eval}, \code{converged}, \code{method}.
#' @references Haarnoja, T., Zhou, A., Abbeel, P. and Levine, S.
#'   (2018). Soft actor-critic. ICML 2018, PMLR 80, 1861-1870.
#' @export
#' @examples
#' morie_sacc(P = 1, R = 5L, gamma = 0.5)
morie_sacc <- function(P, R, gamma, temp = 1, tol = 1e-12,
                       max_eval = 100000L, max_improve = 1000L) {
  z <- .mdp_args(P, R); Pm <- z$Pm; R <- z$R; S <- z$S; A <- z$A
  gamma <- as.numeric(gamma); temp <- as.numeric(temp)
  tol <- as.numeric(tol)
  if (temp <= 0) stop("temp must be positive")
  Q <- matrix(0, S, A)
  pi <- matrix(1 / A, S, A)
  logpi <- matrix(-log(A), S, A)
  n_eval <- 0L
  converged <- FALSE
  rounds <- 0L
  for (rounds in seq_len(as.integer(max_improve))) {
    for (it in seq_len(as.integer(max_eval))) {
      n_eval <- n_eval + 1L
      V <- numeric(S)
      for (s in seq_len(S)) {
        acc <- 0
        for (a in seq_len(A)) acc <- acc + pi[s, a] * (Q[s, a] - temp * logpi[s, a])
        V[s] <- acc
      }
      delta <- 0
      for (s in seq_len(S)) {
        for (a in seq_len(A)) {
          q <- R[s, a] + gamma * sum(Pm[[a]][s, ] * V)
          d <- abs(q - Q[s, a])
          if (d > delta) delta <- d
          Q[s, a] <- q
        }
      }
      if (delta < tol) break
    }
    moved <- 0
    for (s in seq_len(S)) {
      m <- Q[s, 1L]
      for (a in seq_len(A)) if (Q[s, a] > m) m <- Q[s, a]
      zsum <- 0
      ex <- numeric(A)
      for (a in seq_len(A)) {
        ex[a] <- exp((Q[s, a] - m) / temp)
        zsum <- zsum + ex[a]
      }
      for (a in seq_len(A)) {
        p <- ex[a] / zsum
        d <- abs(p - pi[s, a])
        if (d > moved) moved <- d
        pi[s, a] <- p
        logpi[s, a] <- if (p > 0) log(p) else -1e300
      }
    }
    if (moved < tol) { converged <- TRUE; break }
  }
  V <- numeric(S); H <- numeric(S)
  for (s in seq_len(S)) {
    acc <- 0; h <- 0
    for (a in seq_len(A)) {
      acc <- acc + pi[s, a] * (Q[s, a] - temp * logpi[s, a])
      if (pi[s, a] > 0) h <- h - pi[s, a] * logpi[s, a]
    }
    V[s] <- acc
    H[s] <- h
  }
  list(estimate = V, policy = pi, q = Q, entropy = H,
       n_improve = rounds, n_eval = n_eval, converged = converged,
       method = "Soft policy iteration (maximum-entropy RL, exact tabular SAC core)")
}

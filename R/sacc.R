# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Soft policy iteration, exact tabular SAC core (Sacc). Bit-identical
# mirror of src/morie/fn/sacc.py; uses .w505_mdp_args from mdpval.R.

#' Soft policy iteration on an explicit finite MDP
#'
#' The exact tabular algorithm underlying soft actor-critic (Haarnoja
#' et al. 2018, Section 4.1): soft policy evaluation applies the soft
#' Bellman backup of eqs. (2)-(3), Q <- r + gamma E[V], V =
#' E_pi[Q - temp log pi]; soft policy improvement projects onto
#' softmax(Q/temp) (eq. 4), the exact minimizer for the tabular class,
#' with monotone improvement and convergence per Lemmas 1-2 and
#' Theorem 1. At the fixed point V(s) = temp logsumexp(Q(s,.)/temp)
#' (the test anchor). temp is the entropy weight alpha of eq. (1).
#'
#' @param P List of A transition matrices, each (S x S).
#' @param R Reward matrix (S x A) or list of A (S x S) matrices.
#' @param gamma Discount factor in [0, 1).
#' @param temp Entropy temperature alpha > 0.
#' @param tol Convergence threshold.
#' @param max_eval,max_improve Iteration caps.
#' @return List with \code{estimate} (soft V*), \code{policy},
#'   \code{q}, \code{entropy}, \code{n_improve}, \code{n_eval},
#'   \code{converged}, \code{method}.
#' @references Haarnoja, T., Zhou, A., Abbeel, P. and Levine, S.
#'   (2018), ICML 2018, arXiv:1801.01290, Section 4.1, eqs. (2)-(4),
#'   Lemmas 1-2, Theorem 1. Local source:
#'   fetched-wave3/haarnoja-etal-2018-sac-arxiv1801.01290.pdf.
#' @export
Sacc <- function(P, R, gamma, temp = 1, tol = 1e-12, max_eval = 100000L,
                 max_improve = 1000L) {
  ar <- .w505_mdp_args(P, R)
  Pm <- ar$P; Rm <- ar$R; S <- ar$S; A <- ar$A
  gamma <- as.numeric(gamma)[1]
  temp <- as.numeric(temp)[1]
  tol <- as.numeric(tol)[1]
  if (temp <= 0) stop("temp must be positive", call. = FALSE)
  Q <- matrix(0, S, A)
  pi_ <- matrix(1 / A, S, A)
  logpi <- matrix(-log(A), S, A)
  n_eval <- 0L
  converged <- FALSE
  rounds <- 0L
  for (rounds in seq_len(as.integer(max_improve))) {
    for (it in seq_len(as.integer(max_eval))) {
      n_eval <- n_eval + 1L
      V <- numeric(S)
      for (s in seq_len(S)) {
        V[s] <- sum(pi_[s, ] * (Q[s, ] - temp * logpi[s, ]))
      }
      delta <- 0
      for (s in seq_len(S)) {
        for (a in seq_len(A)) {
          q <- Rm[s, a] + gamma * sum(Pm[[a]][s, ] * V)
          d <- abs(q - Q[s, a])
          if (d > delta) delta <- d
          Q[s, a] <- q
        }
      }
      if (delta < tol) break
    }
    moved <- 0
    for (s in seq_len(S)) {
      m <- max(Q[s, ])
      ex <- exp((Q[s, ] - m) / temp)
      z <- sum(ex)
      for (a in seq_len(A)) {
        p <- ex[a] / z
        d <- abs(p - pi_[s, a])
        if (d > moved) moved <- d
        pi_[s, a] <- p
        logpi[s, a] <- if (p > 0) log(p) else -1e300
      }
    }
    if (moved < tol) { converged <- TRUE; break }
  }
  V <- numeric(S); H <- numeric(S)
  for (s in seq_len(S)) {
    V[s] <- sum(pi_[s, ] * (Q[s, ] - temp * logpi[s, ]))
    H[s] <- -sum(ifelse(pi_[s, ] > 0, pi_[s, ] * logpi[s, ], 0))
  }
  list(estimate = V, policy = pi_, q = Q, entropy = H,
       n_improve = rounds, n_eval = n_eval, converged = converged,
       method = "Soft policy iteration (maximum-entropy RL, exact tabular SAC core)")
}

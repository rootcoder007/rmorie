# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Value iteration for a finite MDP (Mdpval). Bit-identical mirror of
# src/morie/fn/mdpval.py.

# Validate/normalize MDP arguments shared by Mdpval/Mdppol/Qlearn/Sarsa:
# P is a list of A row-stochastic (S, S) matrices, R is an (S, A) matrix
# of expected rewards or a list of A (S, S) per-transition rewards which
# are averaged under P.
#' @keywords internal
#' @noRd
.w505_mdp_args <- function(P, R) {
  Pm <- lapply(P, function(m) { m <- as.matrix(m); storage.mode(m) <- "double"; m })
  A <- length(Pm)
  if (A < 1L || nrow(Pm[[1]]) != ncol(Pm[[1]])) {
    stop("P must be a list of square (S, S) matrices", call. = FALSE)
  }
  S <- nrow(Pm[[1]])
  for (a in seq_len(A)) {
    if (!all(dim(Pm[[a]]) == c(S, S))) stop("P element is not (S, S)", call. = FALSE)
    for (s in seq_len(S)) {
      if (abs(sum(Pm[[a]][s, ]) - 1) > 1e-8) {
        stop(sprintf("P[[%d]] row %d does not sum to 1", a, s), call. = FALSE)
      }
    }
  }
  if (is.list(R) && length(R) == A) {
    Rsa <- matrix(0, S, A)
    for (a in seq_len(A)) {
      Ra <- as.matrix(R[[a]])
      for (s in seq_len(S)) Rsa[s, a] <- sum(Pm[[a]][s, ] * Ra[s, ])
    }
    R <- Rsa
  } else {
    R <- as.matrix(R); storage.mode(R) <- "double"
    if (!all(dim(R) == c(S, A))) stop("R must be (S, A) or a list of A (S, S)", call. = FALSE)
  }
  list(P = Pm, R = R, S = S, A = A)
}

#' Value iteration for a finite MDP
#'
#' Repeats the Bellman optimality sweep
#' \eqn{V(s) \leftarrow \max_a \sum_{s'} P(s'|s,a) \[r + \gamma V(s')\]}
#' until the largest change in a sweep drops below \code{tol}, then
#' outputs the deterministic greedy policy (ties to the lowest action
#' index).  This is the boxed algorithm in Sutton and Barto (2018),
#' Section 4.4, p. 83, their eq. 4.10; the method originates with
#' Bellman (1957) and is Puterman (1994), Section 6.3.
#'
#' @param P List of A transition matrices, each (S, S) row-stochastic.
#' @param R Matrix (S, A) of expected rewards, or a list of A (S, S)
#'   per-transition reward matrices averaged under P.
#' @param gamma Discount factor in 0 to 1.
#' @param tol Sweep-change threshold.
#' @param max_iter Cap on sweeps.
#' @param V0 Optional initial value function, length S.
#' @return List with \code{estimate} (optimal values), \code{policy}
#'   (0-based greedy actions), \code{q}, \code{n_iter}, \code{delta},
#'   \code{converged}, \code{method}.
#' @references Sutton, R. S. and Barto, A. G. (2018). Reinforcement
#'   Learning: An Introduction, 2nd ed., MIT Press, Section 4.4, boxed
#'   algorithm p. 83, eq. 4.10.  Local source:
#'   fetched-wave3/sutton-barto-2018-reinforcement-learning-2nd-ed.pdf.
#'   Bellman, R. (1957). Dynamic Programming, Princeton University
#'   Press.  Puterman, M. L. (1994). Markov Decision Processes, Wiley,
#'   Section 6.3.
#' @examples
#' P <- list(matrix(c(1, 0, 0, 1), 2, byrow = TRUE),
#'           matrix(c(0, 1, 0, 1), 2, byrow = TRUE))
#' R <- matrix(c(1, 0, 0, 0), 2, byrow = TRUE)
#' Mdpval(P, R, 0.5)$estimate
#' @export
Mdpval <- function(P, R, gamma, tol = 1e-10, max_iter = 100000L, V0 = NULL) {
  ar <- .w505_mdp_args(P, R)
  Pm <- ar$P; R <- ar$R; S <- ar$S; A <- ar$A
  gamma <- as.numeric(gamma); tol <- as.numeric(tol)
  V <- numeric(S)
  if (!is.null(V0)) for (s in seq_len(S)) V[s] <- as.numeric(V0[s])
  delta <- Inf; it <- 0L
  while (it < max_iter) {
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
  Q <- matrix(0, S, A)
  pol <- numeric(S)
  for (s in seq_len(S)) {
    for (a in seq_len(A)) Q[s, a] <- R[s, a] + gamma * sum(Pm[[a]][s, ] * V)
    b <- 1L
    if (A > 1L) for (a in seq(2L, A)) if (Q[s, a] > Q[s, b]) b <- a
    pol[s] <- as.numeric(b - 1L)
  }
  list(estimate = V, policy = pol, q = Q, n_iter = it, delta = delta,
       converged = delta < tol,
       method = "Value iteration (Bellman optimality sweeps) for a finite MDP")
}

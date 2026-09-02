# SPDX-License-Identifier: AGPL-3.0-or-later
#' Low-rank Sinkhorn
#'
#' Scetbon, Cuturi and Peyre (2021), Low-rank Sinkhorn factorization, ICML
#' 139, 9344-9354 (arXiv:2103.04737 -- FETCHED), restrict the plan to
#' Pi_(a,g,b) = \{P = Q diag(1/g) R' : Q in Pi_(a,g), R in Pi_(b,g)\}, a
#' product of two sub-couplings with a common right marginal g, so that
#' rank_+(P) <= r by construction.  The optimisation is mirror descent:
#' each outer step projects the linearisation back onto the two sets by
#' two Sinkhorn solves.
#'
#' Determinism: the paper initialises at random; here Q, R and g start at
#' the rank-one product with g uniform, which is feasible, canonical and
#' identical in both arms.
#'
#' @param a,b marginals.
#' @param C cost matrix.
#' @param rank the nonnegative rank r.
#' @param epsilon regularisation.
#' @param max_iter outer mirror-descent steps.
#' @param inner inner Sinkhorn iterations.
#' @param gamma mirror-descent step scaling.
#' @return list: U, V, T, g, estimate, cost, rank, method.
#' @keywords internal
#' @examples
#' Sinkhlowr(c(0.5, 0.5), c(0.5, 0.5), matrix(c(0, 1, 1, 0), 2, 2), 2,
#'           0.5, 3, 20)$cost
#' @export
Sinkhlowr <- function(a, b, C, rank = 2, epsilon = 0.1, max_iter = 20,
                      inner = 50, gamma = 1) {
  av <- .s03vec(a); bv <- .s03vec(b); Cm <- .s03mat(C)
  n <- length(av); m <- length(bv); r <- as.integer(rank)
  g <- rep(1 / r, r)
  Q <- matrix(0, n, r); R <- matrix(0, m, r)
  for (i in seq_len(n)) for (t in seq_len(r)) Q[i, t] <- av[i] * g[t]
  for (j in seq_len(m)) for (t in seq_len(r)) R[j, t] <- bv[j] * g[t]
  for (it in seq_len(as.integer(max_iter))) {
    CR <- matrix(0, n, r)
    for (i in seq_len(n)) for (t in seq_len(r)) {
      s <- 0
      for (j in seq_len(m)) s <- s + Cm[i, j] * R[j, t]
      CR[i, t] <- if (g[t] > 0) s / g[t] else 0
    }
    CQ <- matrix(0, m, r)
    for (j in seq_len(m)) for (t in seq_len(r)) {
      s <- 0
      for (i in seq_len(n)) s <- s + Cm[i, j] * Q[i, t]
      CQ[j, t] <- if (g[t] > 0) s / g[t] else 0
    }
    Q <- Sinkhlog(av, g, CR, as.numeric(epsilon) / as.numeric(gamma), inner)$T
    R <- Sinkhlog(bv, g, CQ, as.numeric(epsilon) / as.numeric(gamma), inner)$T
    ng <- numeric(r)
    for (t in seq_len(r)) {
      s <- 0
      for (i in seq_len(n)) s <- s + Q[i, t]
      ng[t] <- s
    }
    tot <- 0
    for (x in ng) tot <- tot + x
    g <- if (tot > 0) ng / tot else rep(1 / r, r)
  }
  T <- matrix(0, n, m)
  for (i in seq_len(n)) for (j in seq_len(m)) {
    s <- 0
    for (t in seq_len(r)) if (g[t] > 0) s <- s + Q[i, t] * R[j, t] / g[t]
    T[i, j] <- s
  }
  cost <- 0
  for (i in seq_len(n)) for (j in seq_len(m)) cost <- cost + T[i, j] * Cm[i, j]
  list(U = Q, V = R, T = T, g = g, estimate = cost, cost = cost, rank = r,
       method = "Low-rank Sinkhorn factorisation P = Q diag(1/g) R' (Scetbon et al. 2021)")
}

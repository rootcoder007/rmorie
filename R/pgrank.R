# SPDX-License-Identifier: AGPL-3.0-or-later
#' Stationary importance of a node under a random surfer
#'
#' The damping factor gives every node a floor so a dangling region
#' cannot absorb all the mass, and dangling nodes have their mass
#' redistributed uniformly instead of leaking -- the detail that decides
#' whether the vector sums to one.
#'
#' Determinism: fixed number of power iterations, no tolerance test.
#'
#' Formula: \code{PR(v) = (1 - d)/n + d sum_{u -> v} PR(u)/L(u)}.
#'
#' @param A Adjacency matrix; \code{A[i, j]} non-zero is a link i to j.
#' @param d Damping factor.
#' @param n_iter Power iterations.
#' @return List with \code{pr}, \code{estimate}, \code{top}, \code{n}.
#' @references Page, L., Brin, S., Motwani, R. & Winograd, T. (1999).
#'   Stanford InfoLab technical report 1999-66.
#' @export
Pgrank <- function(A, d = 0.85, n_iter = 100) {
  M <- as.matrix(A); n <- nrow(M)
  out <- rowSums(M)
  pr <- rep(1 / n, n)
  for (it in seq_len(as.integer(n_iter))) {
    dangle <- sum(pr[out == 0]) / n
    new <- numeric(n)
    for (j in seq_len(n)) {
      idx <- which(out > 0)
      s <- if (length(idx)) sum(pr[idx] * M[idx, j] / out[idx]) else 0
      new[j] <- (1 - d) / n + d * (s + dangle)
    }
    pr <- new
  }
  top <- which.max(pr)
  .t1_result(pr = pr, estimate = pr[top], top = top - 1L, n = n,
             method = "PageRank by fixed power iteration")
}

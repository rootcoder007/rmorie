# SPDX-License-Identifier: AGPL-3.0-or-later
#' Rewire a ring lattice and watch the diameter collapse
#'
#' A graph does not have to look random to behave randomly: rewiring a
#' percent or two of a regular lattice leaves the clustering intact while
#' the average path length falls to near its random-graph value.
#'
#' Determinism: edges visited in a fixed order, rewiring draws from the
#' shared Lehmer minstd stream, so a seed gives the same graph in both
#' arms.
#'
#' Formula: ring where each node joins its k nearest neighbours, then
#' rewire each edge with probability p, refusing self-loops and
#' duplicates.
#'
#' @param n Nodes.
#' @param k Even; nearest neighbours each node starts joined to.
#' @param p Rewiring probability.
#' @param seed Seed for the shared generator.
#' @return List with \code{A}, \code{estimate}, \code{n_rewired},
#'   \code{n_edges}, \code{n}.
#' @references Watts, D. J. & Strogatz, S. H. (1998). Nature 393:440-442.
#' @export
#' @examples
#' Watstro(n = 5L, k = 5L, p = 0.5)
Watstro <- function(n, k, p, seed = 1) {
  n <- as.integer(n); k <- as.integer(k)
  A <- matrix(0, n, n)
  for (i in seq_len(n)) for (j in seq_len(k %/% 2L)) {
    t_ <- ((i - 1L + j) %% n) + 1L
    A[i, t_] <- 1; A[t_, i] <- 1
  }
  g <- .t1_lcg(seed)
  rew <- 0L
  for (j in seq_len(k %/% 2L)) for (i in seq_len(n)) {
    t_ <- ((i - 1L + j) %% n) + 1L
    if (A[i, t_] == 0) next
    if (g$unif() < p) {
      cand <- as.integer(g$unif() * n) + 1L
      if (cand > n) cand <- n
      if (cand == i || A[i, cand] != 0) next
      A[i, t_] <- 0; A[t_, i] <- 0
      A[i, cand] <- 1; A[cand, i] <- 1
      rew <- rew + 1L
    }
  }
  edges <- sum(A) / 2
  .t1_result(A = A, estimate = 2 * edges / n, n_rewired = rew,
             n_edges = edges, n = n,
             method = "Watts-Strogatz small-world graph")
}

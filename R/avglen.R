# SPDX-License-Identifier: AGPL-3.0-or-later
#' Mean geodesic distance, and its harmonic counterpart.
#'
#' Formula: \eqn{L = (1/|R|)\sum_{(i,j) \in R} d(i,j)} over the ordered
#' pairs with \eqn{i \ne j} that are actually connected, and
#' \eqn{L_{harm}^{-1} = (1/(n(n-1)))\sum_{i \ne j} 1/d(i,j)} with
#' \eqn{1/d = 0} for unreachable pairs.  The arithmetic mean diverges
#' the moment the graph is disconnected, so unreachable pairs are
#' dropped from \eqn{L} and counted in \code{reachable} rather than
#' imputed; the harmonic mean is the standard repair and is always
#' finite.  Distances come from breadth-first search, so the graph is
#' unweighted -- a non-zero entry means an edge, not a length.
#'
#' @param G n by n adjacency matrix; non-zero means an edge.
#' @param directed Follow edge direction; otherwise symmetrise.
#' @return List with \code{estimate}, \code{harmonic}, \code{diameter},
#'   \code{reachable}, \code{pairs}, \code{n}, \code{method}.
#' @references Newman (2010), Networks: An Introduction, OUP, sec. 7.6, and the harmonic-mean repair for disconnected graphs.  The book is not in the local corpus and is not fetchable, so this is the standard published form rather than a quoted equation; the two conventions that are genuinely in dispute -- self-pairs and unreachable pairs -- are stated above and reported separately.
#' @export
#' @examples
#' Avgpathlen(matrix(c(0, 1, 0, 1, 0, 1, 0, 1, 0), 3, 3, byrow = TRUE))
Avgpathlen <- function(G, directed = FALSE) {
  A <- as.matrix(G); n <- nrow(A)
  if (n < 2 || ncol(A) != n) stop("G must be a square adjacency matrix with n >= 2")
  adj <- vector("list", n)
  for (i in seq_len(n)) {
    nb <- integer(0)
    for (j in seq_len(n)) {
      if (i == j) next
      if (A[i, j] != 0 || (!directed && A[j, i] != 0)) nb <- c(nb, j)
    }
    adj[[i]] <- nb
  }
  total <- 0; harm <- 0; reach <- 0L; diam <- 0L
  for (s in seq_len(n)) {
    dist <- rep(-1L, n); dist[s] <- 0L
    queue <- s; head <- 1L
    while (head <= length(queue)) {
      u <- queue[head]; head <- head + 1L
      for (v in adj[[u]]) if (dist[v] < 0L) { dist[v] <- dist[u] + 1L; queue <- c(queue, v) }
    }
    for (t in seq_len(n)) {
      if (t == s || dist[t] < 0L) next
      total <- total + dist[t]; harm <- harm + 1 / dist[t]; reach <- reach + 1L
      if (dist[t] > diam) diam <- dist[t]
    }
  }
  pairs <- n * (n - 1)
  .t4_result(estimate = if (reach > 0L) total / reach else NaN,
             harmonic = if (harm > 0) pairs / harm else Inf,
             diameter = as.integer(diam), reachable = as.integer(reach),
             pairs = as.integer(pairs), n = as.integer(n),
             method = "Mean geodesic distance")
}

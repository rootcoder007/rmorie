# SPDX-License-Identifier: AGPL-3.0-or-later
#' Closeness centrality.
#'
#' Formula: C_C(v) = (r - 1) / sum_{u reachable} d(v, u)
#'
#' @param A Adjacency matrix; non-zero means an edge.

#' @return List with ``closeness``, ``reachable``, ``total_distance``, ``n``.
#' @references Sabidussi (1966) for the sum-distance form and Freeman (1979), Centrality in social networks: conceptual clarification, Social Networks 1:215-239, for the (n-1)-normalised measure. Freeman's article is paywalled; the normalisation C(v) = (n-1)/sum_u d(v,u) is as restated in the centrality literature that cites him.
#' @export
Clocent <- function(A) {
  A <- as.matrix(A); n <- nrow(A); diag(A) <- 0; B <- A != 0
  clos <- numeric(n); reach <- integer(n); tot <- numeric(n)
  for (s in seq_len(n)) {
    dist <- rep(-1L, n); dist[s] <- 0L; q <- s; h <- 1L
    while (h <= length(q)) {
      v <- q[h]; h <- h + 1L
      w <- which(B[v, ] & dist < 0L)
      if (length(w)) { dist[w] <- dist[v] + 1L; q <- c(q, w) }
    }
    d <- dist[-s]; d <- d[d > 0]
    reach[s] <- length(d) + 1L; tot[s] <- sum(d)
    clos[s] <- if (length(d)) (length(d)) / sum(d) else NA_real_
  }
  .t1_result(closeness = clos, reachable = reach, total_distance = tot, n = n,
             method = "Closeness centrality")
}

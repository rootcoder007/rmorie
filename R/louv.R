# SPDX-License-Identifier: AGPL-3.0-or-later
#' Louvain community detection, all levels
#'
#' Alternates the local moving phase with an aggregation phase that contracts
#' each community to a node carrying self-loops of the internal weight, and
#' stops when a pass produces no move.  Deterministic: index order, ties to the
#' smallest label.  Source consulted: Blondel, Guillaume, Lambiotte and
#' Lefebvre (2008), JSTAT P10008.
#'
#' @param A symmetric adjacency (or weight) matrix.
#' @param max_levels cap on aggregation levels.
#' @return list: estimate, communities, n_communities, levels,
#'   modularity_by_level, n, method.
#' @keywords internal
#' @examples
#' louv(matrix(c(0,1,1,0), 2, 2))$n_communities
#' @export
louv <- function(A, max_levels = 20L) {
  a0 <- as.matrix(A); dimnames(a0) <- NULL
  n0 <- nrow(a0); m2 <- sum(a0)
  if (m2 <= 0) {
    return(list(estimate = 0, communities = seq_len(n0) - 1L, n_communities = n0,
                levels = 0L, modularity_by_level = numeric(0), n = n0,
                method = "Louvain community detection (Blondel et al. 2008)"))
  }
  labels <- seq_len(n0) - 1L
  a <- a0; hist <- numeric(0); levels <- 0L
  for (lv in seq_len(as.integer(max_levels))) {
    n <- nrow(a)
    ph <- .k02phase1(a, n, m2)
    comm <- .k02relabel(ph$comm)
    if (length(unique(comm)) == n) break
    labels <- comm[labels + 1L]
    hist <- c(hist, k02mod(a0, labels))
    levels <- levels + 1L
    nc <- length(unique(comm))
    agg <- matrix(0, nc, nc)
    for (i in seq_len(n)) for (j in seq_len(n))
      agg[comm[i] + 1L, comm[j] + 1L] <- agg[comm[i] + 1L, comm[j] + 1L] + a[i, j]
    a <- agg
  }
  list(estimate = k02mod(a0, labels), communities = .k02relabel(labels),
       n_communities = length(unique(labels)), levels = levels,
       modularity_by_level = hist, n = n0,
       method = "Louvain community detection (Blondel, Guillaume, Lambiotte & Lefebvre 2008)")
}

# CANONICAL TEST
# A <- matrix(0,6,6); E <- rbind(c(1,2),c(1,3),c(2,3),c(3,4),c(4,5),c(4,6),c(5,6))
# for (i in 1:nrow(E)) { A[E[i,1],E[i,2]] <- 1; A[E[i,2],E[i,1]] <- 1 }
# stopifnot(abs(louv(A)$estimate - 0.357142857142857) < 1e-12)

#' @rdname louv
#' @keywords internal
#' @export
morie_louv <- louv

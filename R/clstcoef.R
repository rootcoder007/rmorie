# SPDX-License-Identifier: AGPL-3.0-or-later
#' Fraction of a node neighbours that are themselves connected
#'
#' Real networks have far more triangles than degree alone predicts. A
#' node of degree below two has no pair of neighbours to close, so it has
#' no defined coefficient; those nodes are excluded from the average
#' rather than counted as zero, which stops the average drifting down
#' with every leaf added.
#'
#' Formula: \code{C_v = 2 e(N_v) / (k_v (k_v - 1))}; the graph average is
#' the mean over nodes with \code{k_v >= 2}.
#'
#' @param G Symmetric adjacency matrix; non-zero means an edge.
#' @return List with \code{estimate}, \code{local}, \code{degree},
#'   \code{n_defined}, \code{n}.
#' @references Watts, D. J. & Strogatz, S. H. (1998). Nature 393:440-442.
#' @export
#' @examples
#' Clstcoef(matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3))
Clstcoef <- function(G) {
  A <- as.matrix(G)
  n <- nrow(A)
  adj <- ((A != 0) | (t(A) != 0)) * 1
  diag(adj) <- 0
  deg <- rowSums(adj)
  local <- rep(NaN, n)
  for (v in seq_len(n)) {
    nb <- which(adj[v, ] == 1)
    k <- length(nb)
    if (k < 2L) next
    sub <- adj[nb, nb, drop = FALSE]
    e <- sum(sub[upper.tri(sub)])
    local[v] <- 2 * e / (k * (k - 1))
  }
  defined <- local[!is.nan(local)]
  .t1_result(estimate = if (length(defined)) sum(defined) / length(defined) else NaN,
             local = local, degree = deg, n_defined = length(defined), n = n,
             method = "Watts-Strogatz clustering coefficient")
}

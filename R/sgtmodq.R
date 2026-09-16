# SPDX-License-Identifier: AGPL-3.0-or-later
#' Newman-Girvan modularity Q of a labelled partition
#'
#' Modularity contrasts the observed weight inside communities with the
#' weight the configuration model would put there, \code{k_i k_j / 2m}.
#' A single community containing every node scores exactly zero, however
#' dense the graph, because observed and expected weight then agree by
#' construction.
#'
#' Formula: \code{Q = (1 / 2m) sum_ij (A_ij - k_i k_j / 2m) delta(c_i, c_j)}.
#'
#' @param A Symmetric adjacency or weight matrix.
#' @param labels Integer community label per node.
#' @return List with \code{Q}, \code{estimate}, \code{n_communities}, \code{n}.
#' @references Newman, M. E. J. & Girvan, M. (2004). Finding and
#'   evaluating community structure in networks. Physical Review E 69,
#'   026113. \doi{10.1103/PhysRevE.69.026113}.
#' @export
Sgtmodq <- function(A, labels) {
  M <- as.matrix(A)
  n <- nrow(M)
  if (n == 0L) stop("Sgtmodq: adjacency matrix is empty")
  if (ncol(M) != n) stop("Sgtmodq: adjacency matrix must be square")
  lab <- as.integer(unlist(labels))
  if (length(lab) != n) stop("Sgtmodq: labels must have one entry per node")
  k <- rowSums(M)
  m2 <- sum(k)
  if (m2 <= 0) stop("Sgtmodq: graph has no edge weight")
  q <- 0
  for (i in seq_len(n)) for (j in seq_len(n)) {
    if (lab[i] == lab[j]) q <- q + M[i, j] - k[i] * k[j] / m2
  }
  q <- q / m2
  .t1_result(Q = q, estimate = q, n_communities = length(unique(lab)),
             n = n, method = "Newman-Girvan modularity Q")
}

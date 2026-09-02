# SPDX-License-Identifier: AGPL-3.0-or-later
#' Freeman degree centrality of a node, normalised
#'
#' \code{C_D(v) = deg(v) / (n - 1)}. The divisor is the largest degree any
#' node can have in a simple graph on \code{n} nodes, so \code{C_D} lands
#' in \code{\[0, 1\]} and is comparable across graphs of different size: the
#' hub of a star scores exactly 1 and an isolate exactly 0. Weighted
#' adjacency matrices are summed rather than counted.
#'
#' @param A Adjacency or weight matrix.
#' @param node Zero-based index of the node whose centrality is returned.
#' @return List with estimate (normalised C_D), degree (raw), node, n.
#' @references Freeman (1979), Social Networks 1(3), 215-239,
#'   \doi{10.1016/0378-8733(78)90021-7}.
#' @export
Netdeg <- function(A, node = 0) {
  M <- .t1_mat(A)
  n <- nrow(M)
  if (n == 0L) stop("Netdeg: adjacency matrix is empty")
  if (ncol(M) != n) stop("Netdeg: adjacency matrix must be square")
  v <- as.integer(node)
  if (v < 0L || v >= n) stop("Netdeg: node out of range")
  if (n == 1L) stop("Netdeg: undefined for a single-node graph")
  deg <- sum(M[v + 1L, ]) - M[v + 1L, v + 1L]
  .t1_result(estimate = deg / (n - 1), degree = deg, node = v, n = n,
             method = "Freeman degree centrality (normalised)")
}

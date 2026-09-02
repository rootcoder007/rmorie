# SPDX-License-Identifier: AGPL-3.0-or-later
#' Assemble the weighted Laplacian from an edge list
#'
#' Parallel edges accumulate; a loop contributes to the degree but not to
#' the diagonal of L (the d_v - w(v, v) term). Vertex labels are one-based.
#'
#' Formula: w(u, v) = sum of the weights on \{u, v\};
#'   L(u, v) = d_v - w(v, v) if u = v, -w(u, v) otherwise
#'
#' @param edges Matrix with rows (u, v, weight); u, v one-based.
#' @param n Number of vertices (default: the largest label seen).
#' @return List with \code{W}, \code{L}, \code{degree}, \code{volume},
#'   \code{n}, \code{m}.
#' @references Chung (1997), Spectral Graph Theory, CBMS 92, Section 1.4,
#'   for the weighted definition with loops. Fetched from the author's own
#'   copy of the chapter.
#' @export
#' @examples
#' M <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 2)
#' Wgtlap(M)
Wgtlap <- function(edges, n = NULL) {
  E <- as.matrix(edges)
  if (ncol(E) != 3L) stop("edges rows must be (u, v, weight)")
  m <- nrow(E)
  N <- if (is.null(n)) max(as.integer(E[, 1:2])) else as.integer(n)
  if (N < 1L) stop("the graph needs at least one vertex")
  if (any(E[, 3] < 0)) stop("weights must be non-negative")
  W <- matrix(0, N, N)
  for (i in seq_len(m)) {
    u <- as.integer(E[i, 1])
    v <- as.integer(E[i, 2])
    w <- E[i, 3]
    if (u < 1L || u > N || v < 1L || v > N)
      stop("vertex label out of range 1..n")
    W[u, v] <- W[u, v] + w
    if (u != v) W[v, u] <- W[v, u] + w
  }
  d <- rowSums(W)
  L <- -W
  diag(L) <- d - diag(W)
  .t1_result(W = W, L = L, degree = d, volume = sum(d), n = N, m = m,
             method = "Weighted Laplacian from an edge list")
}

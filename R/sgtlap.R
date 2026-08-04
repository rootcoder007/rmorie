# SPDX-License-Identifier: AGPL-3.0-or-later
#' Combinatorial (unnormalised) Laplacian of a weighted graph.
#'
#' \code{rowsum} must be zero to machine precision: the all-ones vector is
#' always in the kernel.
#'
#' Formula: L(u, v) = d_v - w(v, v) if u = v, -w(u, v) if u ~ v, else 0
#'
#' @param W Symmetric non-negative weight matrix.
#' @return List with \code{L}, \code{degree}, \code{rowsum}, \code{n}.
#' @references Chung (1997), Spectral Graph Theory, CBMS 92, Section 1.4,
#'   which generalises the Section 1.2 definition to weighted graphs with
#'   loops exactly as written above. Fetched from the author's own copy of
#'   the chapter.
#' @export
Graphlap <- function(W) {
  W <- as.matrix(W)
  n <- nrow(W)
  if (ncol(W) != n) stop("W must be square")
  d <- rowSums(W)
  L <- -W
  diag(L) <- d - diag(W)
  .t1_result(L = L, degree = d, rowsum = rowSums(L), n = n,
             method = "Combinatorial Laplacian L = T - A")
}

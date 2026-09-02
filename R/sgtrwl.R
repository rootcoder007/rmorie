# SPDX-License-Identifier: AGPL-3.0-or-later
#' Random-walk Laplacian L_rw = I - P, with P the transition matrix
#'
#' Formula: P = T^-1 W; L_rw = I - T^-1 W
#'
#' @param W Symmetric non-negative weight matrix; every vertex must have
#'   positive degree.
#' @return List with \code{Lrw}, \code{P}, \code{degree}, \code{rowsum_P},
#'   \code{n}.
#' @references von Luxburg (2007), A Tutorial on Spectral Clustering,
#'   Statistics and Computing 17(4), 395-416, Section 3, which defines
#'   L_rw = I - D^-1 W and notes it is "closely related to a random walk".
#'   Fetched from arXiv:0711.0189.
#' @export
#' @examples
#' Rwlap(W = 5L)
Rwlap <- function(W) {
  W <- as.matrix(W)
  n <- nrow(W)
  if (ncol(W) != n) stop("W must be square")
  d <- rowSums(W)
  if (any(d <= 0)) stop("L_rw needs every vertex to have positive degree")
  P <- W / d
  .t1_result(Lrw = diag(1, n) - P, P = P, degree = d, rowsum_P = rowSums(P),
             n = n, method = "Random-walk Laplacian I - T^-1 W")
}

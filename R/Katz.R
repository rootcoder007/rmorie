# SPDX-License-Identifier: AGPL-3.0-or-later
#' Katz centrality
#'
#' Katz sums walks of every length with a geometric attenuation, so the
#' status vector solves a single linear system.  Convergence of the
#' series needs alpha below the reciprocal of the spectral radius of A.
#'
#' Formula: x = (I - alpha A)^{-1} 1 = sum_{k >= 0} alpha^k A^k 1.
#'
#' @param A Square adjacency matrix.
#' @param alpha Positive attenuation factor.
#' @return List with \code{estimate} (largest score), \code{centrality},
#'   \code{total}, \code{alpha}, \code{n}, \code{method}.
#' @references Katz (1953), A new status index derived from sociometric
#'   analysis, Psychometrika 18(1):39-43. \doi{10.1007/BF02289026}
#' @export
Katz <- function(A, alpha = 0.1) {
  M <- .s03mat(A)
  n <- nrow(M)
  if (n == 0L) stop("katz_centrality: adjacency matrix is empty")
  if (ncol(M) != n) stop("katz_centrality: adjacency matrix must be square")
  a <- as.numeric(alpha)
  if (a <= 0) stop("katz_centrality: alpha must be positive")
  K <- diag(1, n) - a * M
  x <- as.numeric(solve(K, rep(1, n)))
  .t1_result(estimate = max(x), centrality = x, total = sum(x),
             alpha = a, n = n,
             method = "x = (I - alpha A)^{-1} 1, Katz (1953)")
}

# SPDX-License-Identifier: AGPL-3.0-or-later
#' Katz centrality with an explicit baseline weight
#'
#' The scaled resolvent form: beta = 1 recovers the plain Katz index and
#' beta rescales every score linearly.
#'
#' Formula: C_K = (I - alpha A)^\{-1\} beta 1.
#'
#' @param G Square adjacency matrix of the graph.
#' @param alpha Positive attenuation factor.
#' @param beta Baseline weight.
#' @return List with \code{estimate}, \code{centrality}, \code{alpha},
#'   \code{beta}, \code{n}, \code{method}.
#' @references Katz (1953), Psychometrika 18(1):39-43.
#'   \doi{10.1007/BF02289026}
#' @export
#' @examples
#' Katzcn(matrix(c(0, 1, 1, 0, 0, 1, 1, 0, 0), 3, 3, byrow = TRUE))
Katzcn <- function(G, alpha = 0.1, beta = 1) {
  M <- .s03mat(G)
  n <- nrow(M)
  if (n == 0L) stop("katz_centrality: graph is empty")
  if (ncol(M) != n) stop("katz_centrality: adjacency matrix must be square")
  a <- as.numeric(alpha)
  b <- as.numeric(beta)
  if (a <= 0) stop("katz_centrality: alpha must be positive")
  K <- diag(1, n) - a * M
  x <- as.numeric(solve(K, rep(b, n)))
  .t1_result(estimate = max(x), centrality = x, alpha = a, beta = b,
             n = n, method = "C_K = (I - alpha A)^{-1} beta 1, Katz (1953)")
}

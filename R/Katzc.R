# SPDX-License-Identifier: AGPL-3.0-or-later
#' Katz centrality excluding the length-zero walk
#'
#' The variant that drops the k = 0 term of the resolvent series, so a
#' node with no incident walks scores zero instead of one.  The
#' additive constant beta shifts every score equally.
#'
#' Formula: x = (I - alpha A)^{-1} (alpha A) y + beta.
#'
#' @param y Node weights applied to the seed vector; ones give Katz's
#'   own definition.
#' @param A Square adjacency matrix.
#' @param alpha Positive attenuation factor.
#' @param beta Additive constant.
#' @return List with \code{estimate}, \code{centrality}, \code{alpha},
#'   \code{beta}, \code{n}, \code{method}.
#' @references Katz (1953), Psychometrika 18(1):39-43.
#'   \doi{10.1007/BF02289026}
#' @export
Katzc <- function(y, A, alpha = 0.1, beta = 0) {
  M <- .s03mat(A)
  n <- nrow(M)
  if (n == 0L) stop("katz_centrality: adjacency matrix is empty")
  if (ncol(M) != n) stop("katz_centrality: adjacency matrix must be square")
  w <- .s03vec(y)
  if (length(w) != n) stop("katz_centrality: y and A have different lengths")
  a <- as.numeric(alpha)
  if (a <= 0) stop("katz_centrality: alpha must be positive")
  rhs <- a * as.numeric(M %*% w)
  K <- diag(1, n) - a * M
  x <- as.numeric(solve(K, rhs)) + as.numeric(beta)
  .t1_result(estimate = max(x), centrality = x, alpha = a,
             beta = as.numeric(beta), n = n,
             method = "x = (I - alpha A)^{-1} (alpha A) y + beta, Katz (1953)")
}

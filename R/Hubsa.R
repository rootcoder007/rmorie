# SPDX-License-Identifier: AGPL-3.0-or-later
#' HITS run to convergence
#'
#' The same I and O operations as \code{Hits}, but the loop stops when
#' the largest absolute change in the normalised hub vector falls below
#' \code{tol}, which is the "converge" wording of Kleinberg's Iterate
#' procedure rather than a fixed sweep count.
#'
#' Formula: repeat a <- A' h, h <- A a until max|dh| <= tol.
#'
#' @param y Starting hub vector of length n; ones reproduce the paper.
#' @param A Square adjacency matrix.
#' @param tol Positive convergence tolerance on the hub vector.
#' @param max_iter Iteration cap.
#' @return List with \code{estimate}, \code{hubs}, \code{authorities},
#'   \code{iterations}, \code{delta}, \code{converged}, \code{n},
#'   \code{method}.
#' @references Kleinberg (1999), Journal of the ACM 46(5):604-632.
#'   \doi{10.1145/324133.324140}
#' @export
Hubsa <- function(y, A, tol = 1e-12, max_iter = 1000) {
  M <- .s03mat(A)
  n <- nrow(M)
  if (n == 0L) stop("hits_hubs_authorities: adjacency matrix is empty")
  if (ncol(M) != n) stop("hits_hubs_authorities: adjacency matrix must be square")
  h <- .s03vec(y)
  if (length(h) != n) stop("hits_hubs_authorities: y and A have different lengths")
  if (tol <= 0) stop("hits_hubs_authorities: tol must be positive")
  unit <- function(v) { s <- sqrt(sum(v * v)); if (s == 0) v else v / s }
  h <- unit(h)
  a <- rep(0, n)
  it <- 0L
  delta <- Inf
  while (it < as.integer(max_iter) && delta > tol) {
    a <- unit(as.numeric(t(M) %*% h))
    hn <- unit(as.numeric(M %*% a))
    delta <- max(abs(hn - h))
    h <- hn
    it <- it + 1L
  }
  .t1_result(estimate = max(h), hubs = h, authorities = a,
             iterations = it, delta = delta, converged = delta <= tol,
             n = n,
             method = "Kleinberg (1999) I/O recursion, stopped at max|dh| <= tol")
}

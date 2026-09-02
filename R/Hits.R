# SPDX-License-Identifier: AGPL-3.0-or-later
#' HITS hubs and authorities
#'
#' Kleinberg's I and O operations applied alternately, each pair of
#' updates followed by a normalisation to unit L2 norm.  In matrix form
#' a = A' h and h = A a, so the authority vector converges to the
#' principal eigenvector of A'A and the hub vector to that of A A'.
#'
#' Formula: a <- A' h; h <- A a; both rescaled to unit L2 norm.
#'
#' @param A Square adjacency matrix, A\[i, j\] > 0 for a link i -> j.
#' @param iters Number of I/O sweeps, at least one.
#' @return List with \code{estimate} (largest hub score), \code{hubs},
#'   \code{authorities}, \code{top_hub}, \code{n}, \code{iters},
#'   \code{method}.
#' @references Kleinberg (1999), Authoritative sources in a hyperlinked
#'   environment, Journal of the ACM 46(5):604-632.
#'   \doi{10.1145/324133.324140}
#' @export
#' @examples
#' Hits(matrix(c(0, 1, 1, 0, 0, 1, 1, 0, 0), 3, 3, byrow = TRUE))
Hits <- function(A, iters = 50) {
  M <- .s03mat(A)
  n <- nrow(M)
  if (n == 0L) stop("hits: adjacency matrix is empty")
  if (ncol(M) != n) stop("hits: adjacency matrix must be square")
  m <- as.integer(iters)
  if (m < 1L) stop("hits: iters must be at least 1")
  unit <- function(v) { s <- sqrt(sum(v * v))
  if (s == 0) v else v / s }
  h <- unit(rep(1, n))
  a <- rep(0, n)
  for (i in seq_len(m)) {
    a <- unit(as.numeric(t(M) %*% h))
    h <- unit(as.numeric(M %*% a))
  }
  top <- 1L
  for (i in seq_len(n)) if (h[i] > h[top]) top <- i
  .t1_result(estimate = h[top], hubs = h, authorities = a,
             top_hub = top, n = n, iters = m,
             method = "alternating I/O operations of Kleinberg (1999) sect. 3, L2-normalised")
}

# HITS: hubs and authorities.
# Source: Kleinberg, J. M. (1999), Authoritative sources in a
# hyperlinked environment, Journal of the ACM 46(5), 604-632, Sec. 3:
# the alternating I and O operations, a <- A' h then h <- A a, each
# followed by L2 normalisation, iterated to the principal eigenvectors
# of A'A (authorities) and AA' (hubs).
#
# Native implementation mirroring Python morie.fn.hits exactly: the
# same uniform start vector, the same operation ORDER (authorities
# updated first within each sweep, then hubs from the NEW authority
# vector), and the same lowest-index tie-break for the top hub.

#' .mor_hits_unit
#'
#' A step of the hits_native implementation. Called by \code{morie_hits}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param v Numeric; combined arithmetically in the body.
#' @return One of two values, depending on the branch taken.
#' @export
.mor_hits_unit <- function(v) {
  s <- sqrt(sum(v * v))
  if (s == 0) v else v / s
}

#' HITS hubs and authorities
#'
#' Iterates Kleinberg's (1999) mutually reinforcing updates: a page is
#' a good hub if it points to good authorities, and a good authority
#' if it is pointed to by good hubs.  With \code{A\[i, j\] = 1} for a
#' link from \code{i} to \code{j}, the sweep is
#' \eqn{a \leftarrow A^\top h}, \eqn{h \leftarrow A a}, each
#' normalised to unit L2 length.
#'
#' @param A Square adjacency matrix; \code{A\[i, j\]} is a link from
#'   \code{i} to \code{j}.
#' @param iters Number of sweeps, default 50.
#' @return A list with \code{estimate} (the top hub score),
#'   \code{hubs}, \code{authorities}, \code{top_hub} (1-based),
#'   \code{n}, \code{iters}, \code{method}.
#' @references Kleinberg, J. M. (1999). Authoritative sources in a
#'   hyperlinked environment. Journal of the ACM, 46(5), 604-632.
#' @export
morie_hits <- function(A, iters = 50L) {
  M <- as.matrix(A)
  n <- nrow(M)
  if (n == 0L) stop("hits: adjacency matrix is empty")
  if (ncol(M) != n) stop("hits: adjacency matrix must be square")
  m <- as.integer(iters)
  if (m < 1L) stop("hits: iters must be at least 1")
  h <- .mor_hits_unit(rep(1, n))
  a <- numeric(n)
  for (k in seq_len(m)) {
    a <- .mor_hits_unit(as.numeric(crossprod(M, h)))
    h <- .mor_hits_unit(as.numeric(M %*% a))
  }
  top <- 1L
  for (i in seq_len(n)) if (h[i] > h[top]) top <- i
  list(estimate = h[top], hubs = h, authorities = a, top_hub = top,
       n = n, iters = m,
       method = paste("alternating I/O operations of Kleinberg (1999)",
                      "sect. 3, L2-normalised"))
}

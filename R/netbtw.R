# SPDX-License-Identifier: AGPL-3.0-or-later
#' Freeman betweenness centrality of a node
#'
#' \code{C_B(v) = sum_{s != v != t} sigma_st(v) / sigma_st}, where
#' \code{sigma_st} counts shortest s-t paths and \code{sigma_st(v)} those
#' passing through \code{v}. Accumulated by Brandes' (2001) recursion.
#'
#' Brandes accumulates over ORDERED pairs; Freeman's C_B is defined over
#' unordered pairs, so for a symmetric adjacency the ordered sum is
#' halved. The normalised score divides by the maximum \code{(n-1)(n-2)/2}
#' attained by the hub of a star.
#'
#' @param A Adjacency matrix; non-zero entries are edges (unweighted).
#' @param node Zero-based index of the node whose centrality is returned.
#' @return List with estimate, normalized, cb_ordered, node, n, symmetric.
#' @references Freeman (1977), Sociometry 40(1), 35-41,
#'   \doi{10.2307/3033543}; Brandes (2001), J. Math. Sociol. 25(2),
#'   163-177, \doi{10.1080/0022250X.2001.9990249}.
#' @export
#' @examples
#' A <- matrix(c(4, 1, 0.5, 1, 3, 0.8, 0.5, 0.8, 2), nrow = 3)
#' res <- Netbtw(A = A)
#' res
Netbtw <- function(A, node = 0) {
  M <- .t1_mat(A)
  n <- nrow(M)
  if (n == 0L) stop("Netbtw: adjacency matrix is empty")
  if (ncol(M) != n) stop("Netbtw: adjacency matrix must be square")
  v <- as.integer(node)
  if (v < 0L || v >= n) stop("Netbtw: node out of range")
  sym <- all(M == t(M))
  cb <- rep(0, n)
  for (s in seq_len(n)) {
    S <- integer(0)
    P <- vector("list", n)
    sigma <- rep(0, n)
    sigma[s] <- 1
    d <- rep(-1L, n)
    d[s] <- 0L
    Q <- c(s)
    qh <- 1L
    while (qh <= length(Q)) {
      w0 <- Q[qh]
      qh <- qh + 1L
      S <- c(S, w0)
      for (w in seq_len(n)) {
        if (M[w0, w] == 0) next
        if (d[w] < 0L) { Q <- c(Q, w)
        d[w] <- d[w0] + 1L }
        if (d[w] == d[w0] + 1L) {
          sigma[w] <- sigma[w] + sigma[w0]
          P[[w]] <- c(P[[w]], w0)
        }
      }
    }
    delta <- rep(0, n)
    for (i in rev(seq_along(S))) {
      w <- S[i]
      for (u in P[[w]]) delta[u] <- delta[u] + (sigma[u] / sigma[w]) * (1 + delta[w])
      if (w != s) cb[w] <- cb[w] + delta[w]
    }
  }
  ordered <- cb[v + 1L]
  est <- if (sym) ordered / 2 else ordered
  denom <- if (sym) (n - 1) * (n - 2) / 2 else (n - 1) * (n - 2)
  norm <- if (denom > 0) est / denom else 0
  .t1_result(estimate = est, normalized = norm, cb_ordered = ordered,
             node = v, n = n, symmetric = if (sym) 1 else 0,
             method = "Freeman betweenness centrality")
}

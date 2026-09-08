# SPDX-License-Identifier: AGPL-3.0-or-later
#' Betweenness centrality by Brandes' algorithm
#'
#' Formula: c_B(v) = sum_\{s != v != t\} sigma_st(v) / sigma_st, accumulated by
#' dependency delta_s(v) = sum_w (sigma_sv/sigma_sw)(1 + delta_s(w))
#'
#' @param A Undirected unweighted adjacency; non-zero means an edge.
#' @param normalise Divide by (n-1)(n-2)/2, the maximum possible value.

#' @param A See Usage.
#' @param normalise See Usage.
#' @return List with ``betweenness``, ``normalised``, ``n``.
#' @references Brandes (2001), A faster algorithm for betweenness centrality, Journal of
#' Mathematical Sociology 25:163-177. Paywalled and not held locally; the dependency
#' recursion implemented here is the standard published form of the algorithm. Its output
#' is checked in the batch's anchor file against brute-force enumeration of all shortest
#' paths on a small graph.
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' Btwcent(V)
Btwcent <- function(A, normalise = FALSE) {
  A <- as.matrix(A)
  n <- nrow(A)
  diag(A) <- 0
  B <- A != 0
  cb <- numeric(n)
  for (s in seq_len(n)) {
    stack <- integer(0)
    pred <- vector("list", n)
    sigma <- numeric(n)
    dist <- rep(-1L, n)
    sigma[s] <- 1
    dist[s] <- 0L
    q <- s
    h <- 1L
    while (h <= length(q)) {
      v <- q[h]
      h <- h + 1L
      stack <- c(stack, v)
      for (w in which(B[v, ])) {
        if (dist[w] < 0L) { dist[w] <- dist[v] + 1L
        q <- c(q, w) }
        if (dist[w] == dist[v] + 1L) {
          sigma[w] <- sigma[w] + sigma[v]
          pred[[w]] <- c(pred[[w]], v)
        }
      }
    }
    delta <- numeric(n)
    for (w in rev(stack)) {
      for (v in pred[[w]]) delta[v] <- delta[v] + (sigma[v] / sigma[w]) * (1 + delta[w])
      if (w != s) cb[w] <- cb[w] + delta[w]
    }
  }
  cb <- cb / 2
  denom <- if (n > 2) (n - 1) * (n - 2) / 2 else NA_real_
  .t1_result(betweenness = cb, normalised = cb / denom, n = n,
             method = "Betweenness centrality (Brandes)")
}

# SPDX-License-Identifier: AGPL-3.0-or-later

#' Maximum flow / minimum cut
#'
#' Formula: Ford-Fulkerson augmentation with Edmonds-Karp's rule that the
#' augmenting path is always a SHORTEST one in the residual graph
#' (breadth-first search), bounding the number of augmentations by
#' O(V E) and making the procedure terminate on irrational capacities.
#'
#' On termination the vertices reachable from the source in the residual
#' graph form the source side S of a minimum cut, and the max-flow
#' min-cut theorem says cap(S, V \\ S) equals the value of the flow.
#' Both numbers are computed by separate routes and returned, so their
#' equality is checkable rather than assumed.
#'
#' @param G Square non-negative capacity matrix; G\[i, j\] is the capacity
#'   of the directed arc i -> j.
#' @param source,sink Zero-based vertex indices; must differ.
#' @return List with \code{estimate}, \code{max_flow}, \code{min_cut},
#'   \code{cut_size}, \code{source_side}, \code{augmentations}, \code{n},
#'   \code{method}.
#' @references Ford & Fulkerson (1956), Canadian Journal of Mathematics
#'   8:399-404, doi:10.4153/CJM-1956-045-5; Edmonds & Karp (1972), JACM
#'   19(2):248-264, doi:10.1145/321694.321699.
#' @export
#' @examples
#' G <- matrix(c(0, 3, 2, 0, 0, 0, 1, 2, 0, 0, 0, 3, 0, 0, 0, 0), 4, 4, byrow = TRUE)
#' Flowmm(G, source = 0, sink = 3)
Flowmm <- function(G, source, sink) {
  C <- .s03mat(G)
  n <- nrow(C)
  if (n == 0L) stop("empty input: G has no vertices")
  if (ncol(C) != n) stop("G must be square")
  if (any(C < 0)) stop("capacities must be non-negative")
  s <- as.integer(source) + 1L
  t <- as.integer(sink) + 1L
  if (s < 1L || s > n || t < 1L || t > n)
    stop("source and sink must be valid vertex indices")
  if (s == t) stop("source and sink must differ")
  R <- C
  flow <- 0; aug <- 0L
  repeat {
    prev <- rep(-1L, n)
    prev[s] <- s
    q <- s
    while (length(q) && prev[t] < 0L) {
      v <- q[1]; q <- q[-1]
      for (w in seq_len(n)) if (prev[w] < 0L && R[v, w] > 0) {
        prev[w] <- v; q <- c(q, w)
      }
    }
    if (prev[t] < 0L) break
    b <- Inf; w <- t
    while (w != s) {
      v <- prev[w]
      if (R[v, w] < b) b <- R[v, w]
      w <- v
    }
    w <- t
    while (w != s) {
      v <- prev[w]
      R[v, w] <- R[v, w] - b
      R[w, v] <- R[w, v] + b
      w <- v
    }
    flow <- flow + b
    aug <- aug + 1L
  }
  seen <- rep(FALSE, n); seen[s] <- TRUE; q <- s
  while (length(q)) {
    v <- q[1]; q <- q[-1]
    for (w in seq_len(n)) if (!seen[w] && R[v, w] > 0) {
      seen[w] <- TRUE; q <- c(q, w)
    }
  }
  side <- which(seen) - 1L
  cut <- 0
  for (i in seq_len(n)) if (seen[i]) for (j in seq_len(n)) if (!seen[j])
    cut <- cut + C[i, j]
  .t1_result(estimate = flow, max_flow = flow, min_cut = cut,
             cut_size = length(side), source_side = side,
             augmentations = aug, n = n,
             method = "Maximum flow / minimum cut")
}

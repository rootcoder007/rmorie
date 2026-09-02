# SPDX-License-Identifier: AGPL-3.0-or-later
#' Error and attack tolerance of a network
#'
#' Formula: remove f N nodes, then report S = |largest cluster| / N, <s> = mean size of the remaining fragments, and d = mean shortest path
#'
#' @param A Symmetric 0/1 adjacency matrix.
#' @param strategy Targeted (highest degree first) or random removal.
#' @param k Number of nodes removed.
#' @param seed Seed of the shared minstd stream used by 'error'.

#' @param A See Usage.
#' @param strategy See Usage.
#' @param k See Usage.
#' @param seed See Usage.
#' @return List with ``s_giant``, ``mean_fragment``, ``diameter``, ``removed``, ``n_components``, ``n``.
#' @references Albert, Jeong and Barabasi (2000), Error and attack tolerance of complex networks, Nature 406:378-382, arXiv:cond-mat/0008064. Verified against the paper for the definitions of d, S and <s>.
#' @export
#' @examples
#' M <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 2)
#' Netattack(M)
Netattack <- function(A, strategy = "attack", k = 1, seed = 1) {
  A <- as.matrix(A)
  n <- nrow(A)
  k <- as.integer(k)
  diag(A) <- 0
  deg <- rowSums(A != 0)
  if (strategy == "attack") {
    removed <- sort(order(-deg, seq_len(n))[seq_len(k)])
  } else if (strategy == "error") {
    g <- .t1_lcg(seed)
    pool <- seq_len(n)
    removed <- integer(0)
    for (i in seq_len(min(k, n))) {
      j <- as.integer(g$unif() * length(pool)) + 1L
      if (j > length(pool)) j <- length(pool)
      removed <- c(removed, pool[j])
      pool <- pool[-j]
    }
    removed <- sort(removed)
  } else stop("strategy must be 'attack' or 'error'")
  keep <- setdiff(seq_len(n), removed)
  m <- length(keep)
  B <- A[keep, keep, drop = FALSE] != 0
  comp <- integer(m)
  nc <- 0L
  for (s in seq_len(m)) {
    if (comp[s] != 0L) next
    nc <- nc + 1L
    stack <- s
    comp[s] <- nc
    while (length(stack)) {
      v <- stack[length(stack)]
      stack <- stack[-length(stack)]
      w <- which(B[v, ] & comp == 0L)
      if (length(w)) { comp[w] <- nc
      stack <- c(stack, w) }
    }
  }
  sizes <- sort(as.integer(table(comp)), decreasing = TRUE)
  giant <- if (length(sizes)) sizes[1] else 0L
  rest <- if (length(sizes) > 1) sizes[-1] else integer(0)
  tot <- 0
  pairs <- 0L
  for (s in seq_len(m)) {
    dist <- rep(-1L, m)
    dist[s] <- 0L
    q <- s
    h <- 1L
    while (h <= length(q)) {
      v <- q[h]
      h <- h + 1L
      w <- which(B[v, ] & dist < 0L)
      if (length(w)) { dist[w] <- dist[v] + 1L
      q <- c(q, w) }
    }
    d <- dist[-s]
    tot <- tot + sum(d[d > 0])
    pairs <- pairs + sum(d > 0)
  }
  .t1_result(s_giant = giant / n,
             mean_fragment = if (length(rest)) mean(rest) else NA_real_,
             diameter = if (pairs > 0) tot / pairs else NA_real_,
             removed = removed - 1L, n_components = nc, n = n,
             method = "Error and attack tolerance (Albert-Jeong-Barabasi)")
}

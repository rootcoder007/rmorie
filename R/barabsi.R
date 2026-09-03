# SPDX-License-Identifier: AGPL-3.0-or-later
#' Barabasi-Albert preferential attachment graph
#'
#' Formula: Pi(k_i) = k_i / sum_j k_j; each new vertex brings m edges
#'
#' @param n Final number of vertices.
#' @param m Edges brought by each new vertex.
#' @param m0 Size of the complete seed graph; ``m + 1`` if omitted.
#' @param seed Seed of the shared minstd stream.

#' @param n See Usage.
#' @param m See Usage.
#' @param m0 See Usage.
#' @param seed See Usage.
#' @return List with ``degree``, ``mean_degree``, ``max_degree``, ``edges``, ``n``, ``m``.
#' @references Barabasi and Albert (1999), Emergence of scaling in random networks,
#' Science 286:509-512, arXiv:cond-mat/9910332. Verified against the paper for Pi(k_i) =
#' k_i / sum_j k_j and the growth rule.
#' @export
#' @examples
#' Bamodel(n = 5L)
Bamodel <- function(n, m = 2, m0 = NULL, seed = 1) {
  n <- as.integer(n)
  m <- as.integer(m)
  m0 <- if (is.null(m0)) m + 1L else as.integer(m0)
  if (m < 1 || m0 < m || n < m0) stop("need 1 <= m <= m0 <= n")
  deg <- rep(m0 - 1L, m0)
  edges <- if (m0 > 1) t(utils::combn(m0, 2)) else matrix(0L, 0L, 2L)
  g <- .t1_lcg(seed)
  if (n > m0) for (v in (m0 + 1L):n) {
    cand <- seq_len(v - 1L)
    w <- as.numeric(deg[cand])
    targets <- integer(0)
    for (e in seq_len(m)) {
      u <- g$unif() * sum(w)
      cw <- cumsum(w)
      pick <- which(u < cw)[1]
      if (is.na(pick)) pick <- length(cand)
      targets <- c(targets, cand[pick])
      w[pick] <- 0
    }
    deg <- c(deg, m)
    deg[targets] <- deg[targets] + 1L
    edges <- rbind(edges, cbind(targets, v))
  }
  .t1_result(degree = deg, mean_degree = sum(deg) / n, max_degree = max(deg),
             edges = edges - 1L, n = n, m = m,
             method = "Barabasi-Albert preferential attachment")
}

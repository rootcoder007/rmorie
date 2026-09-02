# SPDX-License-Identifier: AGPL-3.0-or-later
#' Stochastic Kronecker graph edge-probability model
#'
#' The model takes an n0 by n0 initiator matrix Theta of edge
#' probabilities and forms its k-th Kronecker power; the result is the
#' matrix of independent edge probabilities on N = n0^k nodes, so the
#' whole distribution is determined by Theta and k and nothing has to be
#' sampled.  By multiplicativity of the Kronecker product the expected
#' edge count equals (sum Theta)^k exactly.  Drawing a realisation is a
#' separate step and is deliberately not folded in, since it would make
#' a closed form stochastic for no reason.
#'
#' Formula: P[i,j] = prod_l Theta[i_l, j_l] over the base-n0 digits.
#'
#' @param seed Square initiator matrix with entries in [0, 1].
#' @param k Kronecker power; the graph has n0^k nodes.
#' @return List with \code{estimate} (expected edges),
#'   \code{expected_edges}, \code{expected_self_loops}, \code{n_nodes},
#'   \code{mean_degree}, \code{max_degree}, \code{min_degree},
#'   \code{p_min}, \code{p_max}, \code{density}, \code{seed_sum},
#'   \code{k}, \code{P}, \code{n}, \code{method}.
#' @references Leskovec, Chakrabarti, Kleinberg, Faloutsos and
#'   Ghahramani (2010), Kronecker graphs: an approach to modeling
#'   networks, Journal of Machine Learning Research 11:985-1042.
#' @export
#' @examples
#' Krfgrp(seed = 1, k = 5L)
Krfgrp <- function(seed, k) {
  T <- .s03mat(seed); n0 <- nrow(T)
  if (n0 == 0L) stop("kronecker_graph: seed is empty")
  if (ncol(T) != n0) stop("kronecker_graph: seed must be square")
  if (any(T < 0 | T > 1)) stop("kronecker_graph: seed entries must lie in [0, 1]")
  kk <- as.integer(k)
  if (kk < 1L) stop("kronecker_graph: k must be at least 1")
  N <- n0^kk
  if (N > 4096) stop("kronecker_graph: n0^k exceeds 4096 nodes")
  P <- matrix(0, N, N)
  for (i in seq_len(N)) for (j in seq_len(N)) {
    a <- i - 1L; b <- j - 1L; v <- 1
    for (l in seq_len(kk)) {
      v <- v * T[(a %% n0) + 1L, (b %% n0) + 1L]
      a <- a %/% n0; b <- b %/% n0
    }
    P[i, j] <- v
  }
  deg <- rowSums(P)
  flat <- if (N <= 64L) as.numeric(t(P)) else numeric(0)
  .t1_result(estimate = sum(P), expected_edges = sum(P),
             expected_self_loops = sum(diag(P)), n_nodes = N,
             mean_degree = sum(P) / N, max_degree = max(deg),
             min_degree = min(deg), p_min = min(P), p_max = max(P),
             density = sum(P) / (N * N), seed_sum = sum(T), k = kk,
             P = flat, n = N,
             method = "P = Theta^[k]; sum(P) = (sum Theta)^k, Leskovec et al (2010) JMLR 11:985-1042")
}

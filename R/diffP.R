# SPDX-License-Identifier: AGPL-3.0-or-later

#' DiffPool differentiable graph pooling
#'
#' Formula: S = softmax(GNN_pool); H' = S^T H
#'
#' The assignment matrix S is soft and learned, so the coarsened graph
#' A' = S' A S and features H' = S' H are differentiable in it.  Every
#' row of S sums to one by construction; when S is one-hot, A'_rs is
#' exactly the number of edges between cluster r and cluster s.  The
#' auxiliary link prediction loss is ||A - S S'||_F / n.
#'
#' @param A An n x n adjacency matrix.
#' @param X An n x f node feature matrix.
#' @param K_clusters Number of clusters to pool into.
#' @param S An n x K matrix of assignment logits, or NULL.
#' @param seed Seed of the deterministic stream.
#' @return List with \code{estimate}, \code{S}, \code{A_pool},
#'   \code{H_pool}, \code{link_loss}, \code{entropy_loss}, \code{n},
#'   \code{K}, \code{method}.
#' @references Ying et al. (2018), Hierarchical Graph Representation
#'   Learning with Differentiable Pooling, NeurIPS 31:4800-4810.
#' @export
DiffP <- function(A, X, K_clusters = 2, S = NULL, seed = 42) {
  Am <- .s03mat(A)
  n <- nrow(Am)
  if (n == 0L) stop("empty input: A has no rows")
  if (ncol(Am) != n) stop("A must be a square adjacency matrix")
  Xm <- .s03mat(X)
  if (nrow(Xm) != n) stop("X must have one row per node")
  f <- ncol(Xm)
  K <- as.integer(K_clusters)
  if (K < 1L) stop("K_clusters must be at least 1")
  e <- .ghc_rng(seed)
  logit <- matrix(0, n, K)
  if (is.null(S)) {
    for (i in seq_len(n)) for (r in seq_len(K))
      logit[i, r] <- .ghc_norm(e, 1L, 0, 1)
  } else {
    logit <- .s03mat(S)
    if (nrow(logit) != n || ncol(logit) != K)
      stop("S must be an n x K matrix")
  }
  Sm <- matrix(0, n, K)
  for (i in seq_len(n)) Sm[i, ] <- .s03softmax(logit[i, ])
  Ap <- matrix(0, K, K)
  for (r in seq_len(K)) for (s in seq_len(K)) {
    acc <- 0
    for (i in seq_len(n)) for (j in seq_len(n))
      acc <- acc + Sm[i, r] * Am[i, j] * Sm[j, s]
    Ap[r, s] <- acc
  }
  Hp <- matrix(0, K, f)
  for (r in seq_len(K)) for (t in seq_len(f)) {
    acc <- 0
    for (i in seq_len(n)) acc <- acc + Sm[i, r] * Xm[i, t]
    Hp[r, t] <- acc
  }
  ll <- 0
  for (i in seq_len(n)) for (j in seq_len(n)) {
    s <- 0
    for (r in seq_len(K)) s <- s + Sm[i, r] * Sm[j, r]
    ll <- ll + (Am[i, j] - s)^2
  }
  ll <- sqrt(ll) / n
  ent <- 0
  for (i in seq_len(n)) for (r in seq_len(K))
    ent <- ent - Sm[i, r] * log(Sm[i, r] + 1e-300)
  .t1_result(estimate = ll, S = Sm, A_pool = Ap, H_pool = Hp,
             link_loss = ll, entropy_loss = ent / n, n = n, K = K,
             method = "DiffPool differentiable graph pooling")
}

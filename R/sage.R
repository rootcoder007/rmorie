# SPDX-License-Identifier: AGPL-3.0-or-later
#' GraphSAGE embedding generation (forward propagation)
#'
#' Algorithm 1 of the paper: aggregate the neighbourhood, concatenate with the
#' previous representation, apply W and a non-linearity, then L2-normalise;
#' equation (2) gives the convolutional variant.  Full neighbourhoods are used,
#' so the pass is deterministic.  Source consulted: Hamilton, Ying and Leskovec
#' (2017), Inductive Representation Learning on Large Graphs, arXiv:1706.02216.
#'
#' @param G square adjacency matrix.
#' @param X node feature matrix, one row per node.
#' @param W optional weight matrix; defaults to averaging the self and
#'   neighbourhood halves.
#' @param aggregator one of mean, sum, max.
#' @param K search depth (number of layers).
#' @param convolutional use equation (2) instead of Algorithm 1 line 5.
#' @return list: estimate, Z, frob, n, dim, method.
#' @keywords internal
#' @examples
#' sage(matrix(c(0, 1, 1, 0), 2, 2), matrix(c(1, 0, 0, 1), 2, 2, byrow = TRUE))
#' @export
sage <- function(G, X, W = NULL, aggregator = "mean", K = 1L, convolutional = FALSE) {
  A <- as.matrix(G); H <- as.matrix(X)
  n <- nrow(A); d <- ncol(H)
  if (is.null(W)) {
    wm <- if (convolutional) diag(d) else {
      m <- matrix(0, 2 * d, d)
      for (i in seq_len(2 * d)) m[i, ((i - 1) %% d) + 1] <- 0.5
      m
    }
  } else wm <- as.matrix(W)
  for (it in seq_len(as.integer(K))) {
    d <- ncol(H)
    agg <- matrix(0, n, d)
    for (v in seq_len(n)) {
      nb <- which(A[v, ] != 0)
      if (length(nb) > 0) for (j in seq_len(d)) {
        vals <- H[nb, j]
        agg[v, j] <- switch(aggregator, mean = mean(vals), sum = sum(vals),
                            max = max(vals), stop("aggregator must be mean, sum or max"))
      }
    }
    if (convolutional) {
      comb <- matrix(0, n, d)
      for (v in seq_len(n)) {
        nb <- c(which(A[v, ] != 0), v)
        for (j in seq_len(d)) comb[v, j] <- sum(H[nb, j]) / length(nb)
      }
      pre <- comb %*% wm
    } else {
      pre <- cbind(H, agg) %*% wm
    }
    Hn <- t3relu(pre)
    for (v in seq_len(n)) {
      nrm <- sqrt(sum(Hn[v, ]^2))
      if (nrm > 0) Hn[v, ] <- Hn[v, ] / nrm
    }
    H <- Hn
  }
  list(estimate = mean(H), Z = H, frob = sqrt(sum(H * H)),
       n = as.integer(n), dim = as.integer(ncol(H)),
       method = "GraphSAGE forward propagation (Hamilton, Ying & Leskovec 2017)")
}

# CANONICAL TEST
# r <- sage(matrix(c(0, 1, 1, 0), 2, 2), matrix(c(1, 0, 0, 1), 2, 2, byrow = TRUE))
# stopifnot(abs(r$frob - sqrt(2)) < 1e-12)

#' @rdname sage
#' @keywords internal
#' @export
morie_graphsage <- sage

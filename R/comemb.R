# SPDX-License-Identifier: AGPL-3.0-or-later

# Unnormalised second-order weights out of cur, having come from prev.
# 1/p to step back, 1 to a neighbour of prev, 1/q otherwise.  At
# p = q = 1 every weight is 1, so the walk is exactly DeepWalk's uniform
# walk -- the reduction that pins this function.
.n2v_probs <- function(nb, prev, cur, p, q) {
  x <- nb[[cur]]
  out <- numeric(length(x))
  for (k in seq_along(x)) {
    out[k] <- if (is.null(prev) || is.na(prev)) 1 else
      if (x[k] == prev) 1 / p else
        if (x[k] %in% nb[[prev]]) 1 else 1 / q
  }
  out
}

#' node2vec embeddings
#'
#' Formula: biased random walks + skip-gram
#'
#' The second-order walk interpolates between breadth-first and
#' depth-first exploration: the return parameter p penalises stepping
#' straight back, the in-out parameter q penalises leaving the
#' neighbourhood of the previous node.  At p = q = 1 all weights are
#' equal and the walk degenerates to DeepWalk.
#'
#' @param G An n x n adjacency matrix.
#' @param p Return parameter, strictly positive.
#' @param q In-out parameter, strictly positive.
#' @param dim Embedding dimension.
#' @param walk_len,n_walks,window,epochs,lr,neg As in \code{Deepw}.
#' @param seed Seed of the deterministic stream.
#' @return List with \code{estimate}, \code{embedding}, \code{walks},
#'   \code{degree}, \code{n}, \code{dim}, \code{p}, \code{q},
#'   \code{method}.
#' @references Grover & Leskovec (2016), node2vec, KDD 2016:855-864.
#' @export
Comemb <- function(G, p = 1, q = 1, dim = 8, walk_len = 10, n_walks = 4,
                   window = 3, epochs = 1, lr = 0.05, neg = 2, seed = 42) {
  g <- .dw_adj(G)
  n <- g$n; nb <- g$nb
  if (!(p > 0 && q > 0)) stop("p and q must be strictly positive")
  dim <- as.integer(dim); walk_len <- as.integer(walk_len)
  if (dim < 1L) stop("dim must be at least 1")
  if (walk_len < 2L) stop("walk_len must be at least 2")
  e <- .ghc_rng(seed)
  walks <- list()
  for (r in seq_len(as.integer(n_walks))) for (v in seq_len(n)) {
    w <- c(v)
    prev <- NA_integer_
    cur <- v
    for (s in seq_len(walk_len - 1L)) {
      if (!length(nb[[cur]])) break
      wt <- .n2v_probs(nb, prev, cur, p, q)
      tot <- 0
      for (t in wt) tot <- tot + t
      u <- .ghc_unif(e, 1L) * tot
      acc <- 0
      pick <- length(wt)
      for (k in seq_along(wt)) {
        acc <- acc + wt[k]
        if (u <= acc) { pick <- k; break }
      }
      prev <- cur
      cur <- nb[[cur]][pick]
      w <- c(w, cur)
    }
    walks[[length(walks) + 1L]] <- w
  }
  sg <- .dw_skipgram(walks, n, dim, as.integer(window), as.integer(epochs),
                     as.numeric(lr), as.integer(neg), e)
  W <- sg$W
  tot <- 0; cnt <- 0L
  for (i in seq_len(n)) for (j in nb[[i]]) {
    a <- sqrt(sum(W[i, ] * W[i, ])); b <- sqrt(sum(W[j, ] * W[j, ]))
    if (a > 0 && b > 0) {
      s <- 0
      for (d in seq_len(dim)) s <- s + W[i, d] * W[j, d]
      tot <- tot + s / (a * b)
      cnt <- cnt + 1L
    }
  }
  .t1_result(estimate = if (cnt > 0L) tot / cnt else NaN, embedding = W,
             walks = walks, degree = vapply(nb, length, 0L), n = n,
             dim = dim, p = p, q = q,
             method = "node2vec biased-walk embeddings")
}

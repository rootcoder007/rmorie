# SPDX-License-Identifier: AGPL-3.0-or-later

# Neighbour lists from a square 0/1 adjacency matrix.
#' Neighbour lists from a square 0/1 adjacency matrix
#'
#' A step of the deepw implementation. Called by \code{Comemb}, \code{Deepw}.
#' See the file header for the source the module follows.
#' it follows.
#'
#' @param G Passed to \code{.s03mat}.
#' @return A list with \code{A}, \code{n}, \code{nb}.
#' @export
.dw_adj <- function(G) {
  A <- .s03mat(G)
  n <- nrow(A)
  if (n == 0L) stop("empty input: G has no rows")
  if (ncol(A) != n) stop("G must be a square adjacency matrix")
  nb <- lapply(seq_len(n), function(i) {
    j <- which(A[i, ] != 0)
    j[j != i]
  })
  list(A = A, n = n, nb = nb)
}

# One uniform random walk: every neighbour has probability 1/deg.
#' One uniform random walk: every neighbour has probability 1/deg
#'
#' A step of the deepw implementation. Called by \code{Deepw}.
#' See the file header for the source the module follows.
#' it follows.
#'
#' @param nb A vector; indexed elementwise.
#' @param start See Usage.
#' @param length_ A count; the body uses it as \code{integer(...)}.
#' @param e Passed to \code{.ghc_unif}.
#' @return The value of \code{[}.
#' @export
.dw_walk <- function(nb, start, length_, e) {
  w <- integer(length_)
  w[1] <- start
  cur <- start
  m <- 1L
  if (length_ > 1L) for (s in seq_len(length_ - 1L)) {
    d <- length(nb[[cur]])
    if (d == 0L) break
    u <- .ghc_unif(e, 1L)
    k <- as.integer(u * d) + 1L
    if (k > d) k <- d
    cur <- nb[[cur]][k]
    m <- m + 1L
    w[m] <- cur
  }
  w[seq_len(m)]
}

# Skip-gram with negative sampling, plain SGD, fixed schedule.
#' Skip-gram with negative sampling, plain SGD, fixed schedule
#'
#' A step of the deepw implementation. Called by \code{Comemb}, \code{Deepw}.
#' See the file header for the source the module follows.
#' it follows.
#'
#' @param walks See Usage.
#' @param n A count; the body uses it as \code{seq_len(...)}.
#' @param dim A count; the body uses it as \code{seq_len(...)}.
#' @param window Numeric; combined arithmetically in the body.
#' @param epochs A count; the body uses it as \code{seq_len(...)}.
#' @param lr Numeric; combined arithmetically in the body.
#' @param neg A count; the body uses it as \code{seq_len(...)}.
#' @param e Passed to \code{.ghc_unif}.
#' @return A list with \code{W}, \code{C}.
#' @export
.dw_skipgram <- function(walks, n, dim, window, epochs, lr, neg, e) {
  W <- matrix(0, n, dim)
  for (i in seq_len(n)) for (d in seq_len(dim))
    W[i, d] <- .ghc_unif(e, 1L, -0.5, 0.5) / dim
  C <- matrix(0, n, dim)
  for (ep in seq_len(epochs)) {
    for (w in walks) {
      L <- length(w)
      for (i in seq_len(L)) {
        lo <- max(1L, i - window); hi <- min(L, i + window)
        for (j in lo:hi) {
          if (i == j) next
          tgt <- w[i]; ctx <- w[j]
          cs <- c(ctx); labs <- c(1)
          if (neg > 0L) for (q in seq_len(neg)) {
            k <- as.integer(.ghc_unif(e, 1L) * n) + 1L
            if (k > n) k <- n
            cs <- c(cs, k); labs <- c(labs, 0)
          }
          for (q in seq_along(cs)) {
            cc <- cs[q]
            s <- 0
            for (d in seq_len(dim)) s <- s + W[tgt, d] * C[cc, d]
            g <- (.s03sigmoid(s) - labs[q]) * lr
            for (d in seq_len(dim)) {
              wt <- W[tgt, d]
              W[tgt, d] <- wt - g * C[cc, d]
              C[cc, d] <- C[cc, d] - g * wt
            }
          }
        }
      }
    }
  }
  list(W = W, C = C)
}

#' DeepWalk node embeddings
#'
#' Formula: random walks + skip-gram
#'
#' Truncated uniform random walks are treated as sentences and fed to
#' skip-gram with negative sampling.  The walk itself is the whole model
#' assumption: from a node of degree d each neighbour is taken with
#' probability exactly 1/d, so the stationary distribution is
#' proportional to degree.
#'
#' @param G An n x n adjacency matrix.
#' @param walk_len Length of each walk.
#' @param dim Embedding dimension.
#' @param n_walks Walks started from each node.
#' @param window Skip-gram context window.
#' @param epochs Passes over the corpus.
#' @param lr SGD step size.
#' @param neg Negative samples per positive pair.
#' @param seed Seed of the deterministic stream.
#' @return List with \code{estimate}, \code{embedding}, \code{walks},
#'   \code{n_walks_total}, \code{degree}, \code{n}, \code{dim},
#'   \code{method}.
#' @references Perozzi, Al-Rfou & Skiena (2014), DeepWalk, KDD
#'   2014:701-710.
#' @export
#' @examples
#' G <- matrix(c(0, 1, 0, 1, 1, 0, 1, 0, 0, 1, 0, 1, 1, 0, 1, 0), 4, 4, byrow = TRUE)
#' Deepw(G, dim = 4, walk_len = 5, n_walks = 2)
Deepw <- function(G, walk_len = 10, dim = 8, n_walks = 4, window = 3,
                  epochs = 1, lr = 0.05, neg = 2, seed = 42) {
  g <- .dw_adj(G)
  n <- g$n; nb <- g$nb; A <- g$A
  walk_len <- as.integer(walk_len); dim <- as.integer(dim)
  if (walk_len < 2L) stop("walk_len must be at least 2")
  if (dim < 1L) stop("dim must be at least 1")
  if (as.integer(n_walks) < 1L) stop("n_walks must be at least 1")
  e <- .ghc_rng(seed)
  walks <- list()
  for (r in seq_len(as.integer(n_walks))) for (v in seq_len(n))
    walks[[length(walks) + 1L]] <- .dw_walk(nb, v, walk_len, e)
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
             walks = walks, n_walks_total = length(walks),
             degree = vapply(nb, length, 0L), n = n, dim = dim,
             method = "DeepWalk node embeddings")
}

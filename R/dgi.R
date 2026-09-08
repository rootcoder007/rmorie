# SPDX-License-Identifier: AGPL-3.0-or-later

#' Deep Graph Infomax
#'
#' Formula: max MI(local h_v, global s)
#'
#' One propagation step gives node summaries h = sigma(D^-1 A X W); the
#' graph summary s is their mean passed through a sigmoid; the
#' discriminator D(h, s) = sigmoid(h' M s) is trained to separate real
#' node summaries from those of a corrupted graph whose feature rows are
#' shuffled.  The objective is the binary cross-entropy of that
#' discriminator, so at M = 0 every score is 1/2 and the loss is exactly
#' log 2 -- the value that pins the sign convention.
#'
#' @param G An n x n adjacency matrix.
#' @param X An n x f node feature matrix.
#' @param encoder An f x d weight matrix, or NULL.
#' @param seed Seed of the deterministic stream.
#' @return List with \code{estimate}, \code{loss}, \code{h}, \code{s},
#'   \code{pos_score}, \code{neg_score}, \code{n}, \code{d},
#'   \code{method}.
#' @references Velickovic et al. (2019), Deep Graph Infomax, ICLR 2019.
#' @export
#' @examples
#' set.seed(1)
#' G <- matrix(c(0, 1, 0, 1, 0, 1, 0, 1, 0), 3, 3, byrow = TRUE)
#' X <- matrix(rnorm(6), 3, 2)
#' Dgi(G, X)
Dgi <- function(G, X, encoder = NULL, seed = 42) {
  A <- .s03mat(G)
  n <- nrow(A)
  if (n == 0L) stop("empty input: G has no rows")
  if (ncol(A) != n) stop("G must be a square adjacency matrix")
  Xm <- .s03mat(X)
  if (nrow(Xm) != n) stop("X must have one row per node")
  f <- ncol(Xm)
  e <- .ghc_rng(seed)
  if (is.null(encoder)) {
    d <- min(f, 4L)
    Wm <- matrix(0, f, d)
    for (t in seq_len(f)) for (c in seq_len(d))
      Wm[t, c] <- .ghc_norm(e, 1L, 0, 1) / sqrt(f)
    M <- matrix(0, d, d)
  } else {
    Wm <- .s03mat(encoder)
    if (nrow(Wm) != f) stop("encoder must have one row per feature")
    d <- ncol(Wm)
    M <- diag(1, d)
  }
  propagate <- function(F) {
    H <- matrix(0, n, d)
    for (i in seq_len(n)) {
      deg <- sum(A[i, ])
      if (deg == 0) deg <- 1
      agg <- numeric(f)
      for (t in seq_len(f)) {
        s <- 0
        for (j in seq_len(n)) s <- s + A[i, j] * F[j, t]
        agg[t] <- s / deg
      }
      for (c in seq_len(d)) {
        s <- 0
        for (t in seq_len(f)) s <- s + agg[t] * Wm[t, c]
        H[i, c] <- .s03sigmoid(s)
      }
    }
    H
  }
  H <- propagate(Xm)
  perm <- ((seq_len(n) - 1L) * 7L + 3L) %% n
  seen <- integer(0)
  for (v in perm) if (!(v %in% seen)) seen <- c(seen, v)
  for (i in 0:(n - 1L)) if (!(i %in% seen)) seen <- c(seen, i)
  Hc <- propagate(Xm[seen + 1L, , drop = FALSE])
  s <- numeric(d)
  for (c in seq_len(d)) {
    acc <- 0
    for (i in seq_len(n)) acc <- acc + H[i, c]
    s[c] <- .s03sigmoid(acc / n)
  }
  pos <- numeric(n)
  neg <- numeric(n)
  Ms <- numeric(d)
  for (c in seq_len(d)) {
    acc <- 0
    for (b in seq_len(d)) acc <- acc + M[c, b] * s[b]
    Ms[c] <- acc
  }
  for (i in seq_len(n)) {
    a <- 0
    b <- 0
    for (c in seq_len(d)) {
      a <- a + H[i, c] * Ms[c]
      b <- b + Hc[i, c] * Ms[c]
    }
    pos[i] <- .s03sigmoid(a)
    neg[i] <- .s03sigmoid(b)
  }
  lp <- 0
  ln <- 0
  for (v in pos) lp <- lp + log(v + 1e-300)
  for (v in neg) ln <- ln + log(1 - v + 1e-300)
  loss <- -(lp + ln) / (2 * n)
  .t1_result(estimate = loss, loss = loss, h = H, s = s, pos_score = pos,
             neg_score = neg, n = n, d = d,
             method = "Deep Graph Infomax objective")
}

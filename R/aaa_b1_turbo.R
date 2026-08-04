# SPDX-License-Identifier: AGPL-3.0-or-later
#' Shared helpers for the big1 quantization modules
#'
#' Internal only. Mirrors \code{morie.fn._b1turbo} on the Python side: a
#' deterministic rotation matrix and a Lloyd-Max scalar codebook, both
#' needed by more than one module in this batch.
#'
#' @name b1_turbo
#' @keywords internal
NULL

.b1_rotation <- function(d, seed = 1) {
  d <- as.integer(d)
  if (d < 1L) stop("the dimension must be at least 1")
  g <- .t1_lcg(seed)
  A <- matrix(0, d, d)
  for (i in seq_len(d)) for (j in seq_len(d)) A[i, j] <- g$norm()
  Q <- matrix(0, d, d)
  for (j in seq_len(d)) {
    v <- A[, j]
    if (j > 1L) for (k in seq_len(j - 1L)) v <- v - sum(Q[, k] * A[, j]) * Q[, k]
    nrm <- sqrt(sum(v^2))
    if (nrm < 1e-300) stop("the random matrix was rank deficient")
    if (v[j] < 0) nrm <- -nrm
    Q[, j] <- v / nrm
  }
  Q
}

.b1_codebook <- function(b, iters = 200, grid = 2001, lo = -6, hi = 6) {
  b <- as.integer(b)
  if (b < 1L) stop("the bit width must be at least 1")
  K <- 2^b
  n <- as.integer(grid)
  h <- (hi - lo) / (n - 1)
  x <- lo + (seq_len(n) - 1) * h
  w <- exp(-0.5 * x^2)
  c0 <- stats::qnorm((seq_len(K) - 0.5) / K)
  for (t in seq_len(as.integer(iters))) {
    d <- abs(outer(x, c0, "-"))
    best <- max.col(-d, ties.method = "first")
    num <- tapply(w * x, factor(best, levels = seq_len(K)), sum)
    den <- tapply(w, factor(best, levels = seq_len(K)), sum)
    num[is.na(num)] <- 0; den[is.na(den)] <- 0
    keep <- den > 0
    c0[keep] <- as.numeric(num[keep] / den[keep])
  }
  as.numeric(c0)
}

.b1_quantize <- function(y, cb) {
  d <- abs(outer(as.numeric(y), cb, "-"))
  as.integer(max.col(-d, ties.method = "first")) - 1L
}

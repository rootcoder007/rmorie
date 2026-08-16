# SPDX-License-Identifier: AGPL-3.0-or-later
#' Quantize every key in a cache and measure the achieved MSE.
#'
#' The per-key MSE is reported alongside the mean: one badly
#' reconstructed key can dominate an attention score even when the
#' average looks fine. The rotation is shared across keys.
#'
#' Formula: for each row k_i, xtilde_i = TurboQuant_mse(k_i, b);
#'   MSE_i = ||k_i - xtilde_i||^2, relative to ||k_i||^2
#'
#' @param K Key cache, one key per row.
#' @param b Bits per coordinate.
#' @param seed Seed for the shared pinned rotation.
#' @return List with \code{mse}, \code{relative_mse}, \code{mean_mse},
#'   \code{mean_relative}, \code{worst_relative}, \code{bound},
#'   \code{within_bound}, \code{n}, \code{d}, \code{b}.
#' @references Zandieh et al., arXiv:2504.19874, Algorithm 1 and Theorem
#'   1. Fetched from arXiv. NOTE: the worklist filed this row under
#'   "vdLaan-ICLR2026-arxiv-2504.19874"; arXiv 2504.19874 is Zandieh et
#'   al.'s TurboQuant, not a van der Laan paper.

#' .kvmse_rotation
#'
#' Part of the tqkmse implementation; see the file header for the source
#' it follows.
#'
#' @param d See Usage.
#' @param seed See Usage.
#' @return The value of \code{Q}, as built in the body.
#' @export
.kvmse_rotation <- function(d, seed) {
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

#' .kvmse_codebook
#'
#' Part of the tqkmse implementation; see the file header for the source
#' it follows.
#'
#' @param b See Usage.
#' @param iters Defaults to \code{200}.
#' @param grid Defaults to \code{2001}.
#' @param lo Defaults to \code{-6}.
#' @param hi Defaults to \code{6}.
#' @return A vector, from \code{as.numeric}.
#' @export
.kvmse_codebook <- function(b, iters = 200, grid = 2001, lo = -6, hi = 6) {
  K <- 2^as.integer(b)
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

#' .kvmse_quantize
#'
#' Part of the tqkmse implementation; see the file header for the source
#' it follows.
#'
#' @param y See Usage.
#' @param cb See Usage.
#' @return A numeric value.
#' @export
.kvmse_quantize <- function(y, cb) {
  d <- abs(outer(as.numeric(y), cb, "-"))
  as.integer(max.col(-d, ties.method = "first")) - 1L
}

#' @export
Kvmse <- function(K, b = 2, seed = 1) {
  K <- as.matrix(K); n <- nrow(K); d <- ncol(K); b <- as.integer(b)
  if (n < 1L) stop("the cache must hold at least one key")
  if (b < 1L) stop("the bit width must be at least 1")
  Pi <- .kvmse_rotation(d, seed)
  base <- .kvmse_codebook(b)
  mse <- rel <- numeric(n)
  for (i in seq_len(n)) {
    x <- K[i, ]
    y <- as.numeric(Pi %*% x)
    nrm <- sqrt(sum(x^2))
    sc <- if (nrm > 0) nrm / sqrt(d) else 1
    cb <- sc * base
    yt <- cb[.kvmse_quantize(y, cb) + 1L]
    xt <- as.numeric(t(Pi) %*% yt)
    mse[i] <- sum((x - xt)^2)
    rel[i] <- if (nrm > 0) mse[i] / nrm^2 else NaN
  }
  bnd <- sqrt(3) * pi / 2 * 4^(-b)
  mr <- mean(rel)
  .t1_result(mse = mse, relative_mse = rel, mean_mse = mean(mse),
             mean_relative = mr, worst_relative = max(rel), bound = bnd,
             within_bound = as.numeric(mr <= bnd), n = as.numeric(n),
             d = as.numeric(d), b = as.numeric(b),
             method = "Key-cache MSE under TurboQuant_mse, arXiv:2504.19874")
}

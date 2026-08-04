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
#' @export
Kvmse <- function(K, b = 2, seed = 1) {
  K <- as.matrix(K); n <- nrow(K); d <- ncol(K); b <- as.integer(b)
  if (n < 1L) stop("the cache must hold at least one key")
  if (b < 1L) stop("the bit width must be at least 1")
  Pi <- .b1_rotation(d, seed)
  base <- .b1_codebook(b)
  mse <- rel <- numeric(n)
  for (i in seq_len(n)) {
    x <- K[i, ]
    y <- as.numeric(Pi %*% x)
    nrm <- sqrt(sum(x^2))
    sc <- if (nrm > 0) nrm / sqrt(d) else 1
    cb <- sc * base
    yt <- cb[.b1_quantize(y, cb) + 1L]
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

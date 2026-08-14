# SPDX-License-Identifier: AGPL-3.0-or-later
#' Quantize one vector: rotate, scalar-quantize, dequantize, rotate back.
#'
#' ONLINE: the rotation and codebook are fixed in advance, so a vector is
#' quantized on arrival with no knowledge of those that follow. The
#' codebook is Lloyd-Max for a standard normal, rescaled by
#' ||x||/sqrt(d).
#'
#' Formula: y <- Pi . x; idx_j <- argmin_k |y_j - c_k|;
#'   ytilde_j <- c_\{idx_j\}; xtilde <- Pi' . ytilde
#'
#' @param x The vector to quantize.
#' @param b Bits per coordinate, b >= 1.
#' @param seed Seed for the pinned rotation.
#' @return List with \code{idx}, \code{reconstruction}, \code{codebook},
#'   \code{mse}, \code{relative_mse}, \code{bound}, \code{within_bound},
#'   \code{d}, \code{b}.
#' @references Zandieh et al., arXiv:2504.19874, Algorithm 1 lines 2-11
#'   and Theorem 1. Fetched from arXiv. The paper specifies the codebook
#'   only as the MSE-minimising centroids; the Lloyd-Max construction is
#'   documented in the batch helper .kvmse_codebook.
#' @export
Kvquant <- function(x, b = 2, seed = 1) {
  x <- .t1_vec(x); d <- length(x); b <- as.integer(b)
  if (d < 1L) stop("the vector must be non-empty")
  if (b < 1L) stop("the bit width must be at least 1")
  Pi <- .kvmse_rotation(d, seed)
  y <- as.numeric(Pi %*% x)
  nrm <- sqrt(sum(x^2))
  sc <- if (nrm > 0) nrm / sqrt(d) else 1
  cb <- sc * .kvmse_codebook(b)
  idx <- .kvmse_quantize(y, cb)
  yt <- cb[idx + 1L]
  xt <- as.numeric(t(Pi) %*% yt)
  mse <- sum((x - xt)^2)
  bnd <- sqrt(3) * pi / 2 * 4^(-b)
  rel <- if (nrm > 0) mse / nrm^2 else NaN
  .t1_result(idx = as.numeric(idx), reconstruction = xt, codebook = cb,
             mse = mse, relative_mse = rel, bound = bnd,
             within_bound = as.numeric(rel <= bnd), d = as.numeric(d),
             b = as.numeric(b),
             method = "TurboQuant_mse, arXiv:2504.19874 Algorithm 1")
}

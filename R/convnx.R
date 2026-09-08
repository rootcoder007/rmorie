# SPDX-License-Identifier: AGPL-3.0-or-later

#' ConvNeXt block
#'
#' Formula: depthwise conv + LayerNorm + 1x1 conv
#'
#' Depthwise k x k convolution, channel-last LayerNorm, a pointwise
#' expansion with GELU, a pointwise projection back, then a residual add
#' scaled by gamma.  ConvNeXt initialises the layer scale gamma at a
#' small value; at gamma = 0 the block is EXACTLY the identity, which is
#' the check that the residual path is wired the right way round.
#'
#' @param x An H x W matrix for a single channel.
#' @param filters Number of channels; inferred when NULL.
#' @param kernel Depthwise kernel size, odd.
#' @param expand Pointwise expansion factor.
#' @param layer_scale Initial gamma of the residual scaling.
#' @param seed Seed of the deterministic stream.
#' @return List with \code{estimate}, \code{out}, \code{residual_norm},
#'   \code{H}, \code{W}, \code{C}, \code{method}.
#' @references Liu et al. (2022), A ConvNet for the 2020s,
#'   CVPR 2022:11976-11986.
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' Convnx(V)
Convnx <- function(x, filters = NULL, kernel = 7, expand = 4,
                   layer_scale = 0, seed = 42) {
  M <- .s03mat(x)
  H <- nrow(M)
  if (H == 0L) stop("empty input: x has no rows")
  W <- ncol(M)
  C <- if (is.null(filters)) 1L else as.integer(filters)
  if (C < 1L) stop("filters must be at least 1")
  k <- as.integer(kernel)
  if (k < 1L || k %% 2L == 0L) stop("kernel must be odd and positive")
  expand <- as.integer(expand)
  if (expand < 1L) stop("expand must be at least 1")
  e <- .ghc_rng(seed)
  dw <- matrix(0, k, k)
  for (a in seq_len(k)) for (b in seq_len(k))
    dw[a, b] <- .ghc_norm(e, 1L, 0, 1) / k
  w1 <- numeric(expand)
  for (q in seq_len(expand)) w1[q] <- .ghc_norm(e, 1L, 0, 0.02)
  w2 <- numeric(expand)
  for (q in seq_len(expand)) w2[q] <- .ghc_norm(e, 1L, 0, 0.02)
  r <- k %/% 2L
  conv <- matrix(0, H, W)
  for (i in seq_len(H)) for (j in seq_len(W)) {
    s <- 0
    for (a in -r:r) for (b in -r:r) {
      ii <- min(max(i + a, 1L), H)
      jj <- min(max(j + b, 1L), W)
      s <- s + M[ii, jj] * dw[a + r + 1L, b + r + 1L]
    }
    conv[i, j] <- s
  }
  mu <- 0
  for (i in seq_len(H)) for (j in seq_len(W)) mu <- mu + conv[i, j]
  mu <- mu / (H * W)
  vr <- 0
  for (i in seq_len(H)) for (j in seq_len(W)) vr <- vr + (conv[i, j] - mu)^2
  vr <- vr / (H * W)
  inv <- 1 / sqrt(vr + 1e-6)
  out <- matrix(0, H, W)
  for (i in seq_len(H)) for (j in seq_len(W)) {
    h <- (conv[i, j] - mu) * inv
    acc <- 0
    for (q in seq_len(expand)) acc <- acc + .s03gelu(h * w1[q]) * w2[q]
    out[i, j] <- M[i, j] + layer_scale * acc
  }
  res <- 0
  for (i in seq_len(H)) for (j in seq_len(W)) res <- res + (out[i, j] - M[i, j])^2
  tot <- 0
  for (i in seq_len(H)) for (j in seq_len(W)) tot <- tot + out[i, j]
  .t1_result(estimate = tot / (H * W), out = out, residual_norm = sqrt(res),
             H = H, W = W, C = C, method = "ConvNeXt block")
}

# SPDX-License-Identifier: AGPL-3.0-or-later

#' ControlNet attachment
#'
#' Formula: trainable copy of the UNet encoder; zero-conv
#'
#' The conditioning branch is a copy of the frozen encoder whose output
#' passes through a 1x1 convolution initialised at ZERO before it is
#' added back to the base feature map.  At initialisation the sum is
#' therefore exactly the base output: attaching a ControlNet cannot
#' perturb the pretrained model until the zero-convolution has learned a
#' non-zero weight.  That identity is the whole design.
#'
#' @param base Base-network feature map, H x W.
#' @param condition Conditioning image of the same shape.
#' @param zero_conv_weight Weight of the zero convolution; 0 at
#'   initialisation.
#' @param seed Seed of the deterministic stream.
#' @return List with \code{estimate}, \code{out}, \code{control},
#'   \code{delta_norm}, \code{is_identity}, \code{H}, \code{W},
#'   \code{method}.
#' @references Zhang, Rao & Agrawala (2023), Adding Conditional Control
#'   to Text-to-Image Diffusion Models, ICCV 2023:3836-3847.
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' Cncpat(V, V)
Cncpat <- function(base, condition, zero_conv_weight = 0, seed = 42) {
  B <- .s03mat(base); Cm <- .s03mat(condition)
  H <- nrow(B)
  if (H == 0L) stop("empty input: base has no rows")
  W <- ncol(B)
  if (nrow(Cm) != H || ncol(Cm) != W)
    stop("base and condition must have the same shape")
  e <- .ghc_rng(seed)
  w <- matrix(0, 3, 3)
  for (a in 1:3) for (b in 1:3) w[a, b] <- .ghc_norm(e, 1L, 0, 0.5)
  ctrl <- matrix(0, H, W)
  for (i in seq_len(H)) for (j in seq_len(W)) {
    s <- 0
    for (a in -1:1) for (b in -1:1) {
      ii <- min(max(i + a, 1L), H)
      jj <- min(max(j + b, 1L), W)
      s <- s + Cm[ii, jj] * w[a + 2L, b + 2L]
    }
    ctrl[i, j] <- .s03gelu(s)
  }
  out <- B + zero_conv_weight * ctrl
  dn <- 0
  for (i in seq_len(H)) for (j in seq_len(W)) dn <- dn + (out[i, j] - B[i, j])^2
  dn <- sqrt(dn)
  tot <- 0
  for (i in seq_len(H)) for (j in seq_len(W)) tot <- tot + out[i, j]
  .t1_result(estimate = tot / (H * W), out = out, control = ctrl,
             delta_norm = dn, is_identity = as.integer(dn == 0),
             H = H, W = W,
             method = "ControlNet attachment with a zero convolution")
}

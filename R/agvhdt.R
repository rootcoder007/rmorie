# SPDX-License-Identifier: AGPL-3.0-or-later
#' AlphaZero value head
#'
#' Silver et al. (2017), Nature 550, 354-359, methods, "Neural network
#' architecture": a 1x1 convolution to one plane, batch normalisation, a
#' rectifier, a fully connected layer to 256 units, a rectifier, a fully
#' connected layer to one unit, and a tanh, so the output lies in \[-1,
#' 1\].  Silver et al. (2018), arXiv:1712.01815 (FETCHED), keeps the same
#' architecture.  The Nature paper is paywalled; the layer list is
#' reproduced identically everywhere and its only numeric content, the
#' final tanh, is unambiguous.  Weights are supplied by the caller; with
#' none, the projection is the mean of the plane -- the 1x1 convolution
#' with a uniform kernel, the only choice that invents no parameters.
#'
#' @param x the feature plane, flattened.
#' @param W optional weights of the final linear layer.
#' @param b bias.
#' @param scale multiplier on the pre-activation.
#' @return list: estimate, v, pre, n, method.
#' @keywords internal
#' @examples
#' Valuehead(c(0.2, -0.4, 0.9))$v
#' @export
Valuehead <- function(x, W = NULL, b = 0, scale = 1) {
  f <- .s03vec(x)
  n <- length(f)
  w <- if (!is.null(W)) .s03vec(W) else rep(if (n > 0L) 1 / n else 0, n)
  pre <- as.numeric(b)
  for (i in seq_len(n)) pre <- pre + w[i] * f[i]
  pre <- pre * as.numeric(scale)
  v <- tanh(pre)
  list(estimate = v, v = v, pre = pre, n = n,
       method = "AlphaZero value head: linear projection then tanh")
}

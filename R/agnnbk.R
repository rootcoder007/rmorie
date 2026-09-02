# SPDX-License-Identifier: AGPL-3.0-or-later
#' AlphaZero residual block
#'
#' He, Zhang, Ren and Sun (2016), Deep residual learning for image
#' recognition, CVPR 770-778 (arXiv:1512.03385), equation (1), y = F(x,
#' \{W_i\}) + x, specialised by Silver et al. (2017), Nature 550, 354-359,
#' methods, to two 3x3 convolutions each followed by batch normalisation,
#' with a rectifier after the first and after the skip addition:
#' y = relu(x + BN(conv2(relu(BN(conv1(x)))))).  Silver et al. (2018),
#' arXiv:1712.01815 (FETCHED), uses the same tower.  Batch normalisation
#' is the inference-time affine on the input's own statistics (Ioffe and
#' Szegedy 2015, arXiv:1502.03167, eq. 3).  Convolution is 1-D and
#' zero-padded; an integer `filters` means the identity kernel, the honest
#' degenerate case that invents no weights.
#'
#' @param x input features.
#' @param filters an integer, or a list of two kernels.
#' @return list: estimate, y, h1, n, method.
#' @keywords internal
#' @examples
#' Resblock(c(1, -2, 3, 0))$y
#' @export
Resblock <- function(x, filters = 1) {
  bn <- function(v) {
    m <- .s03mean(v)
    s <- 0
    for (z in v) s <- s + (z - m) * (z - m)
    va <- if (length(v)) s / length(v) else 0
    (v - m) / sqrt(va + 1e-5)
  }
  conv1d <- function(v, kern) {
    n <- length(v)
    K <- length(kern)
    off <- K %/% 2L
    out <- numeric(n)
    for (i in seq_len(n)) {
      s <- 0
      for (j in seq_len(K)) {
        t <- i + j - 1L - off
        if (t >= 1L && t <= n) s <- s + kern[j] * v[t]
      }
      out[i] <- s
    }
    out
  }
  v <- .s03vec(x)
  if (is.numeric(filters) && length(filters) == 1L) {
    kerns <- list(1, 1)
  } else {
    kerns <- list(
      .s03vec(filters[[1]]),
      .s03vec(if (length(filters) > 1L) filters[[2]] else filters[[1]])
    )
  }
  h <- conv1d(v, kerns[[1]])
  h <- bn(h)
  h1 <- pmax(h, 0)
  h2 <- conv1d(h1, kerns[[2]])
  h2 <- bn(h2)
  y <- pmax(v + h2, 0)
  list(
    estimate = .s03mean(y), y = y, h1 = h1, n = length(v),
    method = "Residual block y = relu(x + BN(conv2(relu(BN(conv1(x))))))"
  )
}

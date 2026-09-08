# SPDX-License-Identifier: AGPL-3.0-or-later
#' EfficientNet MBConv block and compound scaling
#'
#' Tan and Le (2019), EfficientNet: rethinking model scaling for
#' convolutional neural networks, ICML 97, 6105-6114 (arXiv:1905.11946 --
#' FETCHED).  The block: "its main building block is mobile inverted
#' bottleneck MBConv (Sandler et al. 2018; Tan et al. 2019), to which we
#' also add squeeze-and-excitation optimization (Hu et al. 2018)" -- an
#' expanding 1x1 convolution, a depthwise convolution, a
#' squeeze-and-excitation gate, a projecting 1x1 convolution, and a
#' residual connection when the shapes allow.  The compound scaling, eqs.
#' (2)-(3): d = alpha^phi, w = beta^phi, r = gamma^phi subject to alpha
#' beta^2 gamma^2 ~= 2, with "the best values for EfficientNet-B0 are
#' alpha = 1.2, beta = 1.1, gamma = 1.15" -- quoted verbatim and used as
#' the defaults.  The block runs over a 1-D feature vector, the honest
#' reduction: the spatial structure is orthogonal to its arithmetic.
#'
#' @param x input features.
#' @param expand_ratio the inverted-bottleneck expansion.
#' @param filters output width; defaults to the input width.
#' @param se_ratio squeeze-and-excitation reduction ratio.
#' @param phi compound-scaling coefficient.
#' @return list: estimate, y, se, depth, width, resolution, constraint,
#'   method.
#' @keywords internal
#' @examples
#' Mbconv(c(1, -2, 3, 0.5), 2, phi = 1)$depth
#' @export
Mbconv <- function(x, expand_ratio = 6, filters = NULL, se_ratio = 0.25,
                   phi = NULL) {
  alpha <- 1.2
  beta <- 1.1
  gam <- 1.15
  v <- .s03vec(x)
  n <- length(v)
  m <- as.integer(as.numeric(expand_ratio) * n)
  if (m < 1L) m <- 1L
  e <- numeric(m)
  for (i in seq_len(m)) e[i] <- v[((i - 1L) %% n) + 1L] * (1 / as.numeric(expand_ratio))
  dw <- numeric(m)
  for (i in seq_len(m)) {
    s <- 0
    cnt <- 0
    for (j in c(-1L, 0L, 1L)) {
      t <- i + j
      if (t >= 1L && t <= m) { s <- s + e[t]
      cnt <- cnt + 1 }
    }
    dw[i] <- s / cnt
  }
  for (i in seq_len(m)) dw[i] <- .s03swish(dw[i])
  avg <- .s03mean(dw)
  r <- as.numeric(se_ratio)
  se <- rep(.s03sigmoid(avg * r), m)
  ex <- dw * se
  f <- if (!is.null(filters)) as.integer(filters) else n
  y <- numeric(f)
  for (j in seq_len(f)) {
    s <- 0
    cnt <- 0
    for (i in seq_len(m)) if (((i - 1L) %% f) + 1L == j) { s <- s + ex[i]
    cnt <- cnt + 1 }
    y[j] <- if (cnt > 0) s / cnt else 0
  }
  if (f == n) y <- y + v
  if (is.null(phi)) {
    d <- NaN
    w <- NaN
    res <- NaN
  } else {
    d <- alpha^as.numeric(phi)
    w <- beta^as.numeric(phi)
    res <- gam^as.numeric(phi)
  }
  list(estimate = .s03mean(y), y = y, se = se, depth = d, width = w,
       resolution = res, constraint = alpha * beta^2 * gam^2,
       method = "MBConv with squeeze-and-excitation, plus EfficientNet compound scaling (Tan and Le 2019, eqs. 2-3)")
}

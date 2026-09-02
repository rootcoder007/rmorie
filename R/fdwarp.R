# SPDX-License-Identifier: AGPL-3.0-or-later
#' Dynamic time warping
#'
#' Sakoe, H. and Chiba, S. (1978), "Dynamic programming algorithm optimization
#' for spoken word recognition", IEEE Transactions on Acoustics, Speech, and
#' Signal Processing 26(1), 43-49.  The time-normalised distance between two
#' patterns is the minimum, over monotone alignment paths, of the accumulated
#' local distance, computed by the forward recursion
#' g(i, j) = d(i, j) + min\[g(i-1, j), g(i-1, j-1), g(i, j-1)\] with
#' g(1, 1) = d(1, 1).  That is the symmetric form with no slope constraint,
#' equation (7) of the paper in its unweighted case.
#'
#' The Sakoe-Chiba adjustment window |i - j| <= window is supported and is off
#' by default.  The time-normalised distance divides the accumulated cost by
#' the length of the optimal path.
#'
#' @param x,y the two sequences; they need not have the same length.
#' @param cost local distance, "abs" for |x_i - y_j| or "sq" for its square.
#' @param window Sakoe-Chiba adjustment window; unrestricted when omitted.
#' @return list: estimate, distance, normalized, path_length, path, n, m,
#'   method.
#' @keywords internal
#' @examples
#' Fdwarp(c(0, 1), c(0, 2))$distance
#' @export
Fdwarp <- function(x, y, cost = "abs", window = NULL) {
  a <- .s03vec(x)
  b <- .s03vec(y)
  n <- length(a); m <- length(b)
  if (n == 0L || m == 0L) stop("functional_warping: both sequences must be non-empty")
  if (!identical(cost, "abs") && !identical(cost, "sq")) stop("functional_warping: cost must be abs or sq")
  if (!is.null(window)) {
    w <- as.integer(window)
    if (is.na(w) || w < 0L) stop("functional_warping: window must be non-negative")
    if (w < abs(n - m)) w <- abs(n - m)
  } else w <- NULL
  dfun <- function(i, j) {
    r <- a[i + 1L] - b[j + 1L]
    if (r < 0) r <- -r
    if (identical(cost, "sq")) r * r else r
  }
  g <- matrix(Inf, n, m)
  for (i in 0:(n - 1L)) {
    lo <- if (is.null(w)) 0L else max(0L, i - w)
    hi <- if (is.null(w)) m - 1L else min(m - 1L, i + w)
    if (hi < lo) next
    for (j in lo:hi) {
      if (i == 0L && j == 0L) { g[1, 1] <- dfun(0L, 0L); next }
      best <- Inf
      if (i > 0L && g[i, j + 1L] < best) best <- g[i, j + 1L]
      if (j > 0L && g[i + 1L, j] < best) best <- g[i + 1L, j]
      if (i > 0L && j > 0L && g[i, j] < best) best <- g[i, j]
      g[i + 1L, j + 1L] <- if (is.finite(best)) dfun(i, j) + best else Inf
    }
  }
  dist <- g[n, m]
  if (!is.finite(dist)) stop("functional_warping: the window is too narrow to admit any path")
  pi_ <- integer(0); pj_ <- integer(0)
  i <- n - 1L; j <- m - 1L
  repeat {
    pi_ <- c(pi_, i); pj_ <- c(pj_, j)
    if (i == 0L && j == 0L) break
    bv <- Inf; bi <- i; bj <- j; seen <- FALSE
    if (i > 0L && j > 0L) { bv <- g[i, j]; bi <- i - 1L; bj <- j - 1L; seen <- TRUE }
    if (i > 0L) {
      v <- g[i, j + 1L]
      if (!seen || v < bv) { bv <- v; bi <- i - 1L; bj <- j; seen <- TRUE }
    }
    if (j > 0L) {
      v <- g[i + 1L, j]
      if (!seen || v < bv) { bv <- v; bi <- i; bj <- j - 1L }
    }
    i <- bi; j <- bj
  }
  pi_ <- rev(pi_); pj_ <- rev(pj_)
  L <- length(pi_)
  list(estimate = dist, distance = dist, normalized = dist / L, path_length = L,
       path = cbind(as.numeric(pi_), as.numeric(pj_)), n = n, m = m,
       method = "Sakoe-Chiba (1978) symmetric DP recursion g(i,j) = d(i,j) + min[g(i-1,j), g(i-1,j-1), g(i,j-1)]")
}

# SPDX-License-Identifier: AGPL-3.0-or-later
#' Piecewise aggregate approximation
#'
#' Formula: \code{xbar_i = (N / n) sum_{j = n(i-1)/N + 1}^{n i / N} x_j}.
#'
#' When \code{N} divides \code{n} this is the mean of each of \code{N}
#' equal blocks. When it does not, the sum is over a FRACTIONAL window:
#' the observation straddling a segment boundary contributes to both
#' segments in proportion to the overlap. Truncating instead of
#' splitting is the usual implementation error and it breaks the one
#' property the representation is supposed to have -- that the segment
#' means average back to the mean of the series.
#'
#' @param x The series, length n.
#' @param N Number of segments, \code{1 <= N <= n}.
#' @return List with \code{paa}, \code{estimate}, \code{segment_width},
#'   \code{N}, \code{n}.
#' @references Keogh, E., Chakrabarti, K., Pazzani, M. & Mehrotra, S.
#'   (2001). Dimensionality reduction for fast similarity search in
#'   large time series databases. Knowledge and Information Systems,
#'   3(3), 263-286. doi:10.1007/PL00011669
#' @export
#' @examples
#' Paa(x = c(1, 2, 3, 4, 5, 6, 7, 8), N = 5L)
Paa <- function(x, N) {
  v <- as.numeric(x)
  n <- length(v)
  k <- as.integer(N)
  if (n == 0L) stop("Paa: x is empty")
  if (k < 1L || k > n) stop("Paa: N must satisfy 1 <= N <= n")
  w <- n / k
  out <- numeric(k)
  for (i in seq_len(k)) {
    lo <- (i - 1) * w
    hi <- i * w
    s <- 0
    j <- as.integer(floor(lo))
    while (j < n && j < hi) {
      a <- max(lo, j)
      b <- min(hi, j + 1)
      if (b > a) s <- s + (b - a) * v[j + 1L]
      j <- j + 1L
    }
    out[i] <- s / w
  }
  .t1_result(paa = out, estimate = out[1], segment_width = w,
             N = k, n = n,
             method = "Piecewise aggregate approximation")
}

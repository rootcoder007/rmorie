# SPDX-License-Identifier: AGPL-3.0-or-later
#' Baxter-King approximate band-pass filter
#'
#' Baxter, M. and King, R. G. (1999), "Measuring Business Cycles: Approximate
#' Band-Pass Filters for Economic Time Series", The Review of Economics and
#' Statistics 81(4), 575-593, doi:10.1162/003465399558454 (verified against
#' Crossref).  The construction below was read from rendered images of the
#' NBER working-paper version (w5022), whose text layer is unusable; the
#' load-bearing passage is section 2.5, "Constraints on specific points",
#' printed page 8, equation (8).
#'
#' The ideal band-pass filter has frequency response 1 on
#' w_low <= |w| <= w_high and 0 elsewhere.  Its Fourier coefficients follow
#' directly: b_0 = (w_high - w_low)/pi and
#' b_h = (sin(h w_high) - sin(h w_low))/(pi h) for h != 0, equivalently the
#' difference of two low-pass filters, which is how the paper derives them.
#' Truncating at lag K gives a finite moving average but destroys the
#' zero-frequency property that matters: page 8 requires the band-pass weights
#' to SUM TO ZERO, so the filter annihilates a constant and, applied to a
#' series with a unit root, returns a stationary one.  Equation (8) makes the
#' adjustment additive, a_h = b_h + theta with
#' theta = (target - sum_{h=-K}^{K} b_h)/(2K + 1), target 1 for the low-pass
#' case the paper writes out and target 0 for the band-pass case it states in
#' the following paragraph.  Without that adjustment the filter has non-zero
#' gain at frequency zero and leaks the trend straight into the "cycle" -- the
#' single most common way this filter is got wrong.
#'
#' Because the filter is a two-sided moving average of half-width K, the first
#' and last K observations have no filtered value; they are returned as NA
#' rather than silently padded, and n_valid reports the count.
#'
#' Anchors, both exact: the weights sum to zero to machine precision, and the
#' filter therefore annihilates any constant series and any linear trend (a
#' symmetric zero-sum kernel kills both, since sum a_h = 0 and sum h a_h = 0
#' by symmetry).  weight_sum reports the first.
#'
#' @param y the series, in time order.
#' @param p_low shortest cycle length passed, in periods; must exceed 1.
#' @param p_high longest cycle length passed, in periods.
#' @param K truncation half-width; the filter is a 2K+1 moving average and
#'   loses K observations at each end.
#' @return list: cycle, weights, weight_sum, n_valid, estimate, sd_cycle, K,
#'   n, method.
#' @keywords internal
#' @examples
#' Bxprfl(sin(1:120 / 4) + (1:120) / 10, 6, 32, 12)$weight_sum
#' @export
Bxprfl <- function(y, p_low = 6, p_high = 32, K = 12) {
  yy <- .s03vec(y)
  n <- length(yy)
  K <- as.integer(K)
  if (n < 2L * K + 1L) stop("baxter_king: series shorter than the 2K+1 filter window")
  a <- .bxprfl_weights(p_low, p_high, K)
  out <- rep(NA_real_, n)
  for (t in (K + 1L):(n - K)) {
    s <- 0
    for (h in (-K):K) s <- s + a[h + K + 1L] * yy[t - h]
    out[t] <- s
  }
  val <- out[!is.na(out)]
  list(cycle = out, weights = a, weight_sum = sum(a), n_valid = length(val),
       estimate = if (length(val)) .s03mean(val) else NaN,
       sd_cycle = if (length(val) > 1L) .s03sd(val, 1L) else NaN,
       K = K, n = n,
       method = "Baxter and King (1999) Rev. Econ. Statist. 81(4):575-593, eq. (8) constraint")
}

#' @noRd
.bxprfl_weights <- function(p_low, p_high, K) {
  if (!(p_low > 1)) stop("baxter_king: p_low must exceed 1 period")
  if (!(p_high > p_low)) stop("baxter_king: p_high must exceed p_low")
  K <- as.integer(K)
  if (K < 1L) stop("baxter_king: K must be at least 1")
  w_high <- 2 * pi / as.numeric(p_low)
  w_low <- 2 * pi / as.numeric(p_high)
  b <- numeric(K + 1L)
  b[1] <- (w_high - w_low) / pi
  for (h in seq_len(K)) b[h + 1L] <- (sin(h * w_high) - sin(h * w_low)) / (pi * h)
  tot <- b[1] + 2 * sum(b[-1])
  theta <- -tot / (2 * K + 1)
  vapply((-K):K, function(h) b[abs(h) + 1L] + theta, 0)
}

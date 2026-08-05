# SPDX-License-Identifier: AGPL-3.0-or-later
#' Bootstrap confidence interval for a Pearson correlation, on Fisher's z
#'
#' Efron and Tibshirani (1993), An Introduction to the Bootstrap, Chapman and
#' Hall, chapters 12-13 (the law-school correlation example), and Davison and
#' Hinkley (1997), Bootstrap Methods and their Application, Section 5.2 for
#' the percentile machinery.
#'
#' Pairs are resampled -- rows of (x, y) together, never the two vectors
#' independently, which would destroy the very dependence being estimated.
#' Each resample gives r*_b; the interval is built on the variance-stabilising
#' transform z*_b = atanh(r*_b) and mapped back with tanh,
#' [tanh(z*_(alpha/2)), tanh(z*_(1-alpha/2))].
#'
#' Because tanh is monotone the transform does not move the percentile
#' endpoints at all -- tanh(quantile) = quantile(tanh) -- so on its own it buys
#' nothing.  What it buys is a symmetric scale on which the normal interval is
#' worth building, returned alongside as lo_normal/hi_normal, with the z-scale
#' standard error reported so the difference is visible.
#'
#' Resampling is deterministic, from the Park-Miller generator
#' s <- 16807 s mod (2^31 - 1), so both language arms draw identical pairs.
#'
#' This module previously carried a body computing a Kolmogorov-Smirnov
#' statistic and a Spearman correlation, matching neither its name nor its
#' documentation; that body has been discarded.
#'
#' @param x,y the paired samples.
#' @param B number of resamples.
#' @param alpha two-sided error rate.
#' @param seed integer seed for the deterministic index generator.
#' @return list: lo, hi, estimate, r_hat, z_hat, z_se, lo_z, hi_z, lo_normal,
#'   hi_normal, B_used, B, n, method.
#' @keywords internal
#' @examples
#' Btcicor(c(1, 2, 3, 4, 5), c(2, 1, 4, 3, 5), B = 99)$r_hat
#' @export
Btcicor <- function(x, y, B = 999, alpha = 0.05, seed = 1) {
  xv <- .s03vec(x); yv <- .s03vec(y)
  n <- length(xv)
  if (n == 0L) stop("boot_ci_correlation: x is empty")
  if (length(yv) != n) stop("boot_ci_correlation: x and y have different lengths")
  if (n < 3L) stop("boot_ci_correlation: need at least three pairs")
  a <- as.numeric(alpha)
  if (!(a > 0 && a < 1)) stop("boot_ci_correlation: alpha must lie strictly between 0 and 1")
  Bn <- as.integer(B)
  if (Bn < 1L) stop("boot_ci_correlation: B must be at least one")
  r_hat <- .s03corr(xv, yv)
  s <- as.numeric(as.integer(seed)) %% 2147483647
  if (s <= 0) s <- s + 2147483646
  zs <- numeric(0); rs <- numeric(0)
  for (b in seq_len(Bn)) {
    xa <- numeric(n); ya <- numeric(n)
    for (i in seq_len(n)) {
      s <- (16807 * s) %% 2147483647
      u <- (s - 1) / 2147483646
      j <- floor(u * n)
      if (j >= n) j <- n - 1
      xa[i] <- xv[j + 1L]; ya[i] <- yv[j + 1L]
    }
    r <- .s03corr(xa, ya)
    if (is.na(r)) next
    if (r >= 1) r <- 1 - 1e-15
    if (r <= -1) r <- -1 + 1e-15
    rs <- c(rs, r)
    zs <- c(zs, 0.5 * log((1 + r) / (1 - r)))
  }
  if (length(zs) == 0L) stop("boot_ci_correlation: every resample was degenerate")
  lo_z <- .s03quantile7(zs, a / 2)
  hi_z <- .s03quantile7(zs, 1 - a / 2)
  zse <- if (length(zs) > 1L) .s03sd(zs, 1L) else NA_real_
  zc <- if (abs(r_hat) < 1) 0.5 * log((1 + r_hat) / (1 - r_hat)) else NA_real_
  q <- .s03qnorm(1 - a / 2)
  list(lo = tanh(lo_z), hi = tanh(hi_z), estimate = tanh(hi_z) - tanh(lo_z),
       r_hat = r_hat, z_hat = zc, z_se = zse, lo_z = lo_z, hi_z = hi_z,
       lo_normal = tanh(zc - q * zse), hi_normal = tanh(zc + q * zse),
       B_used = length(zs), B = Bn, n = n,
       method = "pairs bootstrap, percentile interval on atanh(r), mapped back by tanh")
}

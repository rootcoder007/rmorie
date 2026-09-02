# SPDX-License-Identifier: AGPL-3.0-or-later
#' Bootstrap two-sided p-value for H0: theta = theta0
#'
#' Davison and Hinkley (1997), Bootstrap Methods and their Application,
#' Cambridge University Press, Chapter 4.  The resampling must be done under
#' the null, not under the fitted model: the data are shifted so their
#' statistic equals theta0, x0_i = x_i - t + theta0, resampled with
#' replacement, and the two-sided p-value is
#' p = 2 min((1 + #\{T*_b >= T_hat\})/(B + 1), (1 + #\{T*_b <= T_hat\})/(B + 1)),
#' capped at one.  The +1 in numerator and denominator is Davison and Hinkley's
#' convention (Section 4.2): the observed statistic is itself one of the B + 1
#' values under the null, so a p-value of exactly zero -- which the raw
#' proportion would report the moment no replicate reaches T_hat -- is not
#' attainable, and the smallest reportable value is 2/(B + 1).  A p-value of
#' zero from a finite simulation is a statement the simulation cannot support.
#'
#' Shifting is the step that is easy to skip and fatal to skip: resampling the
#' unshifted data estimates the distribution of T around t, so the test would
#' compare T_hat against its own centre and return a p-value near one for
#' every data set no matter how far t is from theta0.  The degenerate anchor
#' checks exactly that -- with theta0 set to t the p-value must come out near
#' one, and with theta0 far away it must fall to the resolution floor 2/B.
#'
#' Resampling is deterministic, from the Park-Miller generator
#' s <- 16807 s mod (2^31 - 1), so both language arms see the same resamples.
#' The shift is additive, so it is exact only for location-equivariant
#' statistics; method says so.
#'
#' @param x the sample.
#' @param theta0 the null value.
#' @param stat optional statistic; defaults to the sample mean.
#' @param B number of resamples.
#' @param seed seed for the deterministic index generator.
#' @return list: p, estimate, T_hat, theta0, T_b, p_upper, p_lower, B, n,
#'   method.
#' @keywords internal
#' @examples
#' Btht(c(1, 2, 3, 4, 5), theta0 = 3, B = 99)$p
#' @export
Btht <- function(x, theta0 = 0, stat = NULL, B = 999, seed = 1) {
  v <- .s03vec(x)
  n <- length(v)
  if (n == 0L) stop("boot_test_hypothesis: x is empty")
  Bn <- as.integer(B)
  if (Bn < 1L) stop("boot_test_hypothesis: B must be at least one")
  f <- if (is.null(stat)) function(z) .s03mean(z) else stat
  t <- as.numeric(f(v))
  th0 <- as.numeric(theta0)
  shifted <- v - t + th0
  s <- as.numeric(as.integer(seed)) %% 2147483647
  if (s <= 0) s <- s + 2147483646
  reps <- numeric(Bn); ge <- 0L; le <- 0L
  for (b in seq_len(Bn)) {
    samp <- numeric(n)
    for (i in seq_len(n)) {
      s <- (16807 * s) %% 2147483647
      u <- (s - 1) / 2147483646
      j <- floor(u * n)
      if (j >= n) j <- n - 1
      samp[i] <- shifted[j + 1L]
    }
    tb <- as.numeric(f(samp))
    reps[b] <- tb
    if (tb >= t) ge <- ge + 1L
    if (tb <= t) le <- le + 1L
  }
  pge <- (1 + ge) / (Bn + 1); ple <- (1 + le) / (Bn + 1)
  p <- 2 * min(pge, ple)
  if (p > 1) p <- 1
  list(p = p, estimate = p, T_hat = t, theta0 = th0, T_b = reps, p_upper = pge,
       p_lower = ple, B = Bn, n = n,
       method = "null resampling of the shifted data, p = 2 min((1+#)/(B+1)); D&H Sect. 4.2")
}

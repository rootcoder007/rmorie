# SPDX-License-Identifier: AGPL-3.0-or-later
#' Bootstrap percentile confidence interval for a quantile
#'
#' Davison and Hinkley (1997), Bootstrap Methods and their Application,
#' Cambridge University Press, Sections 2.3 and 5.3.  Resample with
#' replacement, recompute the tau-quantile on each resample, and read the
#' alpha/2 and 1 - alpha/2 percentiles off the B replicates.
#'
#' Two things are deliberate.  The resampling is deterministic: indices come
#' from the Park-Miller multiplicative congruential generator
#' s <- 16807 s mod (2^31 - 1), so the Python and R arms draw the same
#' resamples and land on the same numbers, not merely the same distribution;
#' the arithmetic stays exact in a double because 16807 * (2^31 - 2) < 2^53.
#' And the sample quantile of a resample is lattice-valued -- it can only take
#' values present in the data -- so its bootstrap distribution is discrete and
#' the endpoints are themselves data values.  For extreme tau and small n the
#' interval degenerates to a point; n_distinct makes that visible rather than
#' letting it pass for precision.
#'
#' @param x the sample.
#' @param tau the quantile level in [0, 1].
#' @param B number of resamples.
#' @param alpha two-sided error rate.
#' @param seed integer seed for the deterministic index generator.
#' @return list: lo, hi, estimate, q_hat, theta_b, n_distinct, B, n, tau,
#'   method.
#' @keywords internal
#' @examples
#' Btciqua(c(1, 2, 3, 4, 5, 6, 7, 8), 0.5, B = 99)$lo
#' @export
Btciqua <- function(x, tau = 0.5, B = 999, alpha = 0.05, seed = 1) {
  v <- .s03vec(x)
  n <- length(v)
  if (n == 0L) stop("boot_ci_quantile: x is empty")
  t <- as.numeric(tau)
  if (!(t >= 0 && t <= 1)) stop("boot_ci_quantile: tau must lie in [0, 1]")
  a <- as.numeric(alpha)
  if (!(a > 0 && a < 1)) stop("boot_ci_quantile: alpha must lie strictly between 0 and 1")
  Bn <- as.integer(B)
  if (Bn < 1L) stop("boot_ci_quantile: B must be at least one")
  s <- .btlcg_seed(seed)
  reps <- numeric(Bn)
  for (b in seq_len(Bn)) {
    samp <- numeric(n)
    for (i in seq_len(n)) {
      s <- (16807 * s) %% 2147483647
      u <- (s - 1) / 2147483646
      j <- floor(u * n)
      if (j >= n) j <- n - 1
      samp[i] <- v[j + 1L]
    }
    reps[b] <- .s03quantile7(samp, t)
  }
  lo <- .s03quantile7(reps, a / 2)
  hi <- .s03quantile7(reps, 1 - a / 2)
  list(lo = lo, hi = hi, estimate = hi - lo, q_hat = .s03quantile7(v, t), theta_b = reps,
       n_distinct = length(unique(reps)), B = Bn, n = n, tau = t,
       method = "percentile interval on B deterministic resample quantiles (Park-Miller indices)")
}

#' @noRd
.btlcg_seed <- function(seed) {
  s <- as.numeric(as.integer(seed)) %% 2147483647
  if (s <= 0) s <- s + 2147483646
  s
}

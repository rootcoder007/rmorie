# SPDX-License-Identifier: AGPL-3.0-or-later
#' Bootstrap percentile confidence interval for the median
#'
#' Efron, B. (1979), "Bootstrap methods: another look at the jackknife",
#' \emph{The Annals of Statistics} 7(1), 1-26, doi:10.1214/aos/1176344552,
#' p. 3, steps 1-3, read from the Project Euclid PDF rendered as page images;
#' Section 3 of that paper is the sample-median example, the case where the
#' ordinary jackknife fails and the bootstrap does not.  Here R(X*, F-hat) is
#' the median of the bootstrap sample, and the interval is the alpha/2 and
#' 1 - alpha/2 quantiles of its distribution, taken with the type-7 rule that
#' R's own quantile() uses by default.
#'
#' With exhaustive the median's bootstrap distribution is computed completely
#' rather than sampled.  For n = 3 that distribution is exactly 7/27, 13/27,
#' 7/27 on the three order statistics -- a resample of three draws has its
#' median at x_(1) exactly when two or more draws are index 1, which happens
#' in 3*2 + 1 = 7 of the 27 ordered triples, and symmetrically at x_(3) -- and
#' that count is the anchor for this module.
#'
#' @param x The sample.
#' @param B Replications when not enumerating.
#' @param alpha Two-sided level; the interval is (alpha/2, 1 - alpha/2).
#' @param rng Base offset of the Halton design.
#' @param exhaustive Enumerate all n^n resamples (n <= 6).
#' @return list: estimate (median of the data), lo, hi, medians, alpha, B, n,
#'   exhaustive, method.
#' @keywords internal
#' @examples
#' Btcimed(c(3, 7, 10), exhaustive = TRUE)$lo
#' @export
Btcimed <- function(x, B = 200L, alpha = 0.05, rng = 2L, exhaustive = FALSE) {
  v <- .s03vec(x)
  n <- length(v)
  if (n == 0L) stop("boot_ci_median: x is empty")
  a <- as.numeric(alpha)
  if (!(a > 0 && a < 1)) {
    stop("boot_ci_median: alpha must lie strictly between 0 and 1")
  }
  B <- as.integer(B)
  if (!exhaustive && B < 1L) stop("boot_ci_median: B must be at least 1")
  rng <- as.integer(rng)
  if (rng < 2L) stop("boot_ci_median: rng must be a base of at least 2")
  cs <- .bt_counts(n, B, rng, isTRUE(exhaustive))
  o <- order(v)
  nb <- nrow(cs)
  meds <- numeric(nb)
  for (b in seq_len(nb)) {
    rs <- numeric(0)
    for (i in o) if (cs[b, i] > 0) rs <- c(rs, rep(v[i], cs[b, i]))
    meds[b] <- .bt_median(rs)
  }
  sm <- sort(meds)
  list(estimate = .bt_median(sort(v)), lo = .s03quantile7(sm, a / 2),
       hi = .s03quantile7(sm, 1 - a / 2), medians = meds, alpha = a,
       B = nb, n = n, exhaustive = isTRUE(exhaustive),
       method = "Bootstrap CI for the median")
}

.bt_median <- function(sorted_vals) {
  m <- length(sorted_vals)
  h <- m %/% 2L
  if (m %% 2L == 1L) sorted_vals[h + 1L] else 0.5 * (sorted_vals[h] + sorted_vals[h + 1L])
}

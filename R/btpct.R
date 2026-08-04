# SPDX-License-Identifier: AGPL-3.0-or-later
#' Percentile bootstrap confidence interval
#'
#' Efron (1979), Bootstrap methods: another look at the jackknife, Annals
#' of Statistics 7(1), 1-26, and Efron and Tibshirani (1993), An
#' Introduction to the Bootstrap, chapter 13: the percentile interval is
#' the alpha/2 and 1 - alpha/2 quantiles of the replicates.  Neither was
#' retrievable here as a full text; the interval is quoted in its standard
#' published form.  It is NOT second-order accurate when the replicate
#' distribution is skewed or biased, so the bias-corrected variant (their
#' chapter 14) is returned alongside and the discrepancy is visible.
#' Quantiles use type 7 in both arms.
#'
#' @param theta_b bootstrap replicates.
#' @param alpha 1 - coverage.
#' @param theta_hat the original estimate, for the bias correction.
#' @return list: lo, hi, estimate, bc_lo, bc_hi, z0, n, method.
#' @keywords internal
#' @examples
#' Bootpct(c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10), 0.2)$lo
#' @export
Bootpct <- function(theta_b, alpha = 0.05, theta_hat = NULL) {
  v <- .s03vec(theta_b); n <- length(v); a <- as.numeric(alpha)
  lo <- .s03quantile7(v, a / 2); hi <- .s03quantile7(v, 1 - a / 2)
  th <- if (!is.null(theta_hat)) as.numeric(theta_hat) else .s03mean(v)
  cnt <- 0
  for (x in v) if (x < th) cnt <- cnt + 1
  p <- if (n) cnt / n else 0.5
  if (p <= 0) p <- 0.5 / n
  if (p >= 1) p <- 1 - 0.5 / n
  z0 <- qnorm(p); za <- qnorm(a / 2); zb <- qnorm(1 - a / 2)
  list(lo = lo, hi = hi, estimate = hi - lo,
       bc_lo = .s03quantile7(v, pnorm(2 * z0 + za)),
       bc_hi = .s03quantile7(v, pnorm(2 * z0 + zb)), z0 = z0, n = n,
       method = "Efron (1979) percentile bootstrap interval, with the bias-corrected variant")
}

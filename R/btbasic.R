# SPDX-License-Identifier: AGPL-3.0-or-later
#' Basic (reverse-percentile) bootstrap confidence interval
#'
#' Davison and Hinkley (1997), Bootstrap Methods and their Application,
#' Cambridge University Press, Section 5.2, limits (2.10)/(5.6).  The interval
#' inverts the bootstrap distribution of T - theta rather than reading
#' quantiles of T* directly: [2t - t*_(1-alpha/2), 2t - t*_(alpha/2)].
#'
#' The reversal is the whole point.  If the replicates are skewed to the right
#' the percentile interval leans right and the basic interval leans left,
#' because the quantity being inverted is the error t* - t, not the estimate.
#' Getting the direction wrong silently produces an interval of the correct
#' width on the wrong side, which no width check would catch, so the anchor
#' pins the reflection about t and not merely the length.  Quantiles are R's
#' type 7 in both language arms, computed by the shared helper.
#'
#' @param theta_hat the estimate on the original data.
#' @param theta_b the bootstrap replicates.
#' @param alpha two-sided error rate; nominal coverage is 1 - alpha.
#' @return list: lo, hi, estimate, q_lo, q_hi, theta_hat, B, method.
#' @keywords internal
#' @examples
#' Btbasic(5, c(4, 4.5, 5, 5.5, 6, 6.5, 7))$lo
#' @export
Btbasic <- function(theta_hat, theta_b, alpha = 0.05) {
  v <- .s03vec(theta_b)
  n <- length(v)
  if (n == 0L) stop("boot_basic_ci: no bootstrap replicates")
  a <- as.numeric(alpha)
  if (!(a > 0 && a < 1)) stop("boot_basic_ci: alpha must lie strictly between 0 and 1")
  t <- as.numeric(theta_hat)
  qlo <- .s03quantile7(v, a / 2)
  qhi <- .s03quantile7(v, 1 - a / 2)
  lo <- 2 * t - qhi
  hi <- 2 * t - qlo
  list(lo = lo, hi = hi, estimate = hi - lo, q_lo = qlo, q_hi = qhi, theta_hat = t, B = n,
       method = "Davison and Hinkley (1997) basic bootstrap limits, 2t - t*_(1-a/2), 2t - t*_(a/2)")
}

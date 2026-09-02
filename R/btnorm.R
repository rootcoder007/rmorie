# SPDX-License-Identifier: AGPL-3.0-or-later
#' Normal-approximation bootstrap confidence interval
#'
#' Davison and Hinkley (1997), Bootstrap Methods and their Application,
#' Cambridge University Press, Section 5.2.  The bootstrap supplies a bias
#' estimate and a standard error and the interval is the ordinary normal one
#' built from them: bias* = mean(t*) - t, se* = sd(t*), and
#' \[t - bias* - z_(1-alpha/2) se*, t - bias* + z_(1-alpha/2) se*\].
#'
#' The bias is subtracted, not added: mean(t*) - t estimates E(T) - theta, so
#' the corrected estimate is t - bias*.  The interval assumes the replicate
#' distribution is normal and is the first thing to abandon when it is not;
#' the returned skew, the standardised third moment of the replicates, makes
#' that assumption visible rather than implicit.  sd(t*) uses the n-1 divisor,
#' matching stats::sd, which is the anchor.
#'
#' @param theta_hat the estimate on the original data.
#' @param theta_b the bootstrap replicates.
#' @param alpha two-sided error rate.
#' @return list: lo, hi, estimate, bias, se_b, centre, skew, z, B, method.
#' @keywords internal
#' @examples
#' Btnorm(5, c(4, 4.5, 5, 5.5, 6, 6.5, 7))$se_b
#' @export
Btnorm <- function(theta_hat, theta_b, alpha = 0.05) {
  v <- .s03vec(theta_b)
  n <- length(v)
  if (n < 2L) stop("boot_normal_ci: need at least two bootstrap replicates")
  a <- as.numeric(alpha)
  if (!(a > 0 && a < 1)) stop("boot_normal_ci: alpha must lie strictly between 0 and 1")
  t <- as.numeric(theta_hat)
  m <- .s03mean(v)
  se <- .s03sd(v, 1L)
  bias <- m - t
  z <- .s03qnorm(1 - a / 2)
  centre <- t - bias
  m2 <- 0; m3 <- 0
  for (x in v) {
    d <- x - m
    m2 <- m2 + d * d
    m3 <- m3 + d * d * d
  }
  m2 <- m2 / n; m3 <- m3 / n
  skew <- if (m2 > 0) m3 / m2^1.5 else NA_real_
  list(lo = centre - z * se, hi = centre + z * se, estimate = 2 * z * se, bias = bias,
       se_b = se, centre = centre, skew = skew, z = z, B = n,
       method = "t - bias* +/- z_{1-a/2} se*, Davison and Hinkley (1997) Sect. 5.2")
}

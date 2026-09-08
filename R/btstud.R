# SPDX-License-Identifier: AGPL-3.0-or-later
#' Studentised (bootstrap-t) confidence interval
#'
#' Davison and Hinkley (1997), Bootstrap Methods and their Application,
#' Cambridge University Press, Section 5.2, which defines the studentised
#' statistic z* = (t* - t)/v*^(1/2) and gives the limits
#' \[t - z*_(1-alpha/2) v^(1/2), t - z*_(alpha/2) v^(1/2)\].  Also Hall (1988),
#' "Theoretical comparison of bootstrap confidence intervals", The Annals of
#' Statistics 16(3), 927-953, doi:10.1214/aos/1176350933, the second-order
#' accuracy result that motivates studentising in the first place.
#'
#' Like the basic interval this one is reversed: the upper quantile of z* sets
#' the lower endpoint.  Feeding it the raw replicates t* instead of the
#' studentised z* is the classic misuse and yields an interval that looks
#' plausible and is wrong, so the function takes t_b and se_hat separately and
#' never divides for the caller.
#'
#' @param theta_hat estimate on the original data.
#' @param se_hat its standard error on the original data; must be positive.
#' @param t_b the studentised replicates (t*_b - t)/se*_b.
#' @param alpha two-sided error rate.
#' @return list: lo, hi, estimate, z_lo, z_hi, se_hat, theta_hat, B, method.
#' @keywords internal
#' @examples
#' Btstud(5, 1, c(-2, -1, 0, 1, 2))$lo
#' @export
Btstud <- function(theta_hat, se_hat, t_b, alpha = 0.05) {
  v <- .s03vec(t_b)
  n <- length(v)
  if (n == 0L) stop("boot_studentized_ci: no studentised replicates")
  a <- as.numeric(alpha)
  if (!(a > 0 && a < 1)) stop("boot_studentized_ci: alpha must lie strictly between 0 and 1")
  s <- as.numeric(se_hat)
  if (!(s > 0)) stop("boot_studentized_ci: se_hat must be positive")
  t <- as.numeric(theta_hat)
  zlo <- .s03quantile7(v, a / 2)
  zhi <- .s03quantile7(v, 1 - a / 2)
  list(lo = t - zhi * s, hi = t - zlo * s, estimate = (t - zlo * s) - (t - zhi * s),
       z_lo = zlo, z_hi = zhi, se_hat = s, theta_hat = t, B = n,
       method = "t - z*_(1-a/2) se, t - z*_(a/2) se; Davison and Hinkley (1997) Sect. 5.2")
}

# SPDX-License-Identifier: AGPL-3.0-or-later
#' Standardise by the control group's spread, not the pooled spread
#'
#' Pooling the two standard deviations assumes the treatment changed the
#' mean and left the variance alone. When the treatment also changes the
#' spread -- the usual case for anything that helps some people and not
#' others -- the pooled denominator is itself an effect of the treatment,
#' and the standardised difference is no longer comparable across studies.
#' Glass's delta uses the control spread, which the treatment cannot have
#' touched.
#'
#' Formula: \code{Delta = (m1 - m2)/s_ctrl} with
#' \code{Var(Delta) = (n1 + n2)/(n1 n2) + Delta^2/(2 (n2 - 1))}, \code{n2}
#' being the control group -- Glass, McGaw and Smith (1981), Chapter 5;
#' variance from Hedges and Olkin (1985) eq. (5.10).
#'
#' @param m1,m2 Treatment and control means.
#' @param s_ctrl Control-group standard deviation, strictly positive.
#' @param n1,n2 Treatment and control sample sizes; \code{n2 >= 2}.
#' @return List with \code{delta}, \code{var}, \code{se}, \code{ci_lo},
#'   \code{ci_hi}, \code{n1}, \code{n2}.
#' @references Glass, G. V., McGaw, B. and Smith, M. L. (1981).
#'   Meta-Analysis in Social Research. Sage, Chapter 5. Hedges, L. V. and
#'   Olkin, I. (1985). Statistical Methods for Meta-Analysis, eq. (5.10).
#' @export
Magsd <- function(m1, m2, s_ctrl, n1, n2) {
  s <- as.numeric(s_ctrl)
  a <- as.numeric(n1)
  b <- as.numeric(n2)
  if (s <= 0) stop("the control standard deviation must be positive")
  if (a < 1 || b < 2) stop("need n1 >= 1 and n2 >= 2")
  d <- (as.numeric(m1) - as.numeric(m2)) / s
  v <- (a + b) / (a * b) + d^2 / (2 * (b - 1))
  se <- sqrt(v)
  .t1_result(delta = d, var = v, se = se,
             ci_lo = d - 1.959963984540054 * se,
             ci_hi = d + 1.959963984540054 * se,
             n1 = a, n2 = b, method = "Glass's delta")
}

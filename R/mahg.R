# SPDX-License-Identifier: AGPL-3.0-or-later
#' Hedges g, the bias-corrected standardised mean difference
#'
#' g_U = c(m) (m1 - m2) / s_pooled with the exact correction factor
#' c(m) = Gamma(m/2) / (sqrt(m/2) Gamma((m-1)/2)), m = n1 + n2 - 2, which the
#' paper shows gives the unique minimum-variance unbiased estimator; the
#' algebraic approximation 1 - 3/(4m - 1) is reported but not used.  Source
#' consulted: Hedges (1981), Distribution theory for Glass estimator of effect
#' size and related estimators, Journal of Educational Statistics 6(2),
#' 107-128, equation (6e).
#'
#' @param m1,m2 group means.
#' @param s1,s2 group standard deviations (divisor n - 1).
#' @param n1,n2 group sizes.
#' @return list: estimate, d, J, J_approx, se, variance, df, s_pooled, n, method.
#' @keywords internal
#' @examples
#' mahg(5, 5, 1, 1, 10, 10)
#' @export
mahg <- function(m1, m2, s1, s2, n1, n2) {
  n1f <- as.numeric(n1)
  n2f <- as.numeric(n2)
  df <- n1f + n2f - 2
  sp <- sqrt(((n1f - 1) * s1^2 + (n2f - 1) * s2^2) / df)
  d <- (m1 - m2) / sp
  jexact <- exp(lgamma(df / 2) - 0.5 * log(df / 2) - lgamma((df - 1) / 2))
  japprox <- 1 - 3 / (4 * df - 1)
  g <- jexact * d
  varr <- (n1f + n2f) / (n1f * n2f) + g^2 / (2 * (n1f + n2f))
  list(estimate = as.numeric(g), d = as.numeric(d), J = as.numeric(jexact),
       J_approx = as.numeric(japprox), se = sqrt(varr), variance = as.numeric(varr),
       df = as.numeric(df), s_pooled = as.numeric(sp),
       n = as.integer(n1 + n2),
       method = "Hedges g, bias-corrected standardised mean difference (Hedges 1981)")
}

# CANONICAL TEST
# r <- mahg(5, 5, 1, 1, 10, 10)
# stopifnot(abs(r$estimate) < 1e-15, abs(r$J - r$J_approx) < 1e-4)

#' @rdname mahg
#' @keywords internal
#' @export
morie_ma_hedges_g <- mahg

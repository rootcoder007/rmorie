# SPDX-License-Identifier: AGPL-3.0-or-later
#' Higgins-Thompson H^2 heterogeneity ratio
#'
#' H^2 = Q / (k - 1), equal to one under homogeneity, with the same SE(log H)
#' as the I^2 interval.  Source consulted: Higgins and Thompson (2002),
#' Statistics in Medicine 21, 1539-1558, equations (6)-(8).  Verified against
#' metafor::rma.
#'
#' @param yi study effect sizes.
#' @param vi within-study sampling variances.
#' @param level confidence level.
#' @return list: estimate, H, se, ci_lower, ci_upper, Q, df, p_Q, n, method.
#' @keywords internal
#' @examples
#' math2(c(0.1, 0.3, -0.2, 0.45), c(0.02, 0.05, 0.03, 0.08))$estimate
#' @export
math2 <- function(yi, vi, level = 0.95) {
  y <- as.numeric(yi); v <- as.numeric(vi)
  fe <- k02fe(y, v)
  k <- length(y); q <- fe$Q; df <- fe$df
  h2 <- q / df
  h <- sqrt(h2)
  selnh <- if (q > k) {
    (log(q) - log(df)) / (2 * (sqrt(2 * q) - sqrt(2 * k - 3)))
  } else {
    sqrt((1 / (2 * (k - 2))) * (1 - 1 / (3 * (k - 2)^2)))
  }
  crit <- k02z(0.5 + 0.5 * level)
  hlo <- max(1, exp(log(h) - crit * selnh))
  hhi <- exp(log(h) + crit * selnh)
  list(estimate = h2, H = h, se = selnh, ci_lower = hlo^2, ci_upper = hhi^2,
       Q = q, df = as.integer(df), p_Q = k02pchi(q, df), n = k,
       method = "H^2 heterogeneity ratio (Higgins & Thompson 2002, eq. 6-8)")
}

# CANONICAL TEST
# r <- math2(c(0.10,0.30,-0.20,0.45,0.05,0.22), c(0.02,0.05,0.03,0.08,0.01,0.04))
# stopifnot(abs(r$estimate - 1.17733737024221) < 1e-12)

#' @rdname math2
#' @keywords internal
#' @export
morie_math2 <- math2

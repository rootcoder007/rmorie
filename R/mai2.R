# SPDX-License-Identifier: AGPL-3.0-or-later
#' Higgins-Thompson I^2 inconsistency statistic
#'
#' I^2 = 100 (Q - (k-1)) / Q truncated at zero, with the confidence interval
#' obtained from the interval for H via I^2 = (H^2 - 1)/H^2 and
#' SE(log H) = (log Q - log(k-1)) / (2 (sqrt(2Q) - sqrt(2k-3))) when Q > k,
#' else sqrt((1/(2(k-2))) (1 - 1/(3 (k-2)^2))).  Source consulted: Higgins and
#' Thompson (2002), Statistics in Medicine 21, 1539-1558, equations (7)-(9).
#' Verified against metafor::rma.
#'
#' @param yi study effect sizes.
#' @param vi within-study sampling variances.
#' @param level confidence level.
#' @return list: estimate, se, ci_lower, ci_upper, Q, df, p_Q, H, n, method.
#' @keywords internal
#' @examples
#' mai2(c(0.1, 0.3, -0.2, 0.45), c(0.02, 0.05, 0.03, 0.08))$estimate
#' @export
mai2 <- function(yi, vi, level = 0.95) {
  y <- as.numeric(yi)
  v <- as.numeric(vi)
  fe <- k02fe(y, v)
  k <- length(y)
  q <- fe$Q
  df <- fe$df
  i2 <- 100 * max(0, (q - df) / q)
  h <- sqrt(q / df)
  selnh <- if (q > k) {
    (log(q) - log(df)) / (2 * (sqrt(2 * q) - sqrt(2 * k - 3)))
  } else {
    sqrt((1 / (2 * (k - 2))) * (1 - 1 / (3 * (k - 2)^2)))
  }
  crit <- k02z(0.5 + 0.5 * level)
  hlo <- max(1, exp(log(h) - crit * selnh))
  hhi <- exp(log(h) + crit * selnh)
  list(estimate = i2, se = selnh, ci_lower = 100 * (hlo^2 - 1) / hlo^2,
       ci_upper = 100 * (hhi^2 - 1) / hhi^2, Q = q, df = as.integer(df),
       p_Q = k02pchi(q, df), H = h, n = k,
       method = "I^2 inconsistency with Higgins-Thompson interval (Higgins & Thompson 2002, eq. 9)")
}

# CANONICAL TEST
# r <- mai2(c(0.10,0.30,-0.20,0.45,0.05,0.22), c(0.02,0.05,0.03,0.08,0.01,0.04))
# stopifnot(abs(r$estimate - 15.0625788940795) < 1e-11)

#' @rdname mai2
#' @keywords internal
#' @export
morie_mai2 <- mai2

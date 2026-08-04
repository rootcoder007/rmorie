# SPDX-License-Identifier: AGPL-3.0-or-later
#' Risk difference and its variance for a 2x2 table
#'
#' RD = a/n1 - c/n2 with Var = a b / n1^3 + c d / n2^3, the sum of the two
#' binomial variances.  Source consulted: Borenstein, Hedges, Higgins and
#' Rothstein (2009), equations (5.5)-(5.6).  Verified against
#' metafor::escalc(measure = "RD").
#'
#' @param ai,bi events and non-events in the treated arm.
#' @param ci,di events and non-events in the control arm.
#' @param level confidence level.
#' @return list: estimate, se, variance, risk1, risk2, ci_lower, ci_upper,
#'   n, method.
#' @keywords internal
#' @examples
#' mard(12, 38, 7, 43)$estimate
#' @export
mard <- function(ai, bi, ci, di, level = 0.95) {
  a <- as.numeric(ai); b <- as.numeric(bi); cc <- as.numeric(ci); d <- as.numeric(di)
  n1 <- a + b; n2 <- cc + d
  p1 <- a / n1; p2 <- cc / n2
  yi <- p1 - p2
  vi <- a * b / n1^3 + cc * d / n2^3
  se <- sqrt(vi)
  crit <- k02z(0.5 + 0.5 * level)
  list(estimate = yi, se = se, variance = vi, risk1 = p1, risk2 = p2,
       ci_lower = yi - crit * se, ci_upper = yi + crit * se, n = length(yi),
       method = "Risk difference (Borenstein et al. 2009, eq. 5.5-5.6)")
}

# CANONICAL TEST
# r <- mard(12, 38, 7, 43)
# stopifnot(abs(r$variance - 0.006056) < 1e-15)

#' @rdname mard
#' @keywords internal
#' @export
morie_mard <- mard

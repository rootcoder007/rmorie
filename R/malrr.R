# SPDX-License-Identifier: AGPL-3.0-or-later
#' Log risk ratio and its variance for a 2x2 table
#'
#' ln(RR) = log((a/n1)/(c/n2)) with Var = 1/a - 1/n1 + 1/c - 1/n2,
#' n1 = a + b and n2 = c + d.  Zero-cell tables receive the continuity
#' correction on every cell.  Source consulted: Borenstein, Hedges, Higgins
#' and Rothstein (2009), equations (5.1)-(5.2).  Verified against
#' metafor::escalc(measure = "RR").
#'
#' @param ai,bi events and non-events in the treated arm.
#' @param ci,di events and non-events in the control arm.
#' @param add continuity correction for zero cells.
#' @param level confidence level.
#' @return list: estimate, se, variance, risk_ratio, ci_lower, ci_upper,
#'   corrected, n, method.
#' @keywords internal
#' @examples
#' malrr(12, 38, 7, 43)$estimate
#' @export
malrr <- function(ai, bi, ci, di, add = 0.5, level = 0.95) {
  a <- as.numeric(ai); b <- as.numeric(bi); cc <- as.numeric(ci); d <- as.numeric(di)
  zero <- (a == 0) | (b == 0) | (cc == 0) | (d == 0)
  adj <- ifelse(zero, add, 0)
  a <- a + adj; b <- b + adj; cc <- cc + adj; d <- d + adj
  n1 <- a + b; n2 <- cc + d
  yi <- log((a / n1) / (cc / n2))
  vi <- 1 / a - 1 / n1 + 1 / cc - 1 / n2
  se <- sqrt(vi)
  crit <- k02z(0.5 + 0.5 * level)
  list(estimate = yi, se = se, variance = vi, risk_ratio = exp(yi),
       ci_lower = yi - crit * se, ci_upper = yi + crit * se,
       corrected = as.integer(sum(zero)), n = length(yi),
       method = "Log risk ratio (Borenstein et al. 2009, eq. 5.1-5.2)")
}

# CANONICAL TEST
# r <- malrr(12, 38, 7, 43)
# stopifnot(abs(r$variance - 0.186190476190476) < 1e-13)

#' @rdname malrr
#' @keywords internal
#' @export
morie_malrr <- malrr

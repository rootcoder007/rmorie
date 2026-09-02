# SPDX-License-Identifier: AGPL-3.0-or-later
#' Log odds ratio and Woolf's variance for a 2x2 table
#'
#' ln(OR) = log(a d / (b c)) with Var = 1/a + 1/b + 1/c + 1/d.  A continuity
#' correction is added to every cell of any table containing a zero.  Source
#' consulted: Borenstein, Hedges, Higgins and Rothstein (2009), Introduction
#' to Meta-Analysis, equations (5.8)-(5.9); Woolf (1955).  Verified against
#' metafor::escalc(measure = "OR").
#'
#' @param ai,bi events and non-events in the treated arm.
#' @param ci,di events and non-events in the control arm.
#' @param add continuity correction for zero cells.
#' @param level confidence level.
#' @return list: estimate, se, variance, odds_ratio, ci_lower, ci_upper,
#'   corrected, n, method.
#' @keywords internal
#' @examples
#' malor(12, 38, 7, 43)$estimate
#' @export
malor <- function(ai, bi, ci, di, add = 0.5, level = 0.95) {
  a <- as.numeric(ai)
  b <- as.numeric(bi)
  cc <- as.numeric(ci)
  d <- as.numeric(di)
  zero <- (a == 0) | (b == 0) | (cc == 0) | (d == 0)
  adj <- ifelse(zero, add, 0)
  a <- a + adj
  b <- b + adj
  cc <- cc + adj
  d <- d + adj
  yi <- log(a * d / (b * cc))
  vi <- 1 / a + 1 / b + 1 / cc + 1 / d
  se <- sqrt(vi)
  crit <- k02z(0.5 + 0.5 * level)
  list(estimate = yi, se = se, variance = vi, odds_ratio = exp(yi),
       ci_lower = yi - crit * se, ci_upper = yi + crit * se,
       corrected = as.integer(sum(zero)), n = length(yi),
       method = "Log odds ratio with Woolf variance (Borenstein et al. 2009, eq. 5.8-5.9)")
}

# CANONICAL TEST
# r <- malor(12, 38, 7, 43)
# stopifnot(abs(r$estimate - 0.662610456699864) < 1e-13)

#' @rdname malor
#' @keywords internal
#' @export
morie_malor <- malor

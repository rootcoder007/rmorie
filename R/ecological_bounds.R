# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Research P12: Duncan-Davis bounds (research/lean/P12Bounds.lean; Duncan & Davis 1953; Manski sec. 5.1).
#
#   Research.P12.pq_ge                   the joint mass is at least p + q - 1 (inclusion-exclusion)
#   Research.P12.dd_bounds               max(0, (p+q-1)/p) <= P(y | x) <= min(1, q/p)
#   Research.P12.Cells.ends_attained     both ends are reached by admissible joint tables
#   Research.P12.dd_complement           q = p r + (1 - p) r'  pins the other group's rate
#   Research.P12.dd_aggregate_bounds     the aggregate rate is the m_g p_g-weighted mean, so its bounds are too

#' Duncan-Davis bounds: what neighbourhood marginals say about an individual rate
#'
#' Each neighbourhood reports the share \code{p} of residents with a trait and
#' the share \code{q} with an outcome. The rate of the outcome among trait
#' holders, \eqn{r = P(y \mid x)}, is identified from the two marginals only up
#' to \eqn{[\max(0, (p+q-1)/p),\ \min(1, q/p)]}, both ends attainable
#' (\code{Research.P12.dd_bounds}, \code{Cells.ends_attained}). The rate among
#' the others follows from \eqn{q = p r + (1-p) r'} (\code{dd_complement}),
#' and the city-wide rate, a population-weighted mean of the neighbourhood
#' rates, inherits the weighted mean of the intervals
#' (\code{dd_aggregate_bounds}). An ecological regression that reports a
#' single number for \eqn{r} is choosing a point inside these intervals by
#' assumption, not by data.
#' @param p Trait share per neighbourhood, in (0, 1).
#' @param q Outcome share per neighbourhood, in \[0, 1\].
#' @param weights Population per neighbourhood (positive); equal by default.
#' @return A list with \code{neighbourhoods} (a data frame with \code{p},
#'   \code{q}, \code{lower}, \code{upper}, \code{width}, \code{point_identified},
#'   \code{complement_lower}, \code{complement_upper}), \code{aggregate}
#'   (\code{lower}, \code{upper}, \code{width}, the city-wide rate among trait
#'   holders) and \code{theorems}.
#' @examples
#' morie_ecological_bounds(p = c(0.2, 0.5, 0.8), q = c(0.1, 0.3, 0.6), weights = c(1000, 2000, 500))
#' @export
morie_ecological_bounds <- function(p, q, weights = NULL) {
  n <- length(p)
  if (length(q) != n) stop("p and q must have equal length", call. = FALSE)
  if (anyNA(p) || anyNA(q) || any(p <= 0 | p >= 1) || any(q < 0 | q > 1)) stop("p must lie in (0, 1) and q in [0, 1]", call. = FALSE)
  if (is.null(weights)) weights <- rep(1, n)
  if (length(weights) != n || any(weights <= 0)) stop("weights must be positive, one per neighbourhood", call. = FALSE)
  lower <- pmax(0, (p + q - 1) / p)
  upper <- pmin(1, q / p)
  comp_lower <- (q - p * upper) / (1 - p)
  comp_upper <- (q - p * lower) / (1 - p)
  m <- weights * p
  agg <- c(lower = sum(m * lower) / sum(m), upper = sum(m * upper) / sum(m))
  list(
    neighbourhoods = data.frame(p = p, q = q, lower = lower, upper = upper, width = upper - lower,
                                point_identified = abs(upper - lower) < 1e-12,
                                complement_lower = comp_lower, complement_upper = comp_upper),
    aggregate = c(agg, width = unname(agg["upper"] - agg["lower"])),
    theorems = c("Research.P12.pq_ge", "Research.P12.dd_bounds", "Research.P12.Cells.ends_attained",
                 "Research.P12.dd_complement", "Research.P12.dd_aggregate_bounds")
  )
}

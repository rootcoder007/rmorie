# SPDX-License-Identifier: AGPL-3.0-or-later
#' L2 distance between two functional observations
#'
#' Ramsay and Silverman (2005), Functional Data Analysis, 2nd ed., Springer:
#' the inner product is <f, g> = integral f g and the induced metric is
#' d(f, g) = sqrt(integral (f - g)^2).
#'
#' The integral is taken over the WHOLE observation interval by the composite
#' trapezoid rule.  This is load-bearing: a sibling module once integrated
#' over [a+h, b-h], dropping both end intervals, and returned 3.8667 where
#' the closed form is 4.
#'
#' @param f,g the two curves, sampled at common points.
#' @param t sampling grid; defaults to equally spaced on [0, 1].
#' @return list: estimate, l2sq, l1, sup, n, method.
#' @keywords internal
#' @examples
#' FnDist(c(1, 1, 1), c(0, 0, 0))$estimate
#' @export
FnDist <- function(f, g, t = NULL) {
  ff <- .s03vec(f)
  gg <- .s03vec(g)
  n <- length(ff)
  if (n == 0L) stop("functional_distance: f is empty")
  if (length(gg) != n) stop("functional_distance: f and g must have the same length")
  if (n < 2L) stop("functional_distance: need at least two sampling points")
  tt <- if (is.null(t)) .fdgrid(n) else .s03vec(t)
  if (length(tt) != n) stop("functional_distance: t must match the curve length")
  d <- ff - gg
  l2sq <- .fdtrapz(tt, d * d)
  list(estimate = if (l2sq > 0) sqrt(l2sq) else 0,
       l2sq = l2sq, l1 = .fdtrapz(tt, abs(d)), sup = max(abs(d)), n = n,
       method = "Ramsay-Silverman (2005) L2 metric, composite trapezoid over the whole interval")
}

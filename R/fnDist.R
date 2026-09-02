# SPDX-License-Identifier: AGPL-3.0-or-later
#' L2 distance between two functional observations
#'
#' Ramsay and Silverman (2005), Functional Data Analysis, 2nd ed., Springer:
#' the inner product on the function space is <f, g> = integral f(t) g(t) dt
#' and the induced metric is d(f, g) = sqrt(integral (f(t) - g(t))^2 dt).
#'
#' The integral is taken over the WHOLE observation interval by the composite
#' trapezoid rule.  This is deliberate and load-bearing: a sibling module in
#' this package once integrated over \[a+h, b-h\], dropping both end intervals,
#' and returned 3.8667 where the closed form is 4.
#'
#' @param f,g the two curves, sampled at common points.
#' @param t the sampling grid; defaults to equally spaced on \[0, 1\].
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
  tt <- if (is.null(t)) (seq_len(n) - 1) / (n - 1) else .s03vec(t)
  if (length(tt) != n) stop("functional_distance: t must match the curve length")
  d <- ff - gg
  l2sq <- .fnDist_trapz(tt, d * d)
  list(estimate = if (l2sq > 0) sqrt(l2sq) else 0,
       l2sq = l2sq, l1 = .fnDist_trapz(tt, abs(d)), sup = max(abs(d)), n = n,
       method = "Ramsay-Silverman (2005) L2 metric, composite trapezoid over the whole interval")
}

#' .fnDist_trapz
#'
#' A step of the fnDist implementation. Called by \code{FnDist}.
#' See the file header for the source the module follows.
#' it follows.
#'
#' @param t A vector; its length is taken and its elements indexed.
#' @param v A vector; indexed elementwise.
#' @return The value of \code{s}, as built in the body.
#' @export
.fnDist_trapz <- function(t, v) {
  s <- 0
  n <- length(t)
  if (n > 1L) for (i in seq_len(n - 1L)) s <- s + 0.5 * (v[i] + v[i + 1L]) * (t[i + 1L] - t[i])
  s
}

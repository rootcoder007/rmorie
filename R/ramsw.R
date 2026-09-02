# SPDX-License-Identifier: AGPL-3.0-or-later
#' Ramsay exponential weight function and the E-type M-estimate
#'
#' Ramsay, J. O. (1977), "A comparative study of several robust estimates of
#' slope, intercept, and scale in linear regression", Journal of the American
#' Statistical Association 72(359), 608-615.  The weight function proposed
#' there is the exponential w(r) = exp(-a |r|).  Unlike Huber's or Tukey's it
#' is strictly positive everywhere: no observation is ever given zero weight,
#' it is only downweighted, and the downweighting is governed by the single
#' tuning constant a.
#'
#' The location estimate is the fixed point of the weighted mean under these
#' weights: residuals are scaled by the median absolute deviation, weights
#' recomputed, and the weighted mean iterated to convergence.  This is the
#' ordinary IRLS loop with Ramsay's w in place of Huber's, run to a fixed
#' tolerance from a median start so both language arms take the same number of
#' steps and land on the same number.
#'
#' a = 0 turns every weight into exp(0) = 1, so the estimate is then exactly
#' the arithmetic mean.  That degenerate case is this module's anchor: a
#' closed form that does not depend on the iteration at all.
#'
#' @param y the sample.
#' @param a the tuning constant; a = 0 gives unit weights.
#' @param max_iter cap on the reweighting iterations.
#' @param tol convergence tolerance on the location.
#' @return list: estimate, weights, scale, a, iters, n, method.
#' @keywords internal
#' @examples
#' Ramsw(c(1, 2, 3, 4, 100), 0)$estimate
#' @export
Ramsw <- function(y, a, max_iter = 100L, tol = 1e-13) {
  v <- .s03vec(y)
  n <- length(v)
  if (n == 0L) stop("ramsay_weight: y is empty")
  aa <- as.numeric(a)
  if (is.na(aa) || aa < 0) stop("ramsay_weight: the tuning constant must be non-negative")
  s <- .s03mad(v)
  if (s <= 0) s <- 1
  mu <- .s03median(v)
  it <- 0L
  w <- rep(1, n)
  for (it in seq_len(as.integer(max_iter))) {
    w <- exp(-aa * abs((v - mu) / s))
    sw <- 0
    sx <- 0
    for (i in seq_len(n)) {
      sw <- sw + w[i]
      sx <- sx + w[i] * v[i]
    }
    new <- if (sw > 0) sx / sw else mu
    if (abs(new - mu) <= tol) { mu <- new
    break }
    mu <- new
  }
  list(estimate = mu, weights = w, scale = s, a = aa, iters = it, n = n,
       method = "Ramsay (1977) w(r) = exp(-a |r|), IRLS location from a median start with MAD scale")
}

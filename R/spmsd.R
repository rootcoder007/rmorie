# SPDX-License-Identifier: AGPL-3.0-or-later
#' Mean-square differentiability, from the even derivatives of C at 0
#'
#' Stein (1999, Ch 2.6), quoted by the book: Z(s) is m-times mean-square
#' differentiable IF AND ONLY IF d^(2m) C(h) / dh^(2m) at h = 0 exists
#' and is finite. The covariance function of the m-th derivative field is
#' then (-1)^m d^(2m) C / dh^(2m).
#'
#' This separates the standard models. The gaussian covariance (eq 2.6)
#' is infinitely differentiable -- the book notes Stein regards that
#' smoothness as unrealistic for physical processes. The exponential
#' model has a kink at the origin, so no second derivative exists and it
#' is not mean-square differentiable at all.
#'
#' The criterion is EXISTENCE, so the test is whether the finite
#' difference CONVERGES as the stencil shrinks, not whether it happens to
#' fall below a ceiling. For a kinked C the estimate diverges like a
#' power of 1/h, so halving h inflates it; for a smooth C it settles. A
#' magnitude ceiling gets this wrong: the exponential model at h = 1e-3
#' gives about -6e3, comfortably under any generous bound, yet has no
#' second derivative at all.
#'
#' @param cov_func Function C(h) taking a numeric vector, returning one.
#' @param m Order of differentiability to test; must be >= 1.
#' @param h Stencil spacing. Defaults to eps^(1/(2m+2)), balancing
#'   truncation against cancellation: a 2m-th central difference divides
#'   by h^(2m), so a step chosen for m = 1 is badly conditioned for
#'   m = 2 and meaningless by m = 3.
#' @param tol Magnitude above which the derivative is judged not finite.
#' @return Named list: is_differentiable, order, derivative_2m,
#'   derivative_coarse, growth_ratio, converged, derivative_cov, h,
#'   significant_digits.
#' @references Schabenberger & Gotway (2005), Sec 2.3, pp. 50-51, citing
#'   Stein (1999), Ch 2.6.
#' @examples
#' spmsd(function(h) exp(-3 * h^2))$is_differentiable   # TRUE
#' spmsd(function(h) exp(-3 * h))$is_differentiable     # FALSE
#' @export
spmsd <- function(cov_func, m = 1, h = NULL, tol = 1e6) {
  if (!is.function(cov_func)) stop("`cov_func` must be a function C(h)")
  m <- as.integer(m)
  if (m < 1) stop("`m` must be >= 1")
  eps <- .Machine$double.eps
  if (is.null(h)) h <- eps^(1 / (2 * m + 2))
  if (h <= 0) stop("`h` must be > 0")
  k <- 2 * m
  offs <- seq_len(k + 1) - 1 - k %/% 2
  coeffs <- (-1)^(0:k) * choose(k, 0:k)
  deriv_at <- function(step) {
    vals <- as.numeric(cov_func(abs(rev(offs) * step)))
    sum(coeffs * vals) / step^k
  }
  d1 <- deriv_at(h)
  d2 <- deriv_at(h / 2)
  growth <- abs(d2) / max(abs(d1), 1e-300)
  rel_noise <- eps / h^k
  digits <- max(0, -log10(max(rel_noise, 1e-300)))
  converged <- is.finite(d1) && is.finite(d2) && growth < 1.5
  finite <- isTRUE(converged && abs(d2) <= tol)
  list(is_differentiable = finite, order = m, derivative_2m = d2,
       derivative_coarse = d1, growth_ratio = growth, converged = converged,
       derivative_cov = (-1)^m * d2, h = h, significant_digits = digits)
}

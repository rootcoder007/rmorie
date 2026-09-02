# SPDX-License-Identifier: AGPL-3.0-or-later

#' Bias and variance of the boundary-free KDE (Theorem 5.5)
#'
#' Theorem 5.5, Eqs. (5.10)-(5.11):
#' \deqn{\mathrm{Bias}\[\tilde f_X(x)\] = \frac{h^2 c_2(x)}{2g'(g^{-1}(x))}\mu_2(K) + o(h^2),\quad \mathrm{Var}\[\tilde f_X(x)\] = \frac{f_X(x)}{nhg'(g^{-1}(x))}\int K^2(v)dv + o(1/(nh)).}{Bias\[ftilde(x)\] = h^2 c2(x) mu2(K) / (2 g'(g^-1(x))) + o(h^2), Var\[ftilde(x)\] = f(x) int K^2 / (n h g'(g^-1(x))) + o(1/(nh)).}
#'
#' Both carry `1/g'`, and the SAME power in both -- which is why the
#' transformation does not change the shape of the bias-variance tradeoff, only
#' its constants. The MSE-optimal bandwidth is still `O(n^(-1/5))` and the
#' optimal MSE still `O(n^(-4/5))`.
#'
#' The contrast with Theorem 5.2 is the point. There the variance was
#' `O(1/n) - O(h/n)` and smoothing HELPED; here it is `O(1/(nh))` and smoothing
#' hurts, so bandwidth must be traded against bias in the usual way. A
#' distribution function is estimable at the parametric rate; a density is not.
#'
#' `rk` defaults to the Gaussian `1/(2 sqrt(pi))`.
#'
#' @param n Sample size.
#' @param h Bandwidth.
#' @param density `f_X(x)`.
#' @param c2 The Theorem 5.5 coefficient.
#' @param dg `g'(g^-1(x))`, strictly positive.
#' @param mu2 `int v^2 K(v) dv`.
#' @param rk `int K^2(v) dv`; defaults to the Gaussian `1/(2 sqrt(pi))`.
#' @return Named list with ``bias``, ``variance``, ``se``, ``mse``, ``hopt``, ``h``, ``n``, ``method``.
#' @references Fauzi and Maesono (2023), Theorem 5.5, Eqs. (5.10)-(5.11).
#' @examples
#' Bfkdebv(n = 100, h = 0.3, density = 0.4, c2 = -0.1, dg = 1)
#' @export
Bfkdebv <- function(n, h, density, c2, dg, mu2 = 1, rk = NULL) {
  if (n < 1) stop("sample size must be at least 1.")
  if (h <= 0) stop("bandwidth must be positive.")
  if (dg <= 0) stop("g' must be positive; g is an increasing bijection (D4).")
  if (is.null(rk)) rk <- 1 / (2 * sqrt(pi))
  bias <- h * h * c2 / (2 * dg) * mu2
  v <- density * rk / (n * h * dg)
  lead <- c2 * mu2 / (2 * dg)
  hopt <- if (lead != 0 && density > 0) {
    (density * rk / (dg * 4 * lead^2 * n))^0.2
  } else NA_real_
  list(bias = bias, variance = v, se = sqrt(v), mse = bias * bias + v,
       hopt = hopt, h = h, n = n,
       method = "boundary-free KDE bias and variance (Theorem 5.5)")
}

# CANONICAL TEST
# r <- Bfkdebv(n = 100, h = 0.3, density = 0.4, c2 = -0.1, dg = 1)
# stopifnot(r$variance > 0, r$hopt > 0)

#' @rdname Bfkdebv
#' @keywords internal
#' @export
morie_fauzi_thm5_5_bdfree_kde_bv <- Bfkdebv

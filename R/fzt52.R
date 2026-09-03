# SPDX-License-Identifier: AGPL-3.0-or-later

#' Bias and variance of the boundary-free KDFE (Theorem 5.2)
#'
#' Theorem 5.2, Eqs. (5.6)-(5.7):
#' \deqn{\mathrm{Bias}\[\tilde F_X(x)\] = \frac{h^2}{2}c_1(x)\mu_2(K) + o(h^2),\quad
#' \mathrm{Var}\[\tilde F_X(x)\] = \frac{F(1-F)}{n} - \frac{2h}{n}g'(g^{-1}(x))f_X(x)r_1
#' + o(h/n),}{Bias\[Ftilde(x)\] = (h^2/2) c1(x) mu2(K) + o(h^2), Var\[Ftilde(x)\] =
#' F(1-F)/n - (2h/n) g'(g^-1(x)) f(x) r1 + o(h/n),}
#' with `c1` from (5.8) and `r1` from (2.9).
#'
#' Remark 5.1 draws the consequence: since `r1 > 0` and `g` is increasing, the
#' variance is SMALLER than the naive estimator's whenever `g'(g^-1(x)) >= 1`.
#' The bias comparison is not settled in general, but for `Omega = R+` with
#' `g = exp` the bias converges faster in the boundary region as `x -> 0` --
#' the case the construction exists for.
#'
#' `vargain` is returned explicitly: `2 h g' f r1 / n`, the amount by which
#' smoothing beats the empirical df here, positive exactly when the
#' transformation stretches.
#'
#' @param n Sample size.
#' @param h Bandwidth.
#' @param fx `F_X(x)`.
#' @param density `f_X(x)`.
#' @param c1 The coefficient (5.8).
#' @param dg `g'(g^-1(x))`.
#' @param mu2 `int v^2 K(v) dv`.
#' @param r1 Kernel constant (2.9); defaults to the Gaussian value.
#' @return Named list with ``bias``, ``variance``, ``se``, ``edfvar``, ``vargain``,
#' ``h``, ``n``, ``method``.
#' @references Fauzi and Maesono (2023), Theorem 5.2, Eqs. (5.6)-(5.8).
#' @examples
#' Bfkdfbv(n = 100, h = 0.2, fx = 0.5, density = 0.4, c1 = 0.3, dg = 1)
#' @export
Bfkdfbv <- function(n, h, fx, density, c1, dg, mu2 = 1, r1 = NULL) {
  if (n < 1) stop("sample size must be at least 1.")
  if (h <= 0) stop("bandwidth must be positive.")
  if (is.null(r1)) r1 <- Kdfr1()$estimate
  bias <- (h * h / 2) * c1 * mu2
  edfvar <- fx * (1 - fx) / n
  gain <- 2 * h / n * dg * density * r1
  v <- edfvar - gain
  list(bias = bias, variance = v, se = if (v > 0) sqrt(v) else NA_real_,
       edfvar = edfvar, vargain = gain, h = h, n = n,
       method = "boundary-free KDFE bias and variance (Theorem 5.2)")
}

# CANONICAL TEST
# r <- Bfkdfbv(n = 100, h = 0.2, fx = 0.5, density = 0.4, c1 = 0.3, dg = 1)
# stopifnot(r$variance < r$edfvar)

#' @rdname Bfkdfbv
#' @keywords internal
#' @export
morie_fauzi_thm5_2_bdfree_kdfe_bv <- Bfkdfbv

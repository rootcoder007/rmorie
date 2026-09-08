# SPDX-License-Identifier: AGPL-3.0-or-later

#' Bias and variance of the second cumulative-survival estimator (Theorem 4.2)
#'
#' Theorem 4.2, Eqs. (4.19)-(4.22):
#' \deqn{\mathrm{Bias}\[S_{X,2}(t)\] = \tfrac{h^2}{2}b_3(t)\mu_2(K) + o(h^2),\quad
#' \mathrm{Var}\[S_{X,2}(t)\] = \tfrac{1}{n}\[2\bar S_X(t) - S_X^2(t)\] +
#' o(h/n),}{Bias\[S_X2(t)\] = (h^2/2) b3(t) mu2(K) + o(h^2), Var\[S_X2(t)\] = (1/n)\[2
#' Sbar(t) - S(t)^2\] + o(h/n),}
#' with `b3(t) = \[g'(g^-1(t))\]^2 f(t) - g''(g^-1(t)) S(t)` from (4.21).
#'
#' The variance is IDENTICAL to that of `S_X,1` in Theorem 4.1 -- the same
#' expression, not merely the same order. Only the bias coefficients differ,
#' `b2` versus `b3`. Sec. 4.2 draws the practical conclusion: the two are
#' statistically equivalent, and `m_X,1` is preferred only because it preserves
#' the analytic relationship between the survival and cumulative-survival
#' estimates.
#'
#' Compare `b2` (4.15), which carries an integral of `g' g'' f(g(.))` over
#' `[g^-1(t), Inf)`, against `b3`, which is purely local -- why `b3` is cheap
#' and `b2` is not.
#'
#' @param t Evaluation point.
#' @param n Sample size.
#' @param h Bandwidth.
#' @param surv `S_X(t)`.
#' @param cumsurv `Sbar_X(t)`.
#' @param dg,d2g `g'(g^-1(t))` and `g''(g^-1(t))`.
#' @param density `f_X(t)`.
#' @param mu2 `int y^2 K(y) dy`.
#' @return Named list with ``bias``, ``variance``, ``b3``, ``cov``, ``h``, ``n``, ``method``.
#' @references Fauzi and Maesono (2023), Theorem 4.2, Eqs. (4.19)-(4.22).
#' @examples
#' Srvbv2(t = 1, n = 100, h = 0.1, surv = 0.4, cumsurv = 0.5, dg = 1, d2g = 0, density = 0.35)
#' @export
Srvbv2 <- function(t, n, h, surv, cumsurv, dg, d2g, density, mu2 = 1) {
  if (n < 1) stop("sample size must be at least 1.")
  if (h <= 0) stop("bandwidth must be positive.")
  b3 <- dg^2 * density - d2g * surv
  list(bias = (h * h / 2) * b3 * mu2,
       variance = (2 * cumsurv - surv^2) / n, b3 = b3,
       cov = surv * (1 - surv) / n, h = h, n = n,
       method = "second cumulative-survival estimator moments (Theorem 4.2)")
}

# CANONICAL TEST
# r <- Srvbv2(t = 1, n = 100, h = 0.1, surv = 0.4, cumsurv = 0.5, dg = 1, d2g = 0, density = 0.35)
# stopifnot(abs(r$b3 - 0.35) < 1e-15)

#' @rdname Srvbv2
#' @keywords internal
#' @export
morie_fauzi_thm4_2_surv2_bias_var <- Srvbv2

# SPDX-License-Identifier: AGPL-3.0-or-later

#' Bias of the bias-reduced KDFE (Theorem 2.2)
#'
#' Theorem 2.2, Eq. (2.6):
#' \deqn{\mathrm{Bias}\[\tilde F_X(x)\] = h^4 a^2 \frac{b_2^2(x) - 2 b_4(x) F_X(x)}{2
#' F_X(x)} + o(h^4) + O(n^{-1}),}{Bias\[Ftilde(x)\] = h^4 a^2 (b2(x)^2 - 2 b4(x) F(x)) /
#' (2 F(x)) + o(h^4) + O(1/n),}
#' with `b2` from (2.7) and `b4` from (2.8).
#'
#' Two things worth naming. The `O(1/n)` term is not a smoothing error but the
#' price of the estimator being a NONLINEAR function of two linear statistics,
#' from the `O(p^2)` remainder in `(1+p)^q = 1 + pq + O(p^2)`. There is a floor
#' on the bias that no bandwidth can cross.
#'
#' And the `a^2` factor: the bias GROWS with the second smoothing parameter,
#' while Remark 2.2 says `Ftilde -> Fhat_h` as `a -> Inf`. Both are true -- the
#' `h^4 a^2` form is asymptotic in `h` for FIXED `a`, and Table 2.1 duly shows
#' the smallest AISE at `a = 0.01`.
#'
#' @param h Bandwidth.
#' @param a Second smoothing parameter; `a > 0`, `a != 1`.
#' @param b2,b4 The coefficients (2.7) and (2.8) at `x`.
#' @param fx `F_X(x)`, strictly between 0 and 1.
#' @return Named list with ``bias``, ``leading``, ``h``, ``a``, ``method``.
#' @references Fauzi and Maesono (2023), Theorem 2.2, Eqs. (2.6)-(2.8).
#' @examples
#' Gekdfbias(h = 0.1, a = 2, b2 = 0, b4 = 0.5, fx = 0.5)
#' @export
Gekdfbias <- function(h, a, b2, b4, fx) {
  if (a <= 0 || a == 1) stop("a must be positive and different from 1.")
  if (!(fx > 0 && fx < 1)) stop("F(x) must lie strictly in (0, 1).")
  lead <- (b2^2 - 2 * b4 * fx) / (2 * fx)
  list(bias = h^4 * a * a * lead, leading = lead, h = h, a = a,
       method = "bias of the bias-reduced KDFE (Theorem 2.2)")
}

# CANONICAL TEST
# r <- Gekdfbias(h = 0.1, a = 2, b2 = 0, b4 = 0.5, fx = 0.5)
# stopifnot(abs(r$bias + 2e-04) < 1e-18)

#' @rdname Gekdfbias
#' @keywords internal
#' @export
morie_fauzi_thm2_2_bias_brdkdfe <- Gekdfbias

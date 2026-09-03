# SPDX-License-Identifier: AGPL-3.0-or-later

#' The b_5 coefficient of the mean-residual-life variance (Eq. 4.28)
#'
#' Eq. (4.28), second half:
#' \deqn{b_5(t) = g'(g^{-1}(t))f_X(t)m_X^2(t).}{b5(t) = g'(g^-1(t)) f(t) m(t)^2.}
#'
#' It multiplies the `-h/n` term of (4.27), so it is the SIZE OF THE GAIN from
#' smoothing:
#' \deqn{\mathrm{Var}\[m_{X,i}(t)\] = \frac{1}{n}\frac{b_4(t)}{S_X^2(t)} -
#' \frac{h}{n}\frac{b_5(t)}{S_X^2(t)}\int V(y)W(y)dy + o(h/n).}{Var\[m(t)\] = (1/n)
#' b4/S^2 - (h/n) b5/S^2 int V W dy + o(h/n).}
#'
#' The sign is the same lesson as `r1` in Chapter 2 -- smoothing a
#' distribution-type functional REDUCES variance -- and the same reason the
#' bandwidth rate here is `n^(-1/3)`, not `n^(-1/5)`.
#'
#' Note where `g'` enters: the gain is proportional to the derivative of the
#' transformation at the point. A transformation that stretches near `t` buys
#' more variance reduction there, the mechanism behind Remark 5.1's observation
#' that `g'(g^-1(x)) >= 1` makes the boundary-free estimator strictly better
#' than the naive one.
#'
#' @param dg `g'(g^-1(t))`.
#' @param density `f_X(t)`.
#' @param mrl `m_X(t)`.
#' @param surv `S_X(t)`; needed only for `varterm`.
#' @return Named list with ``estimate``, ``varterm``, ``method``.
#' @references Fauzi and Maesono (2023), Eqs. (4.27)-(4.28).
#' @examples
#' Mrlb5(dg = 1, density = 0.4, mrl = 2)
#' @export
Mrlb5 <- function(dg, density, mrl, surv = NULL) {
  val <- dg * density * mrl^2
  varterm <- if (is.null(surv)) NA_real_ else {
    if (surv <= 0) stop("S_X(t) must be positive.")
    val / (surv * surv)
  }
  list(estimate = val, varterm = varterm,
       method = "b_5 coefficient of the MRL variance (Eq. 4.28)")
}

# CANONICAL TEST
# stopifnot(abs(Mrlb5(dg = 1, density = 0.4, mrl = 2)$estimate - 1.6) < 1e-15)

#' @rdname Mrlb5
#' @keywords internal
#' @export
morie_fauzi_b5_coefficient_mrl <- Mrlb5

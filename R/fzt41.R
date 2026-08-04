# SPDX-License-Identifier: AGPL-3.0-or-later

#' Bias and variance of the boundary-free survival and cumulative-survival estimators (Theorem 4.1)
#'
#' Theorem 4.1, Eqs. (4.10)-(4.16):
#' \deqn{\mathrm{Bias}[\tilde S_X(t)] = -\tfrac{h^2}{2}b_1(t)\mu_2(K) + o(h^2),}{Bias[Stilde(t)] = -(h^2/2) b1(t) mu2(K) + o(h^2),}
#' \deqn{\mathrm{Var}[\tilde S_X(t)] = \tfrac{1}{n}\tilde S_X(t)F_X(t) - \tfrac{h}{n}g'(g^{-1}(t))f_X(t)\int V(y)W(y)dy + o(h/n),}{Var[Stilde(t)] = (1/n) Stilde(t) F(t) - (h/n) g'(g^-1(t)) f(t) int V(y)W(y)dy + o(h/n),}
#' \deqn{\mathrm{Bias}[S_{X,1}(t)] = \tfrac{h^2}{2}b_2(t)\mu_2(K) + o(h^2),\quad \mathrm{Var}[S_{X,1}(t)] = \tfrac{1}{n}[2\bar S_X(t) - S_X^2(t)] + o(h/n),}{Bias[S_X1(t)] = (h^2/2) b2(t) mu2(K), Var[S_X1(t)] = (1/n)[2 Sbar(t) - S(t)^2],}
#' with `b1(t) = g''(g^-1(t)) f(t) + [g'(g^-1(t))]^2 f'(t)` from (4.14) and
#' `b2` from (4.15).
#'
#' Note the SIGNS: the survival estimator's bias carries a minus and the
#' cumulative one a plus, because `S = 1 - F` flips the leading term while the
#' cumulative survival integrates it back. They are not interchangeable, so
#' both come back rather than one behind a flag.
#'
#' For the Gaussian kernel `int V(y)W(y)dy = int (1-W)W dy = 1/sqrt(pi)`, used
#' in closed form.
#'
#' @param t Evaluation point.
#' @param n Sample size.
#' @param h Bandwidth.
#' @param surv `S_X(t)`.
#' @param cdf `F_X(t)`.
#' @param cumsurv `Sbar_X(t)`, the cumulative survival.
#' @param b1,b2 The coefficients (4.14) and (4.15).
#' @param dg `g'(g^-1(t))`.
#' @param density `f_X(t)`.
#' @param mu2 `int y^2 K(y) dy`.
#' @param vw `int V(y)W(y)dy`; defaults to the Gaussian `1/sqrt(pi)`.
#' @return Named list with ``biassurv``, ``varsurv``, ``biascum``, ``varcum``, ``vw``, ``h``, ``n``, ``method``.
#' @references Fauzi and Maesono (2023), Theorem 4.1, Eqs. (4.10)-(4.15).
#' @examples
#' Srvbv1(t = 1, n = 100, h = 0.1, surv = 0.4, cdf = 0.6, cumsurv = 0.5,
#'        b1 = 0.2, b2 = 0.3, dg = 1, density = 0.35)
#' @export
Srvbv1 <- function(t, n, h, surv, cdf, cumsurv, b1, b2, dg, density, mu2 = 1, vw = NULL) {
  if (n < 1) stop("sample size must be at least 1.")
  if (h <= 0) stop("bandwidth must be positive.")
  if (is.null(vw)) vw <- 1 / sqrt(pi)
  list(
    biassurv = -(h * h / 2) * b1 * mu2,
    varsurv = surv * cdf / n - h / n * dg * density * vw,
    biascum = (h * h / 2) * b2 * mu2,
    varcum = (2 * cumsurv - surv^2) / n,
    vw = vw, h = h, n = n,
    method = "boundary-free survival and cumulative-survival moments (Theorem 4.1)"
  )
}

# CANONICAL TEST
# r <- Srvbv1(t = 1, n = 100, h = 0.1, surv = 0.4, cdf = 0.6, cumsurv = 0.5,
#             b1 = 0.2, b2 = 0.3, dg = 1, density = 0.35)
# stopifnot(r$biassurv < 0, r$biascum > 0)

#' @rdname Srvbv1
#' @keywords internal
#' @export
morie_fauzi_thm4_1_surv_bias_var <- Srvbv1

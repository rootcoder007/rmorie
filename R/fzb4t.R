# SPDX-License-Identifier: AGPL-3.0-or-later

#' The b_4 coefficient of the mean-residual-life variance (Eq. 4.28)
#'
#' Eq. (4.28), first half:
#' \deqn{b_4(t) = 2\bar S_X(t) - S_X(t)m_X^2(t),}{b4(t) = 2 Sbar(t) - S(t) m(t)^2,}
#' with `Sbar` the cumulative survival `int_t^Inf S(u) du` and
#' `m(t) = Sbar(t)/S(t)` the mean residual life.
#'
#' It is the numerator of the leading `1/n` term of `Var\[m_X,i(t)\]` in (4.27),
#' divided by `S(t)^2`.
#'
#' Substituting `m = Sbar/S` gives `b4 = 2 Sbar - Sbar^2/S`, so the whole
#' variance term is `Sbar (2S - Sbar) / (n S^3)`. That form makes the failure
#' mode visible: as `t` moves into the tail, `S -> 0` CUBED in the denominator,
#' and the mean residual life becomes unestimable long before the survival
#' function does.
#'
#' @param surv `S_X(t)`, strictly positive.
#' @param cumsurv `Sbar_X(t)`.
#' @param mrl `m_X(t)`; defaults to `cumsurv / surv`.
#' @return Named list with ``estimate``, ``mrl``, ``varterm``, ``method``.
#' @references Fauzi and Maesono (2023), Eqs. (4.27)-(4.28).
#' @examples
#' Mrlb4(surv = 0.5, cumsurv = 1)
#' @export
Mrlb4 <- function(surv, cumsurv, mrl = NULL) {
  if (surv <= 0) stop("S_X(t) must be positive.")
  m <- if (is.null(mrl)) cumsurv / surv else mrl
  val <- 2 * cumsurv - surv * m * m
  list(estimate = val, mrl = m, varterm = val / (surv * surv),
       method = "b_4 coefficient of the MRL variance (Eq. 4.28)")
}

# CANONICAL TEST
# r <- Mrlb4(surv = 0.5, cumsurv = 1)
# stopifnot(abs(r$mrl - 2) < 1e-15, abs(r$estimate) < 1e-15)

#' @rdname Mrlb4
#' @keywords internal
#' @export
morie_fauzi_b4_coefficient_mrl <- Mrlb4

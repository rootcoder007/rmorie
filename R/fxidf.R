# SPDX-License-Identifier: AGPL-3.0-or-later
#' Test whether the effect of X differs across levels of a modifier
#'
#' Effect modification and interaction are different claims. Interaction
#' asks what happens when you set both variables; effect modification
#' only asks whether the effect of one varies across strata defined by
#' the other, and the modifier need not be causal -- it can be a proxy or
#' a stratifying label. Only the exposure need be unconfounded.
#'
#' Formula: fit \code{Y = g0 + g1 X + g2 V + g3 X V}; g3 is the additive
#' modification and \code{g1 + g3 v} the stratum-specific effect.
#'
#' @param Y Outcome.
#' @param X Exposure.
#' @param C_mod Candidate effect modifier.
#' @return List with \code{estimate}, \code{se}, \code{t}, \code{coef},
#'   \code{effect_at_0}, \code{effect_at_1}, \code{n}.
#' @references VanderWeele, T. J. (2009). Epidemiology 20:863-871.
#' @export
Fxidf <- function(Y, X, C_mod) {
  y <- as.numeric(Y); x <- as.numeric(X); v <- as.numeric(C_mod)
  n <- length(y)
  des <- cbind(1, x, v, x * v)
  fit <- .t1_lstsq(des, y)
  dof <- n - 4
  s2 <- if (dof > 0) sum(fit$resid^2) / dof else NaN
  se <- if (dof > 0 && fit$xtxinv[4, 4] > 0) sqrt(s2 * fit$xtxinv[4, 4]) else NaN
  .t1_result(estimate = fit$beta[4], se = se,
             t = if (!is.na(se) && se > 0) fit$beta[4] / se else NaN,
             coef = fit$beta, effect_at_0 = fit$beta[2],
             effect_at_1 = fit$beta[2] + fit$beta[4], n = n,
             method = "Additive effect modification, X by V interaction")
}

# SPDX-License-Identifier: AGPL-3.0-or-later
#' Outcome-only (g-computation) estimation, and its TMLE correction
#'
#' Robins (1986), A new approach to causal inference in mortality studies
#' with a sustained exposure period, Mathematical Modelling 7(9-12),
#' 1393-1512, introduces the g-formula; van der Laan and Rubin (2006), The
#' International Journal of Biostatistics 2(1), art. 11, supplies the
#' targeting step.  The outcome-only estimator is the plug-in psi =
#' mean_i [Qbar(1, X_i) - Qbar(0, X_i)], which uses no propensity score at
#' all, so it is consistent when the outcome regression is right whatever
#' the treatment mechanism.  Neither source was retrievable here as a full
#' text; both are quoted in their standard published form.  The targeted
#' estimate is returned alongside: their difference is the entire
#' contribution of the propensity score, and a large gap warns that the
#' outcome model is doing work the data do not support.
#'
#' @param y,D outcome and treatment.
#' @param X covariates.
#' @param alpha interval level.
#' @return list: estimate, psi_gcomp, psi_tmle, gap, se, ci_lo, ci_hi,
#'   rmse_resid, n, method.
#' @keywords internal
#' @examples
#' Tmleoutcomeonlyregr(c(1, 0, 1, 1, 0, 1), c(1, 0, 1, 0, 1, 0))$psi_gcomp
#' @export
Tmleoutcomeonlyregr <- function(y, D, X = NULL, alpha = 0.05) {
  yv <- .s03vec(y); d <- .s03vec(D); n <- length(yv)
  Z <- .s03design(X, n)
  Q <- cbind(1, d, Z[, -1, drop = FALSE])
  b <- .s03lstsq(Q, yv)
  q1 <- numeric(n); q0 <- numeric(n)
  for (i in seq_len(n)) {
    r1 <- c(1, 1, Z[i, -1]); r0 <- c(1, 0, Z[i, -1])
    s1 <- 0; s0 <- 0
    for (j in seq_along(b)) { s1 <- s1 + b[j] * r1[j]; s0 <- s0 + b[j] * r0[j] }
    q1[i] <- s1; q0[i] <- s0
  }
  psi <- 0
  for (i in seq_len(n)) psi <- psi + (q1[i] - q0[i]) / n
  resid <- numeric(n)
  for (i in seq_len(n)) resid[i] <- yv[i] - (if (d[i] > 0.5) q1[i] else q0[i])
  ic <- (q1 - q0) - psi
  v <- 0
  for (x in ic) v <- v + x * x
  se <- if (n) sqrt(v / (n * n)) else NaN
  tm <- .s03tmle(yv, d, X)
  z <- qnorm(1 - as.numeric(alpha) / 2)
  list(estimate = psi, psi_gcomp = psi, psi_tmle = tm$psi,
       gap = tm$psi - psi, se = se, ci_lo = psi - z * se, ci_hi = psi + z * se,
       rmse_resid = if (n) sqrt(sum(resid^2) / n) else NaN, n = n,
       method = "G-computation plug-in (Robins 1986) with the TMLE-targeted value for comparison")
}

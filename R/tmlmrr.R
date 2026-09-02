# SPDX-License-Identifier: AGPL-3.0-or-later
#' TMLE for the marginal risk ratio
#'
#' van der Laan and Rubin (2006), Targeted maximum likelihood learning,
#' The International Journal of Biostatistics 2(1), art. 11: the initial
#' outcome fit is fluctuated along a submodel whose score spans the
#' efficient influence curve; for the ATE the clever covariate is H = D/g
#' - (1-D)/(1-g) and the fluctuation is fitted on the logistic scale,
#' which keeps the targeted predictions in \[0, 1\].  The article is open
#' access but was not retrievable here; the clever covariate and logistic
#' fluctuation are quoted in their standard published form.  RR =
#' E\[Y(1)\]/E\[Y(0)\] is a smooth function of the two targeted means, so by
#' the delta method IC_logRR = IC_1/E\[Y(1)\] - IC_0/E\[Y(0)\]; the standard
#' error is on the log scale and the interval exponentiated back.
#'
#' @param y,D outcome and treatment.
#' @param X covariates.
#' @param alpha interval level.
#' @param trim propensity truncation.
#' @return list: estimate, rr, log_rr, se_log, ci_lo, ci_hi, ey1, ey0,
#'   eps, n, method.
#' @keywords internal
#' @examples
#' Tmlerr(c(1, 0, 1, 1, 0, 1), c(1, 0, 1, 0, 1, 0))$rr
#' @export
Tmlerr <- function(y, D, X = NULL, alpha = 0.05, trim = 0) {
  fit <- .s03tmle(y, D, X, trim)
  yv <- .s03vec(y)
  d <- .s03vec(D)
  n <- length(yv)
  g <- fit$g
  q1 <- fit$q1
  q0 <- fit$q0
  lo <- fit$shift
  rng <- fit$scale
  m1 <- 0
  m0 <- 0
  for (i in seq_len(n)) { m1 <- m1 + (lo + rng * q1[i]) / n
  m0 <- m0 + (lo + rng * q0[i]) / n }
  ic <- numeric(n)
  for (i in seq_len(n)) {
    qa1 <- lo + rng * q1[i]
    qa0 <- lo + rng * q0[i]
    i1 <- (d[i] / g[i]) * (yv[i] - qa1) + qa1 - m1
    i0 <- ((1 - d[i]) / (1 - g[i])) * (yv[i] - qa0) + qa0 - m0
    ic[i] <- if (m1 != 0 && m0 != 0) i1 / m1 - i0 / m0 else NaN
  }
  v <- 0
  for (x in ic) v <- v + x * x
  se <- if (n) sqrt(v / (n * n)) else NaN
  rr <- if (m0 != 0) m1 / m0 else NaN
  lrr <- if (!is.na(rr) && rr > 0) log(rr) else NaN
  z <- qnorm(1 - as.numeric(alpha) / 2)
  list(estimate = rr, rr = rr, log_rr = lrr, se_log = se,
       ci_lo = if (!is.nan(lrr)) exp(lrr - z * se) else NaN,
       ci_hi = if (!is.nan(lrr)) exp(lrr + z * se) else NaN,
       ey1 = m1, ey0 = m0, eps = fit$eps, n = n,
       method = "TMLE for the marginal risk ratio, delta-method influence curve on the log scale")
}

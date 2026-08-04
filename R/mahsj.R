# SPDX-License-Identifier: AGPL-3.0-or-later
#' HKSJ confidence interval with a t prediction interval
#'
#' CI = mu +/- t_{k-1} se_HKSJ and PI = mu +/- t_{k-1} sqrt(tau^2 + se^2).
#' Source consulted: IntHout, Ioannidis and Borm (2014), BMC Medical Research
#' Methodology 14:25.  Verified against metafor::rma(test = "knha") and its
#' predict() prediction interval.
#'
#' @param yi study effect sizes.
#' @param vi within-study sampling variances.
#' @param level level for the confidence and prediction intervals.
#' @return list: estimate, se, ci_lower, ci_upper, pi_lower, pi_upper,
#'   pi_spread, t, df, p_value, tau2, n, method.
#' @keywords internal
#' @examples
#' mahsj(c(0.1, 0.3, -0.2, 0.45), c(0.02, 0.05, 0.03, 0.08))$pi_lower
#' @export
mahsj <- function(yi, vi, level = 0.95) {
  y <- as.numeric(yi); v <- as.numeric(vi)
  k <- length(y)
  d <- k02dl(y, v)
  w <- 1 / (v + d$tau2)
  sw <- sum(w)
  se <- sqrt(sum(w * (y - d$mu)^2) / ((k - 1) * sw))
  tstat <- d$mu / se
  crit <- k02tq(0.5 + 0.5 * level, k - 1)
  spread <- sqrt(d$tau2 + se * se)
  list(estimate = d$mu, se = se, ci_lower = d$mu - crit * se,
       ci_upper = d$mu + crit * se, pi_lower = d$mu - crit * spread,
       pi_upper = d$mu + crit * spread, pi_spread = spread, t = tstat,
       df = as.integer(k - 1), p_value = k02p2t(tstat, k - 1),
       tau2 = d$tau2, n = k,
       method = "HKSJ interval with t prediction interval (IntHout, Ioannidis & Borm 2014)")
}

# CANONICAL TEST
# r <- mahsj(c(0.10,0.30,-0.20,0.45,0.05,0.22), c(0.02,0.05,0.03,0.08,0.01,0.04))
# stopifnot(abs(r$pi_upper - 0.355430976045376) < 1e-12)

#' @rdname mahsj
#' @keywords internal
#' @export
morie_mahsj <- mahsj

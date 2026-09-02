# SPDX-License-Identifier: AGPL-3.0-or-later
#' Hartung-Knapp adjusted random-effects meta-analysis
#'
#' Replaces the random-effects standard error by
#' sqrt( sum w (y - mu)^2 / ((k-1) sum w) ), w = 1/(v + tau^2), and uses t on
#' k - 1 degrees of freedom.  Source consulted: Hartung and Knapp (2001),
#' Statistics in Medicine 20, 1771-1782.  Verified against
#' metafor::rma(test = "knha").
#'
#' @param yi study effect sizes.
#' @param vi within-study sampling variances.
#' @param level confidence level.
#' @return list: estimate, se, se_dl, ci_lower, ci_upper, t, df, p_value,
#'   tau2, inflation, n, method.
#' @keywords internal
#' @examples
#' mahks(c(0.1, 0.3, -0.2, 0.45), c(0.02, 0.05, 0.03, 0.08))$se
#' @export
mahks <- function(yi, vi, level = 0.95) {
  y <- as.numeric(yi)
  v <- as.numeric(vi)
  k <- length(y)
  d <- k02dl(y, v)
  w <- 1 / (v + d$tau2)
  sw <- sum(w)
  se_dl <- sqrt(d$var)
  se <- sqrt(sum(w * (y - d$mu)^2) / ((k - 1) * sw))
  tstat <- d$mu / se
  crit <- k02tq(0.5 + 0.5 * level, k - 1)
  list(estimate = d$mu, se = se, se_dl = se_dl, ci_lower = d$mu - crit * se,
       ci_upper = d$mu + crit * se, t = tstat, df = as.integer(k - 1),
       p_value = k02p2t(tstat, k - 1), tau2 = d$tau2, inflation = se / se_dl,
       n = k,
       method = "Hartung-Knapp adjusted random-effects meta-analysis (Hartung & Knapp 2001)")
}

# CANONICAL TEST
# r <- mahks(c(0.10,0.30,-0.20,0.45,0.05,0.22), c(0.02,0.05,0.03,0.08,0.01,0.04))
# stopifnot(abs(r$se - 0.0745588782641038) < 1e-13)

#' @rdname mahks
#' @keywords internal
#' @export
morie_mahks <- mahks

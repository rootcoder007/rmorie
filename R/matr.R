# SPDX-License-Identifier: AGPL-3.0-or-later
#' Two-step DerSimonian-Laird tau^2 from the Hedges start value
#'
#' The generalised moment estimator at working weights a_i = 1/(v_i + tau0) is
#' (sum a (y - ybar_a)^2 - sum a v + sum a^2 v / sum a) / (sum a - sum a^2 / sum a);
#' with tau0 = 0 it collapses exactly to DerSimonian-Laird.  The two-step
#' estimator starts from the Hedges variance-components value
#' sum (y - ybar)^2/(k-1) - mean(v).  Source consulted: DerSimonian and Kacker
#' (2007), Contemporary Clinical Trials 28, 105-114, equations (5)-(6).
#' Verified against metafor::rma(method = "HE") and (method = "DL").
#'
#' @param yi study effect sizes.
#' @param vi within-study sampling variances.
#' @param level confidence level.
#' @return list: estimate, tau2_he, tau2_dl, mu, se, ci_lower, ci_upper, z,
#'   p_value, n, method.
#' @keywords internal
#' @examples
#' matr(c(0.1, 0.3, -0.2, 0.45), c(0.02, 0.05, 0.03, 0.08))$estimate
#' @export
matr <- function(yi, vi, level = 0.95) {
  y <- as.numeric(yi); v <- as.numeric(vi)
  k <- length(y)
  tau_he <- max(0, sum((y - mean(y))^2) / (k - 1) - mean(v))
  tau2 <- k02mm(y, v, tau_he)
  d <- k02dl(y, v)
  ws <- 1 / (v + tau2)
  sws <- sum(ws)
  mu <- sum(ws * y) / sws
  se <- sqrt(1 / sws)
  z <- mu / se
  crit <- k02z(0.5 + 0.5 * level)
  list(estimate = tau2, tau2_he = tau_he, tau2_dl = d$tau2, mu = mu, se = se,
       ci_lower = mu - crit * se, ci_upper = mu + crit * se, z = z,
       p_value = k02p2z(z), n = k,
       method = "Two-step DerSimonian-Laird tau^2 from the Hedges start (DerSimonian & Kacker 2007)")
}

# CANONICAL TEST
# y <- c(0.10,0.30,-0.20,0.45,0.05,0.22); v <- c(0.02,0.05,0.03,0.08,0.01,0.04)
# r <- matr(y, v)
# stopifnot(abs(r$tau2_he - 0.0121333333333333) < 1e-14,
#           abs(k02mm(y, v, 0) - r$tau2_dl) < 1e-15)

#' @rdname matr
#' @keywords internal
#' @export
morie_matr <- matr

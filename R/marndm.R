# SPDX-License-Identifier: AGPL-3.0-or-later
#' DerSimonian-Laird random-effects meta-analysis
#'
#' tau^2 = max(0, (Q - (k-1)) / C) with C = sum(w) - sum(w^2)/sum(w) and
#' w_i = 1/v_i; the pooled effect then uses 1/(v_i + tau^2).  Source
#' consulted: DerSimonian and Laird (1986), Controlled Clinical Trials 7,
#' 177-188, equations (5)-(9).  Verified against metafor::rma(method = "DL").
#'
#' @param yi study effect sizes.
#' @param vi within-study sampling variances.
#' @param level confidence level.
#' @return list: estimate, se, ci_lower, ci_upper, z, p_value, tau2, tau, Q,
#'   df, p_Q, weights, n, method.
#' @keywords internal
#' @examples
#' marndm(c(0.1, 0.3, -0.2), c(0.02, 0.05, 0.03))$tau2
#' @export
marndm <- function(yi, vi, level = 0.95) {
  y <- as.numeric(yi); v <- as.numeric(vi)
  d <- k02dl(y, v)
  se <- sqrt(d$var)
  z <- d$mu / se
  crit <- k02z(0.5 + 0.5 * level)
  ws <- 1 / (v + d$tau2)
  list(estimate = d$mu, se = se, ci_lower = d$mu - crit * se,
       ci_upper = d$mu + crit * se, z = z, p_value = k02p2z(z),
       tau2 = d$tau2, tau = sqrt(d$tau2), Q = d$Q, df = as.integer(d$df),
       p_Q = k02pchi(d$Q, d$df), weights = ws / sum(ws), n = length(y),
       method = "DerSimonian-Laird random-effects meta-analysis (DerSimonian & Laird 1986)")
}

# CANONICAL TEST
# r <- marndm(c(0.10,0.30,-0.20,0.45,0.05,0.22), c(0.02,0.05,0.03,0.08,0.01,0.04))
# stopifnot(abs(r$tau2 - 0.00494218900675024) < 1e-14)

#' @rdname marndm
#' @keywords internal
#' @export
morie_marndm <- marndm

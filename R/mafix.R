# SPDX-License-Identifier: AGPL-3.0-or-later
#' Fixed-effect (inverse-variance) meta-analysis
#'
#' Weights w_i = 1/v_i, pooled effect M = sum(w y)/sum(w), Var(M) = 1/sum(w),
#' and Cochran's Q = sum(w (y - M)^2) on k - 1 degrees of freedom.  Source
#' consulted: Borenstein, Hedges, Higgins and Rothstein (2009), Introduction
#' to Meta-Analysis, chapter 11.  Verified against metafor::rma(method = "FE").
#'
#' @param yi study effect sizes.
#' @param vi within-study sampling variances.
#' @param level confidence level.
#' @return list: estimate, se, ci_lower, ci_upper, z, p_value, Q, df, p_Q,
#'   weights, n, method.
#' @keywords internal
#' @examples
#' mafix(c(0.1, 0.3, -0.2), c(0.02, 0.05, 0.03))$estimate
#' @export
mafix <- function(yi, vi, level = 0.95) {
  y <- as.numeric(yi); v <- as.numeric(vi)
  fe <- k02fe(y, v)
  se <- sqrt(fe$var)
  z <- fe$mu / se
  crit <- k02z(0.5 + 0.5 * level)
  list(estimate = fe$mu, se = se, ci_lower = fe$mu - crit * se,
       ci_upper = fe$mu + crit * se, z = z, p_value = k02p2z(z),
       Q = fe$Q, df = as.integer(fe$df), p_Q = k02pchi(fe$Q, fe$df),
       weights = (1 / v) / fe$sw, n = length(y),
       method = "Fixed-effect inverse-variance meta-analysis (Borenstein et al. 2009, ch. 11)")
}

# CANONICAL TEST
# r <- mafix(c(0.10,0.30,-0.20,0.45,0.05,0.22), c(0.02,0.05,0.03,0.08,0.01,0.04))
# stopifnot(abs(r$estimate - 0.0849480968858132) < 1e-13, abs(r$Q - 5.88668685121107) < 1e-12)

#' @rdname mafix
#' @keywords internal
#' @export
morie_mafix <- mafix

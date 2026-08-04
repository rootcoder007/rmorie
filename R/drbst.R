# SPDX-License-Identifier: AGPL-3.0-or-later
#' Bootstrap inference for the doubly robust DiD estimator
#'
#' Sant'Anna and Zhao (2020), Doubly robust difference-in-differences
#' estimators, Journal of Econometrics 219(1), 101-122 (arXiv:1812.01723
#' -- FETCHED).  Equation (2.6): tau = E[(w1(D) - w0(D, X; pi))(dY -
#' mu_0(X))]; equation (2.7): w1 = D/E[D] and w0 = [pi(X)(1-D)/(1-pi(X))]
#' / E[pi(X)(1-D)/(1-pi(X))].  Inference is by the multiplier bootstrap on
#' the influence function, which section 3.2 recommends over the empirical
#' bootstrap.
#'
#' Determinism: Mammen's two-point multiplier is taken at van der Corput
#' points rather than drawn, so it keeps mean 1, variance 1 and third
#' moment 1 while both arms produce the identical replicate sequence.  The
#' standard error is reported as the replicate SD and, as the paper's
#' companion software does, as (q75 - q25)/(z75 - z25).
#'
#' @param y the outcome change dY, or period-1 outcome when y0 is given.
#' @param D treatment indicator.
#' @param X covariates (an intercept is added).
#' @param B bootstrap replicates.
#' @param alpha two-sided level.
#' @param y0 period-0 outcome.
#' @return list: estimate, se, se_sd, se_analytic, ci_lo, ci_hi, boot, n,
#'   B, method.
#' @keywords internal
#' @examples
#' Drdidboot(c(1, 2, 0, 3), c(1, 0, 1, 0), NULL, 20)$estimate
#' @export
Drdidboot <- function(y, D, X = NULL, B = 199, alpha = 0.05, y0 = NULL) {
  dy <- .s03vec(y)
  if (!is.null(y0)) dy <- dy - .s03vec(y0)
  fit <- .s03drdid(dy, D, X)
  inf <- fit$inf; n <- length(inf)
  boot <- numeric(as.integer(B))
  for (b in seq_len(as.integer(B)) - 1L) {
    s <- 0
    for (i in seq_len(n)) s <- s + .s03mammen(b * n + i - 1L) * inf[i]
    boot[b + 1L] <- fit$tau + s / n
  }
  q25 <- .s03quantile7(boot, 0.25); q75 <- .s03quantile7(boot, 0.75)
  se <- (q75 - q25) / (qnorm(0.75) - qnorm(0.25))
  se_sd <- if (length(boot) > 1L) .s03sd(boot, 1L) else NaN
  z <- qnorm(1 - as.numeric(alpha) / 2)
  list(estimate = fit$tau, se = se, se_sd = se_sd, se_analytic = fit$se,
       ci_lo = fit$tau - z * se, ci_hi = fit$tau + z * se, boot = boot,
       n = n, B = as.integer(B),
       method = "DR-DiD (Sant'Anna and Zhao 2020, eq. 2.6) with a deterministic Mammen multiplier bootstrap")
}

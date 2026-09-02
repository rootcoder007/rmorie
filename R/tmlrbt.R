# SPDX-License-Identifier: AGPL-3.0-or-later
#' TMLE with a bounded clever covariate
#'
#' van der Laan and Rubin (2006), The International Journal of
#' Biostatistics 2(1), art. 11, for the targeting step; Petersen, Porter,
#' Gruber, Wang and van der Laan (2012), Diagnosing and responding to
#' violations in the positivity assumption, Statistical Methods in Medical
#' Research 21(1), 31-54, for the response to a near-zero propensity
#' score: bound g away from 0 and 1 at a level chosen in advance, which
#' bounds H = D/g - (1-D)/(1-g) and so bounds the influence of any single
#' observation.  Neither source was retrievable here as a full text; the
#' truncation rule is quoted in its standard published form.  Truncation
#' trades bounded variance for a bias that does not vanish, so both the
#' bound and the number of observations it touched are reported -- the
#' diagnostic Petersen et al. insist on, not a silent fix.
#'
#' @param y,D outcome and treatment.
#' @param X covariates.
#' @param trim propensity bound.
#' @param alpha interval level.
#' @return list: estimate, se, ci_lo, ci_hi, n_trimmed, min_g, max_g,
#'   psi_untrimmed, eps, trim, n, method.
#' @keywords internal
#' @examples
#' Tmlerob(c(1, 0, 1, 1, 0, 1), c(1, 0, 1, 0, 1, 0))$n_trimmed
#' @export
Tmlerob <- function(y, D, X = NULL, trim = 0.025, alpha = 0.05) {
  fit <- .s03tmle(y, D, X, as.numeric(trim))
  raw <- .s03tmle(y, D, X, 0)
  g0 <- raw$g
  t <- as.numeric(trim)
  ntr <- 0L
  for (v in g0) if (v < t || v > 1 - t) ntr <- ntr + 1L
  z <- qnorm(1 - as.numeric(alpha) / 2)
  list(estimate = fit$psi, se = fit$se, ci_lo = fit$psi - z * fit$se,
       ci_hi = fit$psi + z * fit$se, n_trimmed = ntr,
       min_g = if (length(g0)) min(g0) else NaN,
       max_g = if (length(g0)) max(g0) else NaN,
       psi_untrimmed = raw$psi, eps = fit$eps, trim = t, n = length(g0),
       method = "TMLE with the propensity score bounded (Petersen et al. 2012 positivity rule)")
}

# SPDX-License-Identifier: AGPL-3.0-or-later
#' Robustness of an inference: percent bias to invalidate, RIR and ITCV
#'
#' With the threshold estimate delta# = t_crit * se, Frank et al give
#' the percent bias to invalidate an inference and the equivalent number
#' of cases that would have to be replaced (RIR).  Frank (2000) gives
#' the impact threshold for a confounding variable: an omitted variable
#' correlated \code{sqrt(ITCV)} with both the predictor and the outcome
#' would exactly overturn the inference.  Degrees of freedom follow the
#' reference implementation of these indices,
#' \code{df = n - n_covariates - 3}.
#'
#' Formula: pct = 100 (1 - delta#/|est|); RIR = n (1 - delta#/|est|);
#'   ITCV = (|r_xy| - r#)/(1 - r#) with r# = t_crit/sqrt(t_crit^2 + df).
#'
#' @param est Unstandardised coefficient.
#' @param se Its standard error.
#' @param n Sample size.
#' @param threshold Two-sided significance level alpha.
#' @param n_covariates Number of covariates besides the focal predictor.
#' @return List with \code{estimate} (percent bias), \code{pct_bias},
#'   \code{rir}, \code{rir_cases}, \code{delta_threshold}, \code{t},
#'   \code{t_crit}, \code{r_xy}, \code{r_crit}, \code{itcv},
#'   \code{r_cv}, \code{impact}, \code{df}, \code{significant},
#'   \code{n}, \code{method}.
#' @references Frank, Maroulis, Duong and Kelcey (2013), What would it
#'   take to change an inference?, Educational Evaluation and Policy
#'   Analysis 35(4):437-460, \doi{10.3102/0162373713493129}; Frank
#'   (2000), Impact of a confounding variable on a regression
#'   coefficient, Sociological Methods and Research 29(2):147-194.
#'   \doi{10.1177/0049124100029002001}
#' @export
Konfound <- function(est, se, n, threshold, n_covariates = 0) {
  est <- as.numeric(est); se <- as.numeric(se)
  n <- as.integer(n); alpha <- as.numeric(threshold); k <- as.integer(n_covariates)
  if (se <= 0) stop("konfound: se must be positive")
  if (!(alpha > 0 && alpha < 1)) stop("konfound: threshold must be a significance level in (0, 1)")
  df <- n - k - 3L
  if (df <= 0L) stop("konfound: not enough degrees of freedom")
  if (est == 0) stop("konfound: estimate is exactly zero")
  tcrit <- stats::qt(1 - alpha / 2, df)
  delta <- tcrit * se
  t <- est / se
  frac <- 1 - delta / abs(est)
  significant <- abs(t) > tcrit
  rxy <- t / sqrt(t^2 + df)
  rcrit <- tcrit / sqrt(tcrit^2 + df)
  itcv <- (abs(rxy) - rcrit) / (1 - rcrit)
  rcv <- if (itcv >= 0) sqrt(itcv) else -sqrt(-itcv)
  .t1_result(estimate = 100 * frac, pct_bias = 100 * frac, rir = n * frac,
             rir_cases = if (frac >= 0) floor(n * frac) else ceiling(n * frac),
             delta_threshold = delta, t = t, t_crit = tcrit, r_xy = rxy,
             r_crit = rcrit, itcv = itcv, r_cv = rcv,
             impact = if (itcv >= 0) rcv^2 else -rcv^2,
             df = df, significant = as.numeric(significant), n = n,
             method = "Frank et al (2013) percent bias / RIR and Frank (2000) ITCV")
}

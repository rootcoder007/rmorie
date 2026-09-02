# SPDX-License-Identifier: AGPL-3.0-or-later
#' Inverse probability weighting combined with survey design weights
#'
#' DuGoff, Schuler and Stuart recommend multiplying the propensity-score
#' weight by the survey design weight and estimating the treatment
#' effect as the difference of the two weighted (Hajek) group means.
#' Kish's effective sample size is reported because the combined weights
#' are usually far more variable than either component alone.
#'
#' Formula: w = d * (T/pi + (1 - T)/(1 - pi)).
#'
#' @param y Outcome vector.
#' @param T Treatment indicator, 0 or 1.
#' @param weights Survey design weights; \code{NULL} means all ones.
#' @param propensity Propensity scores, strictly inside (0, 1).
#' @return List with \code{estimate}, \code{mu1}, \code{mu0}, \code{se},
#'   \code{var1}, \code{var0}, \code{sum_w}, \code{ess}, \code{n1},
#'   \code{n}, \code{method}.
#' @references DuGoff, Schuler and Stuart (2014), Generalizing
#'   observational study results, Health Services Research
#'   49(1):284-303. \doi{10.1111/1475-6773.12090}
#' @export
#' @examples
#' set.seed(1)
#' Ipwsrv(y = rnorm(20), T = rbinom(20, 1, 0.5), weights = runif(20, 1, 2),
#'        propensity = runif(20, 0.3, 0.7))
Ipwsrv <- function(y, T, weights, propensity) {
  yv <- .s03vec(y); n <- length(yv)
  if (n == 0L) stop("ipw_with_survey_weights: y is empty")
  t <- .s03vec(T); pi <- .s03vec(propensity)
  if (length(t) != n || length(pi) != n) stop("ipw_with_survey_weights: y, T and propensity have different lengths")
  d <- if (!is.null(weights)) .s03vec(weights) else rep(1, n)
  if (length(d) != n) stop("ipw_with_survey_weights: weights and y have different lengths")
  if (any(pi <= 0 | pi >= 1)) stop("ipw_with_survey_weights: propensity must lie strictly in (0, 1)")
  if (any(d < 0)) stop("ipw_with_survey_weights: design weights must be non-negative")
  if (any(t != 0 & t != 1)) stop("ipw_with_survey_weights: T must be 0 or 1")
  w <- d * (t / pi + (1 - t) / (1 - pi))
  i1 <- t == 1; i0 <- t == 0
  s1 <- sum(w[i1]); s0 <- sum(w[i0])
  if (s1 <= 0 || s0 <= 0) stop("ipw_with_survey_weights: both treatment arms must be non-empty")
  mu1 <- sum(w[i1] * yv[i1]) / s1
  mu0 <- sum(w[i0] * yv[i0]) / s0
  v1 <- sum((w[i1] * (yv[i1] - mu1))^2) / s1^2
  v0 <- sum((w[i0] * (yv[i0] - mu0))^2) / s0^2
  sw <- sum(w); sw2 <- sum(w^2)
  ess <- if (sw2 > 0) sw^2 / sw2 else 0
  .t1_result(estimate = mu1 - mu0, mu1 = mu1, mu0 = mu0, se = sqrt(v1 + v0),
             var1 = v1, var0 = v0, sum_w = sw, ess = ess, n1 = sum(i1), n = n,
             method = "w = d (T/pi + (1-T)/(1-pi)); Hajek difference, DuGoff, Schuler & Stuart (2014)")
}

# SPDX-License-Identifier: AGPL-3.0-or-later
#' Odds ratio over a fixed interval of an interval-scaled predictor
#'
#' Source READ FROM THE CORPUS PDF, page rendered with pdftoppm:
#' Hedderich, Sachs and Reynarowych, Applied Statistics: Methods Using R,
#' section 8.4.4, printed page 834, equation (8.63):
#' \code{log psi(a, b) = beta1 (b - a)}, so
#' \code{psi(a, b) = exp(beta1 (b - a))}.  The intercept cancels, so the
#' odds ratio for moving the predictor from a to b depends on beta1 and
#' the span alone.
#'
#' A Wald interval is supplied when \code{se} is given: the log odds
#' ratio is \code{beta1 (b - a)} with standard error
#' \code{abs(b - a) se(beta1)}, which is (8.56) transported through the
#' same linear map.
#'
#' Book worked example, same page: from the Challenger data the book
#' estimates \code{beta1 = -0.2322}; a temperature rise of 10 degF gives
#' \code{psi(10) = exp(-2.322) = 0.098} and a drop of 10 degF gives
#' \code{psi(-10) = exp(2.322) = 10.2}.
#'
#' @param beta1 Logistic-regression slope on the logit scale.
#' @param a,b Endpoints of the interval; the default gives exp(beta1).
#' @param se Optional standard error of beta1; enables the Wald interval.
#' @param level Confidence level, strictly inside (0, 1).
#' @return list: or, logor, span, and with se also se_logor, ci_low,
#'   ci_high, level.
#' @examples
#' Logor(-0.2322, 0, 10)$or
#' @export
Logor <- function(beta1, a = 0, b = 1, se = NULL, level = 0.95) {
  beta1 <- as.numeric(beta1)[1]
  a <- as.numeric(a)[1]
  b <- as.numeric(b)[1]
  if (!is.finite(beta1) || !is.finite(a) || !is.finite(b)) {
    stop("beta1, a and b must be finite")
  }
  if (!(level > 0 && level < 1)) stop("level must be strictly between 0 and 1")
  span <- b - a
  lor <- beta1 * span
  out <- list(or = exp(lor), logor = lor, span = span)
  if (!is.null(se)) {
    se <- as.numeric(se)[1]
    if (!is.finite(se) || se <= 0) stop("se must be a finite positive number")
    se_lor <- abs(span) * se
    z <- stats::qnorm(0.5 + level / 2)
    out$se_logor <- se_lor
    out$ci_low <- exp(lor - z * se_lor)
    out$ci_high <- exp(lor + z * se_lor)
    out$level <- level
  }
  out
}

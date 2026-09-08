# SPDX-License-Identifier: AGPL-3.0-or-later
#' Wald statistic and confidence interval for logistic-regression coefficients
#'
#' Source READ FROM THE CORPUS PDF, page rendered with pdftoppm:
#' Hedderich, Sachs and Reynarowych, Applied Statistics: Methods Using R,
#' section 8.4, printed pages 829-830, equations (8.56) and (8.57):
#' \code{beta_i +/- z_{1-alpha/2} se(beta_i)} and
#' \code{W = beta_i / se(beta_i)}.  W is asymptotically standard normal
#' under H0: beta_i = 0, so the two-sided p-value is
#' \code{2 (1 - Phi(abs(W)))}.
#'
#' BOOK ERRATUM, confirmed by re-running the fit: the R input block
#' printed on page 829 lists t and d vectors that do not pair up --
#' fitting the printed vectors gives beta0 = 23.775, beta1 = -0.3667,
#' not the values in the printed output.  The printed output on the same
#' page is nevertheless the correct Challenger O-ring logistic fit
#' (beta0 = 15.0429, se 7.3786, z 2.039; beta1 = -0.2322, se 0.1082,
#' z -2.145, p 0.0415 / 0.0320).  The printed output is used as the
#' anchor; the printed input vectors are not.
#'
#' @param beta Estimated coefficients.
#' @param se Their standard errors; positive, same length as beta.
#' @param level Confidence level for the interval (8.56).
#' @param alpha Significance level for the per-coefficient decision.
#' @return list: beta, se, z, pvalue, ci_low, ci_high, reject, level,
#'   alpha.
#' @examples
#' Lrwald(c(15.0429, -0.2322), c(7.3786, 0.1082))$z
#' @export
Lrwald <- function(beta, se, level = 0.95, alpha = 0.05) {
  beta <- as.numeric(beta)
  se <- as.numeric(se)
  if (length(beta) == 0L) stop("beta must not be empty")
  if (length(beta) != length(se)) stop("beta and se must have the same length")
  if (any(!is.finite(se)) || any(se <= 0)) {
    stop("every standard error must be finite and positive")
  }
  if (!(level > 0 && level < 1)) stop("level must be strictly between 0 and 1")
  if (!(alpha > 0 && alpha < 1)) stop("alpha must be strictly between 0 and 1")
  z <- stats::qnorm(0.5 + level / 2)
  stat <- beta / se
  pval <- 2 * (1 - stats::pnorm(abs(stat)))
  list(
    beta = beta, se = se, z = stat, pvalue = pval,
    ci_low = beta - z * se, ci_high = beta + z * se,
    reject = pval < alpha, level = level, alpha = alpha
  )
}

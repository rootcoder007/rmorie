# SPDX-License-Identifier: AGPL-3.0-or-later
#' Deviance and Pearson goodness of fit for a Poisson regression
#'
#' Source READ FROM THE CORPUS PDF, page rendered with pdftoppm:
#' Hedderich, Sachs and Reynarowych, Applied Statistics: Methods Using R,
#' section 8.5, printed page 843, equations (8.80) and (8.81):
#' \code{D = 2 sum \[y_i log(y_i / lam_i) - (y_i - lam_i)\]}, printed also
#' as \code{2 sum y_i log(y_i / lam_i)}.  The two agree only when
#' \code{sum (y_i - lam_i) = 0}, which the book states one paragraph
#' above holds exactly when the model carries an intercept.  The general
#' form is computed; the third printed line is returned as
#' \code{D_nointercept} alongside \code{resid_sum} so the identity can
#' be inspected.
#'
#' BOOK ERRATUM in (8.81): the printed statistic is
#' \code{D ~= sum (y_i - lam_i) / lam_i}.  That numerator is not squared,
#' so with an intercept it sums to (nearly) zero by the identity just
#' quoted and cannot approximate a positive deviance.  The Pearson
#' chi-squared for a Poisson fit is
#' \code{sum (y_i - lam_i)^2 / lam_i}, which is what is computed; the
#' literal printed form is not implemented.
#'
#' \code{0 log 0} is taken as 0.
#'
#' @param y Observed non-negative counts.
#' @param lamhat Fitted Poisson means, strictly positive.
#' @param p Number of predictors excluding the intercept; when given,
#'   D is referred to chi-squared on n - p - 1 degrees of freedom.
#' @param alpha Significance level for the decision.
#' @return list: statistic, deviance, pearson_chisq, D_nointercept,
#'   resid_sum, df, pvalue, reject, n.
#' @examples
#' Poisdev(c(2, 3, 5), c(2.1, 3.2, 4.7))$deviance
#' @export
Poisdev <- function(y, lamhat, p = NULL, alpha = 0.05) {
  y <- as.numeric(y)
  lam <- as.numeric(lamhat)
  n <- length(y)
  if (n == 0L) stop("y must not be empty")
  if (length(lam) != n) stop("y and lamhat must have the same length")
  if (any(!is.finite(y)) || any(y < 0)) {
    stop("counts must be finite and non-negative")
  }
  if (any(!is.finite(lam)) || any(lam <= 0)) {
    stop("every fitted mean must be finite and positive")
  }
  t <- ifelse(y == 0, 0, y * log(y / lam))
  d <- 2 * sum(t - (y - lam))
  d3 <- 2 * sum(t)
  chi <- sum((y - lam)^2 / lam)
  rs <- sum(y - lam)
  df <- NULL
  pval <- NA_real_
  rej <- NULL
  if (!is.null(p)) {
    df <- n - as.integer(p) - 1L
    if (df < 1L) stop("n - p - 1 must be at least 1")
    pval <- stats::pchisq(d, df, lower.tail = FALSE)
    rej <- pval < alpha
  }
  list(
    statistic = d, deviance = d, pearson_chisq = chi, D_nointercept = d3,
    resid_sum = rs, df = df, pvalue = pval, reject = rej, n = n
  )
}

# SPDX-License-Identifier: AGPL-3.0-or-later
#' Prediction and mean-response intervals for the linear regression model
#'
#' Source READ FROM THE CORPUS PDF, page rendered with pdftoppm:
#' Hedderich, Sachs and Reynarowych, Applied Statistics: Methods Using R,
#' section 8.2.6, printed page 813, equations (8.36) and (8.37):
#' \code{yhat0 +/- t_{n-p-1, 1-alpha/2} sigma sqrt(1 + x0' (X'X)^-1 x0)}
#' and the same without the leading 1.  (8.36) is the interval for a
#' single future observation; (8.37) is the narrower interval for a mean
#' future value, carrying no Var(epsilon) term.  sigma is
#' \code{sqrt(RSS / (n - p - 1))} and p is the number of predictors
#' excluding the intercept.
#'
#' The book worked example on the same page fits litter size on body
#' weight and brain weight (n = 20, p = 2) and reports for
#' \code{x0 = c(1, 8.0, 0.4)} the estimate \code{yhat0 = 6.37} with 95%
#' prediction interval \code{4.49 ... 8.24}.
#'
#' The design matrix is passed exactly as the book builds it, with its
#' own leading column of ones, so that x0 is written the same way.
#'
#' @param X Design matrix INCLUDING its own leading column of ones.
#' @param y Response, length nrow(X).
#' @param x0 New design row, same width as X and likewise starting at 1.
#' @param level Confidence level.
#' @param mean FALSE (default) gives (8.36); TRUE gives (8.37).
#' @return list: fit, lower, upper, se_fit, sigma, df, tquant, leverage,
#'   coef, level, mean.
#' @examples
#' X <- cbind(1, 1:6); y <- c(2, 4, 5, 4, 6, 7)
#' Lmpi(X, y, c(1, 3))$fit
#' @export
Lmpi <- function(X, y, x0, level = 0.95, mean = FALSE) {
  X <- as.matrix(X)
  storage.mode(X) <- "double"
  n <- nrow(X)
  k <- ncol(X)
  if (n == 0L || k == 0L) stop("X must not be empty")
  y <- as.numeric(y)
  if (length(y) != n) stop("X and y must have the same number of rows")
  xn <- as.numeric(x0)
  if (length(xn) != k) stop("x0 must have the same length as a row of X")
  df <- n - k
  if (df < 1L) stop("need more observations than columns of X")
  if (!(level > 0 && level < 1)) stop("level must be strictly between 0 and 1")
  xtx <- t(X) %*% X
  xtxinv <- solve(xtx)
  beta <- as.numeric(xtxinv %*% (t(X) %*% y))
  rss <- sum((y - as.numeric(X %*% beta))^2)
  sigma <- sqrt(rss / df)
  lev <- as.numeric(t(xn) %*% xtxinv %*% xn)
  if (lev < 0) lev <- 0
  fit <- sum(xn * beta)
  root <- sqrt(if (mean) lev else 1 + lev)
  tq <- stats::qt(0.5 + level / 2, df)
  half <- tq * sigma * root
  list(
    fit = fit, lower = fit - half, upper = fit + half,
    se_fit = sigma * root, sigma = sigma, df = df, tquant = tq,
    leverage = lev, coef = beta, level = level, mean = mean
  )
}

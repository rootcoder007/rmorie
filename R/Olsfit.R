# SPDX-License-Identifier: AGPL-3.0-or-later
#' Linear model fit by least squares (ESL eq. 2.6)
#'
#' Hastie, Tibshirani and Friedman (2009), The Elements of Statistical
#' Learning, 2nd ed., Springer, Chapter 2, Section 2.3.1, book pp. 11-12
#' (PDF pp. 30-31).
#'
#' Yhat = beta0 + sum_j X_j betahat_j (2.1); Yhat = X' betahat (2.2);
#' RSS(beta) = (y - X beta)'(y - X beta) (2.4); the normal equations
#' X'(y - X beta) = 0 (2.5); and betahat = (X'X)^-1 X'y (2.6).
#'
#' Equation (2.15) f(x) ~= x'beta is the same linear form; (2.26)
#' Y = X'beta + eps and (2.29) Y = f(X) + eps are the additive-error models
#' this fit estimates; (2.32) RSS(theta) is (2.4) for a general parameterised
#' f, which for a linear f_theta is exactly this computation.  The normal
#' equations are solved directly, so the answer is (2.6) itself.
#'
#' @param X N-by-p matrix of inputs, one observation per row.
#' @param y N-vector of responses.
#' @param intercept logical; include the constant column of (2.1).
#' @return list: estimate, coefficients, intercept, fitted, residuals, rss,
#'   tss, r2, sigma2, df, n, p, method.
#' @examples
#' Olsfit(cbind(c(1, 2, 3, 4)), c(2, 4, 6, 8))$coefficients
#' @export
Olsfit <- function(X, y, intercept = TRUE) {
  Xm <- .s03mat(X)
  yv <- .s03vec(y)
  n <- nrow(Xm)
  if (n == 0L) stop("olsfit: X is empty")
  if (length(yv) != n) stop("olsfit: X and y must have the same number of rows")
  p <- ncol(Xm)
  if (p == 0L) stop("olsfit: X has no columns")
  D <- if (intercept) cbind(rep(1, n), Xm) else Xm
  q <- ncol(D)
  if (n < q) stop("olsfit: fewer observations than coefficients")
  beta <- .s03lstsq(D, yv, 0)
  fitted <- as.numeric(.s03matvec(D, beta))
  resid <- yv - fitted
  rss <- sum(resid * resid)
  ybar <- sum(yv) / n
  tss <- sum((yv - ybar)^2)
  dfres <- n - q
  sigma2 <- if (dfres > 0L) rss / dfres else NaN
  r2 <- if (tss > 0) 1 - rss / tss else NaN
  list(estimate = beta[1], coefficients = beta,
       intercept = if (intercept) beta[1] else NaN,
       fitted = fitted, residuals = resid, rss = rss, tss = tss, r2 = r2,
       sigma2 = sigma2, df = dfres, n = n, p = p,
       method = "Hastie-Tibshirani-Friedman (2009) ESL eqs. (2.1)-(2.6), normal equations")
}

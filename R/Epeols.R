# SPDX-License-Identifier: AGPL-3.0-or-later
#' Expected prediction error of least squares at a test point (ESL eq. 2.27)
#'
#' Hastie, Tibshirani and Friedman (2009), The Elements of Statistical
#' Learning, 2nd ed., Springer, Section 2.5, book pp. 24-26 (PDF pp. 43, 45).
#' For Y = X'beta + eps of equation (2.26),
#' EPE(x0) = Var(y0|x0) + Var_T(yhat0) + Bias^2(yhat0)
#'         = sigma^2 + E_T x0'(X'X)^-1 x0 sigma^2 + 0^2   (2.27),
#' and for large N with E(X) = 0, E_x0 EPE(x0) ~ sigma^2 (p/N) + sigma^2 (2.28).
#'
#' The bias term is identically zero because least squares is unbiased under
#' (2.26); it is returned rather than dropped.  sigma^2 defaults to the
#' residual estimate RSS/(N - p) from the same design.
#'
#' @param X N-by-p training design; (2.26) has no intercept.
#' @param y N-vector of responses, used only to estimate sigma^2.
#' @param x0 p-vector test point.
#' @param sigma2 optional noise variance.
#' @return list: estimate, epe, variance, bias2, sigma2, leverage, approx,
#'   n, p, method.
#' @examples
#' Epeols(cbind(rep(1, 5), 1:5), c(1, 3, 2, 5, 4), c(1, 3))$epe
#' @export
Epeols <- function(X, y, x0, sigma2 = NULL) {
  Xm <- .s03mat(X)
  yv <- .s03vec(y)
  z <- .s03vec(x0)
  n <- nrow(Xm)
  if (n == 0L) stop("epeols: X is empty")
  if (length(yv) != n) stop("epeols: X and y must have the same number of rows")
  p <- ncol(Xm)
  if (p == 0L) stop("epeols: X has no columns")
  if (length(z) != p) stop("epeols: x0 must have one entry per column of X")
  if (n <= p && is.null(sigma2)) stop("epeols: need N > p to estimate sigma2")
  A <- .s03crossprod(Xm)
  v <- .s03ridgesolve(A, z, 0)
  lev <- sum(z * v)
  if (is.null(sigma2)) {
    beta <- .s03lstsq(Xm, yv, 0)
    fit <- as.numeric(.s03matvec(Xm, beta))
    s2 <- sum((yv - fit)^2) / (n - p)
  } else {
    s2 <- as.numeric(sigma2)
    if (s2 < 0) stop("epeols: sigma2 must be non-negative")
  }
  var <- lev * s2
  bias2 <- 0
  epe <- s2 + var + bias2
  approx <- s2 * (p / n) + s2
  list(estimate = epe, epe = epe, variance = var, bias2 = bias2, sigma2 = s2,
       leverage = lev, approx = approx, n = n, p = p,
       method = "Hastie-Tibshirani-Friedman (2009) ESL eqs. (2.26)-(2.28)")
}

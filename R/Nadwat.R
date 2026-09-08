# SPDX-License-Identifier: AGPL-3.0-or-later
#' Nadaraya-Watson and local linear kernel regression (ESL eqs. 2.40-2.42)
#'
#' Hastie, Tibshirani and Friedman (2009), The Elements of Statistical
#' Learning, 2nd ed., Springer, Section 2.8.2, book p. 35 (PDF p. 54):
#' K_lambda(x0, x) = (1/lambda) exp(-||x - x0||^2 / (2 lambda)) (2.40),
#' fhat(x0) = sum_i K y_i / sum_i K (2.41), and the locally weighted
#' criterion RSS(f_theta, x0) = sum_i K (y_i - f_theta(x_i))^2 (2.42) with
#' f_theta(x) = theta0 (which reproduces 2.41) or theta0 + theta1 x, the
#' popular local linear regression model.
#'
#' In (2.40) lambda is the VARIANCE of the Gaussian, not its standard
#' deviation, so the exponent divides by 2 lambda.  The leading 1/lambda
#' cancels in (2.41) but is kept in the returned weights.
#'
#' @param x N-by-p matrix of inputs.
#' @param y N-vector of responses.
#' @param x0 target point.
#' @param lam lambda of (2.40), the Gaussian variance.
#' @param degree 0 for Nadaraya-Watson, 1 for local linear.
#' @return list: estimate, fit, weights, wsum, coef, lam, n, p, degree,
#'   method.
#' @examples
#' Nadwat(1:5, c(1, 2, 3, 4, 5), 3, lam = 1)$fit
#' @export
Nadwat <- function(x, y, x0, lam, degree = 0) {
  Xm <- .s03mat(x)
  yv <- .s03vec(y)
  n <- nrow(Xm)
  if (n == 0L) stop("nadwat: x is empty")
  if (length(yv) != n) stop("nadwat: x and y must have the same number of rows")
  p <- ncol(Xm)
  z <- .s03vec(x0)
  if (length(z) != p) stop("nadwat: x0 must have one entry per column of x")
  lam <- as.numeric(lam)
  if (lam <= 0) stop("nadwat: lambda must be positive")
  degree <- as.integer(degree)
  if (!(degree %in% c(0L, 1L))) stop("nadwat: degree must be 0 or 1")
  w <- numeric(n)
  for (i in seq_len(n)) {
    d2 <- sum((Xm[i, ] - z)^2)
    w[i] <- exp(-d2 / (2 * lam)) / lam
  }
  wsum <- sum(w)
  if (wsum <= 0) stop("nadwat: all kernel weights underflowed to zero")
  if (degree == 0L) {
    fit <- sum(w * yv) / wsum
    coef <- fit
  } else {
    q <- p + 1L
    if (n < q) stop("nadwat: fewer observations than local linear coefficients")
    D <- matrix(0, n, q)
    D[, 1L] <- 1
    for (a in seq_len(p)) D[, a + 1L] <- Xm[, a] - z[a]
    A <- matrix(0, q, q)
    for (a in seq_len(q)) for (b in seq_len(q)) A[a, b] <- sum(w * D[, a] * D[, b])
    rhs <- numeric(q)
    for (a in seq_len(q)) rhs[a] <- sum(w * D[, a] * yv)
    coef <- .s03ridgesolve(A, rhs, 0)
    fit <- coef[1]
  }
  list(estimate = fit, fit = fit, weights = w, wsum = wsum, coef = coef,
       lam = lam, n = n, p = p, degree = degree,
       method = "Hastie-Tibshirani-Friedman (2009) ESL eqs. (2.40)-(2.42)")
}

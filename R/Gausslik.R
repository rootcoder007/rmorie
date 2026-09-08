# SPDX-License-Identifier: AGPL-3.0-or-later
#' Gaussian conditional log-likelihood (ESL eq. 2.35)
#'
#' Hastie, Tibshirani and Friedman (2009), The Elements of Statistical
#' Learning, 2nd ed., Springer, Section 2.6.3, book p. 31 (PDF p. 50):
#' L(theta) = sum_i log Pr_theta(y_i) (2.33) with
#' Pr(Y|X, theta) = N(f_theta(X), sigma^2) (2.34), giving
#' L = -(N/2) log(2 pi) - N log sigma - (1/(2 sigma^2)) sum (y_i - f_i)^2 (2.35).
#'
#' When sigma is not supplied it is profiled at its maximum-likelihood value
#' sqrt(RSS/N), which is what makes least squares and maximum likelihood
#' coincide here.
#'
#' @param y N-vector of observations.
#' @param mu fitted values f_theta(x_i); a scalar is recycled.
#' @param sigma optional standard deviation.
#' @return list: estimate, loglik, rss, sigma, n, aic, method.
#' @examples
#' Gausslik(c(1, 2, 3), c(1.1, 2.1, 2.9), sigma = 1)$loglik
#' @export
Gausslik <- function(y, mu, sigma = NULL) {
  yv <- .s03vec(y)
  n <- length(yv)
  if (n == 0L) stop("gausslik: y is empty")
  mv <- .s03vec(mu)
  if (length(mv) == 1L) mv <- rep(mv, n)
  if (length(mv) != n) stop("gausslik: y and mu must have the same length")
  rss <- sum((yv - mv)^2)
  if (is.null(sigma)) {
    if (rss <= 0) stop("gausslik: cannot profile sigma from a zero residual sum of squares")
    s <- sqrt(rss / n)
  } else {
    s <- as.numeric(sigma)
    if (s <= 0) stop("gausslik: sigma must be positive")
  }
  ll <- -0.5 * n * log(2 * pi) - n * log(s) - rss / (2 * s * s)
  list(estimate = ll, loglik = ll, rss = rss, sigma = s, n = n,
       aic = -2 * ll + 2,
       method = "Hastie-Tibshirani-Friedman (2009) ESL eqs. (2.33)-(2.35)")
}

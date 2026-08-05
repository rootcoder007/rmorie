# SPDX-License-Identifier: AGPL-3.0-or-later
#' Linear basis expansion fitted by least squares (ESL eqs. 2.30, 2.43)
#'
#' Hastie, Tibshirani and Friedman (2009), The Elements of Statistical
#' Learning, 2nd ed., Springer, Sections 2.6.1 and 2.8.3, book pp. 30 and
#' 35-36 (PDF pp. 49, 54-55):
#' f_theta(x) = sum_k h_k(x) theta_k (2.30) = sum_m theta_m h_m(x) (2.43),
#' minimising RSS(theta) = sum_i (y_i - f_theta(x_i))^2 (2.32).
#'
#' The dictionaries are the ones the book names: polynomial (x^k),
#' trigonometric (cos/sin harmonics), and the linear spline basis
#' b1 = 1, b2 = x, b_{m+2}(x) = (x - t_m)_+.  The basis functions carry no
#' hidden parameters, so the minimisation is closed form and is solved by the
#' normal equations rather than by search.
#'
#' @param x N-vector of scalar inputs.
#' @param y N-vector of responses.
#' @param kind one of "poly", "trig", "spline".
#' @param M polynomial degree, or number of harmonics for "trig".
#' @param knots knots t_m for the linear spline basis.
#' @return list: estimate, theta, basis, fitted, residuals, rss, tss, r2,
#'   K, n, method.
#' @examples
#' Basisexp(1:6, c(1, 4, 9, 16, 25, 36), kind = "poly", M = 2)$theta
#' @export
Basisexp <- function(x, y, kind = "poly", M = 3, knots = NULL) {
  xv <- .s03vec(x)
  yv <- .s03vec(y)
  n <- length(xv)
  if (n == 0L) stop("basisexp: x is empty")
  if (length(yv) != n) stop("basisexp: x and y must have the same length")
  M <- as.integer(M)
  if (M < 0L) stop("basisexp: M must be non-negative")
  if (identical(kind, "poly")) {
    H <- matrix(0, n, M + 1L)
    for (j in 0:M) H[, j + 1L] <- xv^j
  } else if (identical(kind, "trig")) {
    H <- matrix(0, n, 2L * M + 1L)
    H[, 1L] <- 1
    if (M > 0L) for (j in seq_len(M)) {
      H[, 2L * j] <- cos(j * xv)
      H[, 2L * j + 1L] <- sin(j * xv)
    }
  } else if (identical(kind, "spline")) {
    if (is.null(knots)) stop("basisexp: the spline basis needs knots")
    tk <- .s03vec(knots)
    H <- matrix(0, n, 2L + length(tk))
    H[, 1L] <- 1
    H[, 2L] <- xv
    if (length(tk) > 0L) for (j in seq_along(tk)) H[, 2L + j] <- pmax(xv - tk[j], 0)
  } else {
    stop("basisexp: kind must be 'poly', 'trig' or 'spline'")
  }
  K <- ncol(H)
  if (n < K) stop("basisexp: fewer observations than basis functions")
  theta <- .s03lstsq(H, yv, 0)
  fitted <- as.numeric(.s03matvec(H, theta))
  resid <- yv - fitted
  rss <- sum(resid * resid)
  ybar <- sum(yv) / n
  tss <- sum((yv - ybar)^2)
  list(estimate = theta[1], theta = theta, basis = H, fitted = fitted,
       residuals = resid, rss = rss, tss = tss,
       r2 = if (tss > 0) 1 - rss / tss else NaN, K = K, n = n,
       method = "Hastie-Tibshirani-Friedman (2009) ESL eqs. (2.30), (2.32), (2.43)")
}

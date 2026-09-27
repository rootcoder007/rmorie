# SPDX-License-Identifier: AGPL-3.0-or-later
#' Median nearest-neighbour radius in the unit ball
#'
#' (1 - (1/2)^(1/N))^(1/p), the median distance from the origin to the nearest
#' of N points uniform in the p-dimensional unit ball (ESL eq 2.24).
#'
#' @param N Number of points.
#' @param p Dimension.
#' @return Named list: median_radius.
#' @references Hastie, Tibshirani & Friedman (2009), sec. 2.5.
#' @examples
#' morie_esl_median_nn_radius(500, 10)$median_radius
#' @export
morie_esl_median_nn_radius <- function(N, p) {
  if (N < 1 || p < 1) stop("need N >= 1 and p >= 1", call. = FALSE)
  list(median_radius = (1 - 0.5^(1 / N))^(1 / p))
}

#' Test-set R-squared relative to the constant model
#'
#' MSE0 = ave (ybar - mu)^2, MSE = ave (fhat - mu)^2 and R^2 = (MSE0 - MSE) /
#' MSE0 (ESL eqs 9.23-9.24).
#'
#' @param mu True mean at the test points.
#' @param fitted Predictions at the test points.
#' @param ybar Training mean.
#' @return Named list: mse, mse0, r2.
#' @references Hastie, Tibshirani & Friedman (2009), sec. 9.4.2.
#' @examples
#' morie_esl_test_r2(c(1, 2, 3), c(1.1, 1.8, 3.3), 2.1)$r2
#' @export
morie_esl_test_r2 <- function(mu, fitted, ybar) {
  if (length(mu) != length(fitted) || !length(mu)) stop("mu and fitted must be non-empty and of equal length", call. = FALSE)
  mse <- mean((fitted - mu)^2)
  mse0 <- mean((ybar - mu)^2)
  if (mse0 == 0) stop("the constant model is exact; R^2 is undefined", call. = FALSE)
  list(mse = mse, mse0 = mse0, r2 = (mse0 - mse) / mse0)
}

#' Evaluate a hyperplane: value, side and distance
#'
#' f(x) = beta0 + beta' x; sign(f) is the side and |f| / |beta| the Euclidean
#' distance to the hyperplane (ESL eq 4.40; the signed distance is f(x) /
#' |beta|).
#'
#' @param X Points, one per row (or a single vector).
#' @param beta Normal vector (not all zero).
#' @param beta0 Intercept.
#' @return Named list: value, side, distance, on_plane, n.
#' @references Hastie, Tibshirani & Friedman (2009), sec. 4.5.
#' @examples
#' hyperplane_side(rbind(c(1, 2), c(0, -1)), c(3, 4), -2)$distance
#' @export
hyperplane_side <- function(X, beta, beta0 = 0) {
  X <- if (is.null(dim(X))) rbind(X) else as.matrix(X)
  nb <- sqrt(sum(beta^2))
  if (nb == 0) stop("beta must not be all zero", call. = FALSE)
  v <- drop(X %*% beta) + beta0
  list(value = v, side = sign(v), distance = abs(v) / nb, on_plane = v == 0, n = nrow(X))
}

#' Correlation distance between two objects
#'
#' d = 1 - r with r the Pearson correlation of the two profiles over the
#' features (the correlation proximity of ESL sec 14.3.3).
#'
#' @param x,y Profiles of equal length (>= 2).
#' @return Named list: estimate (1 - r), pearson_r, dim.
#' @references Hastie, Tibshirani & Friedman (2009), sec. 14.3.3.
#' @examples
#' correlation_dist(c(1, 2, 4, 3), c(2, 1, 5, 5))$estimate
#' @export
correlation_dist <- function(x, y) {
  if (length(x) != length(y)) stop("x and y must have same length.", call. = FALSE)
  if (length(x) < 2) stop("Need at least 2 observations.", call. = FALSE)
  r <- stats::cor(x, y)
  list(estimate = 1 - r, pearson_r = r, dim = length(x))
}

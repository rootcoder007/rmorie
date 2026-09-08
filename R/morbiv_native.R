# SPDX-License-Identifier: AGPL-3.0-or-later
#' Bivariate Moran I between two variables
#'
#' Spatial correlation between x and the spatial lag of y,
#' \eqn{I_B = \sum_i x_i (Wy)_i / \sum_i x_i^2}, the slope of a
#' regression of Wy on x, with both variables standardized and W
#' row-standardized. Caveat from the source: the statistic ignores the
#' in-place correlation between x and y at the same location and can
#' overstate spatial association when that is strong.
#'
#' Standardization follows the spdep moran_bv reference implementation:
#' mean 0 and unit sample variance (divisor n-1).
#'
#' @param x Focal variable, length n.
#' @param y Lagged variable, length n.
#' @param W Spatial weights matrix, n by n; row-standardized internally
#'   by default.
#' @param scale Standardize x and y.
#' @param row_standardize Divide each row of W by its sum.
#' @return List with statistic, lag_y, x_std, y_std, n.
#' @references Anselin, L., Syabri, I. and Smirnov, O. (2002).
#'   Visualizing multivariate spatial correlation with dynamically
#'   linked windows. In New Tools for Spatial Data Analysis, CSISS,
#'   Santa Barbara.
#'
#'   Anselin, L. GeoDa workbook, Global Spatial Autocorrelation (2),
#'   bivariate Moran scatter plot. Archived:
#'   fetched-wave3/anselin-geoda-workbook-lab5b-bivariate-morans-i.html.
#'
#'   Reference implementation spdep moran_bv (CRAN, source read
#'   directly).
#' @examples
#' W <- matrix(0, 4, 4); W[cbind(1:3, 2:4)] <- 1; W[cbind(2:4, 1:3)] <- 1
#' bivariate_morans_i(c(3, 1, 4, 1), c(2, 7, 1, 8), W)
#' @export
bivariate_morans_i <- function(x, y, W, scale = TRUE, row_standardize = TRUE) {
  x <- as.numeric(x)
  y <- as.numeric(y)
  W <- as.matrix(W)
  n <- length(x)
  if (length(y) != n) stop("`x` and `y` must have equal length")
  if (!all(dim(W) == c(n, n))) stop("W must be n by n")
  if (row_standardize) {
    rs <- rowSums(W)
    rs[rs == 0] <- 1
    W <- W / rs
  }
  if (scale) {
    x <- (x - mean(x)) / stats::sd(x)
    y <- (y - mean(y)) / stats::sd(y)
  }
  lag_y <- as.numeric(W %*% y)
  stat <- sum(x * lag_y) / sum(x^2)
  list(statistic = stat, lag_y = lag_y, x_std = x, y_std = y, n = n,
       method = "Bivariate Moran I (Anselin-Syabri-Smirnov 2002)")
}

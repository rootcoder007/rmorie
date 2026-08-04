# SPDX-License-Identifier: AGPL-3.0-or-later
#' Penalized residual sum of squares of ridge regression.
#'
#' Formula: PRSS(beta, lambda) = sum_i (y_i - b0 - sum_j x_ij b_j)^2 + lambda sum_j b_j^2
#'
#' @param X Design matrix, one record per row.
#' @param y Response vector of length n.
#' @param beta Coefficient vector.
#' @param lam Regularization parameter lambda; must be non-negative.
#' @param add_intercept Treat the first entry of beta as an unpenalized intercept and prepend a column of ones to X.
#'
#' @return List with ``prss``, ``rss``, ``penalty``, ``lambda``, ``n``, ``p``.
#' @references Montesinos Lopez, Montesinos Lopez and Crossa (2022), Multivariate Statistical Machine Learning Methods for Genomic Prediction, Springer, doi:10.1007/978-3-030-89010-0.  Chapter 3, Sect. 3.6.1 p. 81: PRSS_lambda(beta) = RSS(beta) + lambda beta'D beta with D = diag(0, 1, ..., 1), so the intercept is not penalized.  Read from the chapter PDF, not recalled.
#' @export
Ridgeobj <- function(X, y, beta, lam, add_intercept = TRUE) {
  Xm <- .t1_mat(X)
  if (isTRUE(add_intercept)) Xm <- .t1_cbind1(Xm)
  y <- .t1_vec(y); b <- .t1_vec(beta); lam <- as.numeric(lam)
  n <- nrow(Xm); p <- ncol(Xm)
  if (n != length(y)) stop("X must have one row per entry of y")
  if (length(b) != p) stop("beta must have one entry per column of the design")
  if (lam < 0) stop("lambda must be non-negative")
  rss <- sum((y - as.numeric(Xm %*% b))^2)
  start <- if (isTRUE(add_intercept)) 2L else 1L
  pen <- lam * sum(b[start:p]^2)
  .t1_result(prss = rss + pen, rss = rss, penalty = pen, lambda = lam,
             n = n, p = p, method = "Ridge penalized RSS, MVSML Sect. 3.6.1")
}

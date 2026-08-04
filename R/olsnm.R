# SPDX-License-Identifier: AGPL-3.0-or-later
#' Ordinary least squares through the normal equations.
#'
#' Formula: (X'X) beta = X'y;  beta = (X'X)^-1 X'y;  Var(beta) = sigma2 (X'X)^-1;  H = X(X'X)^-1 X'
#'
#' @param X Design matrix, one record per row.
#' @param y Response vector of length n.
#' @param add_intercept Prepend a column of ones to X.
#'
#' @return List with ``beta``, ``fitted``, ``resid``, ``rss``, ``sigma2``, ``se``, ``leverage``, ``n``, ``p``.
#' @references Montesinos Lopez, Montesinos Lopez and Crossa (2022), Multivariate Statistical Machine Learning Methods for Genomic Prediction, Springer, doi:10.1007/978-3-030-89010-0.  Chapter 3, Sect. 3.2 pp. 72-73: setting the gradient of the residual sum of squares to zero gives the normal equations (X'X)beta = X'Y with unique solution beta = (X'X)^-1 X'y, variance sigma2 (X'X)^-1, and hat matrix H = X(X'X)^-1 X'.  sigma2 is the unbiased residual variance RSS/(n - p).  Read from the chapter PDF, not recalled.
#' @export
Olsnormeq <- function(X, y, add_intercept = TRUE) {
  Xm <- .t1_mat(X)
  if (isTRUE(add_intercept)) Xm <- .t1_cbind1(Xm)
  y <- .t1_vec(y)
  n <- nrow(Xm); p <- ncol(Xm)
  if (n != length(y)) stop("X must have one row per entry of y")
  if (n <= p) stop("need more records than columns for the residual variance")
  fit <- .t1_lstsq(Xm, y)
  rss <- sum(fit$resid^2); s2 <- rss / (n - p)
  .t1_result(beta = fit$beta, fitted = fit$fitted, resid = fit$resid,
             rss = rss, sigma2 = s2,
             se = sqrt(s2 * diag(fit$xtxinv)),
             leverage = .t1_hatdiag(Xm, fit$xtxinv), n = n, p = p,
             method = "OLS via the normal equations, MVSML Sect. 3.2")
}

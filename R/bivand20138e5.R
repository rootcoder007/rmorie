# SPDX-License-Identifier: AGPL-3.0-or-later
#' Spatial trend surface: linear mean plus correlated error.
#'
#' Z(s) = sum_{j=0}^p X_j(s) beta_j + e(s) = X beta + e(s), fitted by
#' ordinary least squares; the residuals are what a variogram estimator
#' should be given.
#'
#' @param X Spatial regressors, excluding the intercept by default.
#' @param z Observed values.
#' @param addintercept Prepend the column X_0(s) = 1.
#'
#' @return List with beta, fitted, resid, rss, sigma2, n, p.
#' @references Bivand, Pebesma and Gomez-Rubio (2013), Equation (8.5),
#'   p. 218.  Read from the corpus PDF.
#' @export
Spatrend <- function(X, z, addintercept = TRUE) {
  Xm <- .t1_mat(X); z <- .t1_vec(z); n <- nrow(Xm)
  if (length(z) != n) stop("X must have one row per observation")
  if (isTRUE(addintercept)) Xm <- .t1_cbind1(Xm)
  dimnames(Xm) <- NULL
  p <- ncol(Xm)
  if (n <= p) stop("need more observations than columns")
  f <- .t1_lstsq(Xm, z)
  rss <- sum(f$resid^2)
  .t1_result(beta = f$beta, fitted = f$fitted, resid = f$resid, rss = rss,
             sigma2 = rss / (n - p), n = n, p = p,
             method = "Spatial trend surface (Bivand et al. 2013 eq. 8.5)")
}

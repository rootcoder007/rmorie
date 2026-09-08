# SPDX-License-Identifier: AGPL-3.0-or-later
#' Anderson-Rubin weak-instrument-robust test
#'
#' Source FETCHED: the reference implementation \code{AR.test} in the
#' CRAN package \pkg{ivmodel} (file \code{R/AR.r}), implementing
#' Anderson, T. W. and Rubin, H. (1949), Annals of Mathematical
#' Statistics 20, 46-63.  The 1949 paper was not reachable here.  With
#' \code{temp = Y - beta0 D} after the exogenous covariates have been
#' partialled out of Y, D and Z,
#' \code{Fstat = ||P_Z temp||^2 / ||M_Z temp||^2 * (n - k - L)/L} and
#' \code{p = 1 - pf(Fstat, L, n - k - L)}.
#'
#' @param y Numeric outcome of length n.
#' @param X Numeric endogenous regressor of length n.
#' @param Z Numeric n x L matrix of instruments.
#' @param beta0 Null value of the structural coefficient.  Default 0.
#' @param X_exog Optional n x q matrix of included exogenous covariates.
#' @param add_intercept Include an intercept.  Default TRUE.
#' @return list: statistic, p_value, df1, df2, beta0, n, n_instruments,
#'   method.
#' @examples
#' set.seed(1)
#' Z <- matrix(rnorm(120), 60, 2)
#' D <- Z %*% c(1, 0.5) + rnorm(60)
#' Ivartest(1.5 * D + rnorm(60), D, Z, 1.5)$p_value
#' @export
Ivartest <- function(y, X, Z, beta0 = 0, X_exog = NULL, add_intercept = TRUE) {
  y <- as.numeric(y)
  n <- length(y)
  d <- as.numeric(X)
  Z <- as.matrix(Z)
  if (nrow(Z) != n) Z <- t(Z)
  C <- NULL
  if (add_intercept) C <- cbind(C, rep(1, n))
  if (!is.null(X_exog)) {
    Xe <- as.matrix(X_exog)
    if (nrow(Xe) != n) Xe <- t(Xe)
    C <- cbind(C, Xe)
  }
  k <- if (is.null(C)) 0L else ncol(C)
  L <- ncol(Z)
  df2 <- n - k - L
  if (L < 1 || df2 < 1) stop("need L >= 1 and n > k + L")
  po <- function(M) if (is.null(C)) M else M - C %*% qr.solve(C, M)
  ya <- as.numeric(po(matrix(y, n, 1)))
  da <- as.numeric(po(matrix(d, n, 1)))
  Za <- po(Z)
  temp <- ya - as.numeric(beta0) * da
  fit <- as.numeric(Za %*% qr.solve(Za, temp))
  ssf <- sum(fit * fit)
  sse <- sum(temp * temp) - ssf
  if (sse <= 0) stop("degenerate fit: residual sum of squares is not positive")
  stat <- ssf / sse * df2 / L
  list(
    statistic = stat, p_value = stats::pf(stat, L, df2, lower.tail = FALSE),
    df1 = L, df2 = as.integer(df2), beta0 = as.numeric(beta0),
    n = n, n_instruments = L,
    method = "Anderson-Rubin weak-IV-robust test (Anderson and Rubin 1949)"
  )
}

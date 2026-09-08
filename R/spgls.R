# SPDX-License-Identifier: AGPL-3.0-or-later
#' Generalised least squares with a known covariance matrix
#'
#' When the errors are spatially correlated with known Var(Z(s)) = Sigma,
#' the efficient estimator is
#' beta_gls = (X' Sigma^-1 X)^-1 X' Sigma^-1 Z, with variance
#' (X' Sigma^-1 X)^-1.
#'
#' Two properties worth stating, because both are easy to get wrong.
#' With Sigma = sigma^2 I the estimator reduces EXACTLY to ordinary least
#' squares; an implementation that whitens the wrong way round fails
#' that. And OLS remains unbiased under correlated errors while its usual
#' standard errors do not -- they are computed as if Sigma were scalar --
#' so the OLS estimate and its correct variance
#' (X'X)^-1 X' Sigma X (X'X)^-1 are returned alongside.
#'
#' @param x Design matrix (n by p).
#' @param y Response, length n.
#' @param sigma Known (n by n) error covariance; identity when NULL,
#'   which makes this OLS.
#' @return Named list: beta, vcov, se, residuals, beta_ols, se_ols_naive,
#'   se_ols_correct.
#' @references Schabenberger & Gotway (2005), Sec 6.2.3 "Generalized
#'   Least Squares -- Inference and Diagnostics", p. 341.
#' @examples
#' n <- 30
#' X <- cbind(1, runif(n))
#' spgls(X, X %*% c(2, -1) + rnorm(n, 0, 0.3), diag(n))
#' @export
spgls <- function(x, y, sigma = NULL) {
  X <- as.matrix(x)
  z <- as.numeric(y)
  n <- length(z)
  if (nrow(X) != n) stop("`x` and `y` must have the same number of rows")
  S <- if (is.null(sigma)) diag(n) else as.matrix(sigma)
  if (!identical(dim(S), c(n, n))) {
    stop("`sigma` must be ", n, " by ", n, " to match the data")
  }
  Sinv_X <- solve(S, X)
  XtSinvX <- crossprod(X, Sinv_X)
  vcov <- solve(XtSinvX)
  beta <- as.numeric(vcov %*% crossprod(Sinv_X, z))

  XtX_inv <- solve(crossprod(X))
  beta_ols <- as.numeric(XtX_inv %*% crossprod(X, z))
  resid_ols <- z - as.numeric(X %*% beta_ols)
  s2 <- sum(resid_ols^2) / max(n - ncol(X), 1)
  se_naive <- sqrt(diag(s2 * XtX_inv))
  vcov_ols <- XtX_inv %*% crossprod(X, S %*% X) %*% XtX_inv

  list(beta = beta, vcov = vcov, se = sqrt(diag(vcov)),
       residuals = z - as.numeric(X %*% beta), beta_ols = beta_ols,
       se_ols_naive = se_naive, se_ols_correct = sqrt(diag(vcov_ols)))
}

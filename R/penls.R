# SPDX-License-Identifier: AGPL-3.0-or-later

#' Elastic-net regression via coordinate descent (base R)
#'
#' Uses glmnet if available; otherwise the base-R coordinate-descent
#' fallback. Both solve:
#'
#'   min 1/(2n) ||y - X beta||^2 + lam (alpha ||beta||_1 + (1-alpha)/2 ||beta||_2^2).
#'
#' @param x (n x p) predictor matrix.
#' @param y Numeric response.
#' @param alpha 0 (ridge) to 1 (LASSO).
#' @param lam Penalty strength.
#' @param max_iter,tol Convergence controls.
#' @return list(estimate, beta, intercept, se, alpha, lam, n_iter, n, p, method).
#' @references Friedman, Hastie & Tibshirani (2010); Montesinos Lopez Ch 6.
#' @examples
#' # See the package vignettes for usage examples:
#' #   vignette(package = "rmorie")
#' @export
morie_penalized_regression <- function(x, y, alpha = 0.5, lam = 1.0,
                                 max_iter = 1000, tol = 1e-6) {
  x <- .morie_ensure_design_matrix(x)
  X <- as.matrix(x)
  y <- as.numeric(y)
  n <- nrow(X)
  p <- ncol(X)
  # Native elastic-net coordinate descent (shared core in
  # smallstats_native.R); cross-validated against glmnet in tests.
  fit <- .morie_coord_descent(X, y, alpha = alpha, lambda = lam,
                              max_iter = max_iter, tol = tol)
  y_hat <- as.numeric(X %*% fit$beta) + fit$intercept
  resid <- y - y_hat
  se <- sqrt(sum(resid^2) / max(n - p, 1))
  list(
    estimate = mean(abs(fit$beta)), beta = fit$beta,
    intercept = fit$intercept, y_hat = y_hat, se = se,
    alpha = alpha, lam = lam, n_iter = fit$n_iter,
    n = n, p = p, method = "Elastic-net coordinate descent (native)"
  )
}

# CANONICAL TEST
# set.seed(10); X <- matrix(rnorm(120), 30, 4); b <- c(1,0,-1,0)
# y <- X %*% b + 0.1*rnorm(30); morie_penalized_regression(X, y, alpha=1, lam=0.05)$beta

# SPDX-License-Identifier: AGPL-3.0-or-later
#' Best-linear-predictor calibration test for heterogeneous treatment effects
#'
#' Source FETCHED (reference implementation): \code{test_calibration} in
#' the CRAN package \pkg{grf} (grf 2.6.1, \code{R/forest_summary.R}),
#' implementing the calibration check of Chernozhukov, Demirer, Duflo
#' and Fernandez-Val (2018), arXiv 1712.04802, in the form Athey and
#' Wager (2019) use.  The package source regresses, with no intercept,
#' \code{target = Y - Yhat} on
#' \code{(W - What) * mean(tauhat)} and
#' \code{(W - What) * (tauhat - mean(tauhat))}, with HC3 sandwich SEs
#' and p-values converted to one-sided.
#'
#' The coefficient on the mean term tests whether the average treatment
#' effect is right; the coefficient on the differential term is 1 when
#' the CATE estimates carry real signal, and a coefficient far from 1,
#' or a one-sided p-value that fails to reject 0, says the heterogeneity
#' estimates are biased or noise.
#'
#' HC3 is written out rather than delegated, since \pkg{sandwich} is not
#' a dependency here:
#' \eqn{V = (X primeX)^-1 \[sum_i x_i x_i prime e_i^2/(1 - h_ii)^2\] (X primeX)^-1},
#' MacKinnon and White (1985), the grf default.
#'
#' @param y Numeric outcome of length n.
#' @param D Numeric treatment assignment of length n.
#' @param tau_hat Out-of-fold CATE predictions, one per unit.  The third
#'   argument of the pasted stub was called X; it is the CATE prediction
#'   vector the best linear predictor is calibrated against, not a
#'   covariate matrix.
#' @param y_hat Optional out-of-fold predictions of E\[Y|X\]; defaults to
#'   \code{mean(y)}.
#' @param w_hat Optional out-of-fold propensity scores; defaults to
#'   \code{mean(D)}.
#' @return list: coef_mean, coef_differential, se_mean, se_differential,
#'   t_mean, t_differential, p_mean, p_differential, n, method.
#' @examples
#' set.seed(1)
#' D <- rep(0:1, 30)
#' tau <- seq(0, 2, length.out = 60)
#' Htebias(tau * D + rnorm(60), D, tau)$coef_differential
#' @export
Htebias <- function(y, D, tau_hat, y_hat = NULL, w_hat = NULL) {
  y <- as.numeric(y)
  d <- as.numeric(D)
  tau <- as.numeric(tau_hat)
  n <- length(y)
  if (length(d) != n || length(tau) != n) {
    stop("y, D and tau_hat must have the same length")
  }
  if (n < 4) stop("need n >= 4")
  yh <- if (is.null(y_hat)) rep(mean(y), n) else as.numeric(y_hat)
  wh <- if (is.null(w_hat)) rep(mean(d), n) else as.numeric(w_hat)

  mean_tau <- mean(tau)
  target <- y - yh
  wres <- d - wh
  X <- cbind(wres * mean_tau, wres * (tau - mean_tau))
  if (sum(X[, 2]^2) <= 0) {
    stop("tau_hat is constant: the differential regressor is identically zero")
  }
  beta <- qr.solve(X, target)
  resid <- as.numeric(target - X %*% beta)
  XtXinv <- solve(crossprod(X))
  H <- X %*% XtXinv %*% t(X)
  w <- resid^2 / (1 - diag(H))^2
  meat <- crossprod(X * sqrt(w))
  V <- XtXinv %*% meat %*% XtXinv
  se <- sqrt(abs(diag(V)))
  tstat <- ifelse(se > 0, beta / se, NaN)
  df <- n - 2
  onesided <- function(t) {
    if (is.nan(t)) {
      return(NaN)
    }
    two <- 2 * stats::pt(abs(t), df, lower.tail = FALSE)
    if (t < 0) 1 - two / 2 else two / 2
  }
  list(
    coef_mean = beta[1], coef_differential = beta[2],
    se_mean = se[1], se_differential = se[2],
    t_mean = tstat[1], t_differential = tstat[2],
    p_mean = onesided(tstat[1]), p_differential = onesided(tstat[2]),
    n = n,
    method = paste(
      "BLP calibration test, HC3",
      "(the grf test_calibration statistic; Chernozhukov et al. 2018)"
    )
  )
}

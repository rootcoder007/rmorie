#' Spatial Durbin (SDM) model diagnostics
#'
#' Front ends for the spatial Durbin model \eqn{y = \rho W y + X\beta +
#' WX_*\theta + e} (the lag model on \eqn{Z = (X, WX_*)}, \eqn{X_*} the
#' non-constant columns); R arm of the Python modules \code{morie.fn.sdm*},
#' \code{gnsres} and \code{sdemres}. \code{Sdmdet}, \code{Sdmjac}:
#' \eqn{\log|I - \rho W|}. \code{Sdmconv}: is \eqn{\rho} inside
#' \eqn{(1/e_{min}, 1/e_{max})}? \code{Sdmlrt}: likelihood ratio against the
#' SAR. \code{Sdmwald}: \eqn{(\rho/se)^2}. \code{Sdmcf}: Wald test of the
#' common-factor restriction \eqn{\theta + \rho\beta = 0} (delta method).
#' \code{Sdmr2}: Nagelkerke's R-squared. \code{Sdmflt}: \eqn{(I - \rho W)y}.
#' \code{Sdmwx}: \eqn{WX_*} with column means and correlations.
#' \code{Sdmolsi}: the OLS baseline (coefficients, ML variance,
#' log-likelihood). \code{Sdmres}, \code{Gnsres}, \code{Sdemres}: Moran's I
#' of residuals, exact test when the design is given. \code{Sdmvar}: inverse
#' analytic information matrix of \eqn{(\beta, \theta, \rho, \sigma^2)}.
#' \code{Sdmboot}: residual-bootstrap percentile interval for \eqn{\rho}.
#'
#' @param W Spatial weights matrix.
#' @param rho Spatial lag parameter.
#' @param ll_sdm,ll_sar Log-likelihoods of the SDM and the SAR.
#' @param df Degrees of freedom.
#' @param se_rho Standard error of \code{rho}.
#' @param coef SDM slopes of the lagged regressors (no intercept).
#' @param theta Coefficients of \eqn{WX}.
#' @param vcov Covariance of \code{c(rho, coef, theta)}.
#' @param ll_model,ll_null Log-likelihoods of the model and the null model.
#' @param n Number of observations.
#' @param y Response (or series to filter).
#' @param X Design matrix including any intercept column (optional for the
#'   residual checks).
#' @param resid Model residuals.
#' @param WX Lagged regressors \eqn{WX_*}.
#' @param sigma2 Error variance.
#' @param beta Coefficients of the columns of X and WX.
#' @param B Bootstrap replicates.
#' @param seed Philox seed.
#' @param level Confidence level.
#' @return A list whose \code{statistic} is the headline value, with the
#'   components of the Python result's \code{extra}.
#' @references Burridge, P. (1981). Testing for a common factor in a spatial
#'   autoregression model. Environment and Planning A 13, 795-800.
#'
#'   Elhorst, J. P. (2014). Spatial Econometrics: From Cross-Sectional Data
#'   to Spatial Panels. Springer, Berlin.
#'
#'   LeSage, J. and Pace, R. K. (2009). Introduction to Spatial
#'   Econometrics. CRC Press, Boca Raton.
#'
#'   Cliff, A. D. and Ord, J. K. (1981). Spatial Processes: Models and
#'   Applications. Pion, London.
#' @examples
#' W <- matrix(c(0, .5, 0, 1, 0, 1, 0, .5, 0), 3)
#' Sdmdet(W, 0.5)$statistic
#' Sdmcf(1, -0.2, diag(c(0.01, 0.04, 0.09)), 0.4)$p_value
#' Sdmolsi(c(1, 2.1, 2.9, 4.2), cbind(1, 0:3))$beta
#' @export
Sdmdet <- function(W, rho) {
  list(statistic = .sxd_logdet(as.matrix(W), rho), rho = rho)
}

#' @rdname Sdmdet
#' @export
Sdmjac <- function(W, rho) {
  Sdmdet(W, rho)
}

#' @rdname Sdmdet
#' @export
Sdmconv <- function(W, rho) {
  .sxd_conv(W, rho)
}

#' @rdname Sdmdet
#' @export
Sdmlrt <- function(ll_sdm, ll_sar, df = 2) {
  .sxd_lrt(ll_sdm, ll_sar, df)
}

#' @rdname Sdmdet
#' @export
Sdmwald <- function(rho, se_rho) {
  .sxd_wald1(rho, se_rho)
}

#' @rdname Sdmdet
#' @export
Sdmcf <- function(coef, theta, vcov, rho) {
  .sxd_cf(coef, theta, vcov, rho)
}

#' @rdname Sdmdet
#' @export
Sdmr2 <- function(ll_model, ll_null, n) {
  .sxd_nagelkerke(ll_model, ll_null, n)
}

#' @rdname Sdmdet
#' @export
Sdmflt <- function(y, W, rho = 0.3) {
  .sxd_filter(y, W, rho)
}

#' @rdname Sdmdet
#' @export
Sdmwx <- function(X, W) {
  X <- as.matrix(X)
  W <- as.matrix(W)
  d <- .sd_durbin(X, W)
  WX <- d$Z[, -seq_len(ncol(X)), drop = FALSE]
  cors <- vapply(seq_along(d$lag), function(j) {
    w <- WX[, j]
    if (sum((w - mean(w))^2) > 0) stats::cor(X[, d$lag[j]], w) else NaN
  }, numeric(1))
  list(statistic = length(d$lag), WX = unname(WX), lagged_columns = d$lag - 1L,
       design = unname(d$Z), means = unname(colMeans(WX)), correlations = cors)
}

#' @rdname Sdmdet
#' @export
Sdmolsi <- function(y, X) {
  y <- as.numeric(y)
  X <- as.matrix(X)
  n <- length(y)
  beta <- as.vector(solve(crossprod(X), crossprod(X, y)))
  e <- y - as.vector(X %*% beta)
  s2 <- sum(e^2) / n
  ll <- -0.5 * n * (log(2 * pi * s2) + 1)
  list(statistic = ll, beta = beta, sigma2 = s2, residuals = e, aic = -2 * ll + 2 * (ncol(X) + 1))
}

#' @rdname Sdmdet
#' @export
Sdmres <- function(resid, W, X = NULL) {
  .sxd_resmoran(resid, W, X)
}

#' @rdname Sdmdet
#' @export
Gnsres <- function(resid, W, X = NULL) {
  .sxd_resmoran(resid, W, X)
}

#' @rdname Sdmdet
#' @export
Sdemres <- function(resid, W, X = NULL) {
  .sxd_resmoran(resid, W, X)
}

#' @rdname Sdmdet
#' @export
Sdmvar <- function(X, WX, W, rho, sigma2, beta) {
  Z <- cbind(as.matrix(X), as.matrix(WX))
  r <- .sxd_cov(Z, W, beta, rho, 0, sigma2, TRUE, FALSE)
  c(list(statistic = r$cov[ncol(Z) + 1, ncol(Z) + 1]), r)
}

#' @rdname Sdmdet
#' @export
Sdmboot <- function(y, X, W, B = 99, seed = 0, level = 0.95) {
  X <- as.matrix(X)
  W <- as.matrix(W)
  Z <- .sd_durbin(X, W)$Z
  r <- .sxd_boot(y, Z, W, "lag", B, seed, level)
  .sxd_ci(r$fit$rho, r$draws[, 1], level)
}

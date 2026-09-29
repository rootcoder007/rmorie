#' SAC (SARAR) model diagnostics
#'
#' Front ends for the SAC model \eqn{y = \rho W y + X\beta + u},
#' \eqn{u = \lambda W u + e}; R arm of the Python modules
#' \code{morie.fn.sac*}. \code{Sacdet}, \code{Sacjac}: \eqn{\log|I - \rho W|
#' + \log|I - \lambda W|}. \code{Sacconv}: are both parameters inside
#' \eqn{(1/e_{min}, 1/e_{max})}? \code{Saclrt}: likelihood ratio against the
#' nested SAR or SEM. \code{Sacwald}: joint Wald test \eqn{b'V^{-1}b}.
#' \code{Sacres}: Moran's I of residuals. \code{Sacimp}: LeSage-Pace
#' impacts (those of the lag part; \eqn{\lambda} does not enter).
#' \code{Sacrob}: GS2SLS with the HC sandwich covariance
#' (\code{spatialreg::gstsls(robust = TRUE)}). \code{Sacvar}: inverse
#' analytic information matrix of \eqn{(\beta, \rho, \lambda, \sigma^2)}.
#' \code{Sacboot}: residual-bootstrap percentile interval for \eqn{\rho}.
#'
#' @param W Spatial weights matrix.
#' @param rho Spatial lag parameter.
#' @param lam Spatial error parameter.
#' @param ll_sac,ll_sar Log-likelihoods of the SAC and the nested model.
#' @param df Degrees of freedom.
#' @param params Estimates tested jointly against zero.
#' @param vcov Their covariance matrix.
#' @param resid Model residuals.
#' @param X Design matrix including any intercept column (optional for
#'   \code{Sacres}).
#' @param coef Slopes without intercept.
#' @param y Response.
#' @param robust \code{"HC0"} or \code{"HC1"}.
#' @param sigma2 Error variance.
#' @param beta Regression coefficients including the intercept.
#' @param B Bootstrap replicates.
#' @param seed Philox seed.
#' @param level Confidence level.
#' @return A list whose \code{statistic} is the headline value, with the
#'   components of the Python result's \code{extra}.
#' @references Anselin, L. (1988). Spatial Econometrics: Methods and Models.
#'   Kluwer, Dordrecht.
#'
#'   Lee, L.-F. (2004). Asymptotic distributions of quasi-maximum likelihood
#'   estimators for spatial autoregressive models. Econometrica 72,
#'   1899-1925.
#'
#'   Kelejian, H. H. and Prucha, I. R. (2010). Specification and estimation
#'   of spatial autoregressive models with autoregressive and
#'   heteroskedastic disturbances. Journal of Econometrics 157, 53-67.
#'
#'   LeSage, J. and Pace, R. K. (2009). Introduction to Spatial
#'   Econometrics. CRC Press, Boca Raton.
#' @examples
#' W <- matrix(c(0, .5, 0, 1, 0, 1, 0, .5, 0), 3)
#' Sacdet(W, 0.5, -0.5)$statistic
#' Sacwald(c(0.3, 0.2), matrix(c(0.01, 0.002, 0.002, 0.02), 2))$p_value
#' @export
Sacdet <- function(W, rho, lam) {
  W <- as.matrix(W)
  a <- .sxd_logdet(W, rho)
  b <- .sxd_logdet(W, lam)
  list(statistic = a + b, logdet_rho = a, logdet_lambda = b)
}

#' @rdname Sacdet
#' @export
Sacjac <- function(W, rho, lam) {
  Sacdet(W, rho, lam)
}

#' @rdname Sacdet
#' @export
Sacconv <- function(W, rho, lam) {
  .sxd_conv(W, c(rho, lam))
}

#' @rdname Sacdet
#' @export
Saclrt <- function(ll_sac, ll_sar, df = 1) {
  .sxd_lrt(ll_sac, ll_sar, df)
}

#' @rdname Sacdet
#' @export
Sacwald <- function(params, vcov) {
  r <- SpatialWaldTest(as.numeric(params), as.matrix(vcov))
  list(statistic = r$statistic, p_value = r$pvalue, df = r$df)
}

#' @rdname Sacdet
#' @export
Sacres <- function(resid, W, X = NULL) {
  .sxd_resmoran(resid, W, X)
}

#' @rdname Sacdet
#' @export
Sacimp <- function(coef, rho, lam, W) {
  r <- .sxd_impacts(coef, rho, W)
  list(statistic = r$total[1], direct = r$direct, indirect = r$indirect, total = r$total, lambda = lam)
}

#' @rdname Sacdet
#' @export
Sacrob <- function(y, X, W, robust = "HC0") {
  if (!robust %in% c("HC0", "HC1")) stop("robust must be 'HC0' or 'HC1'")
  r <- GS2SLSSAC(y, X, W, robust = robust, sig2n_k = robust == "HC1")
  list(statistic = r$rho, coefficients = r$coefficients, se = r$se, cov = r$cov, lambda = r$lambda)
}

#' @rdname Sacdet
#' @export
Sacvar <- function(X, W, rho, lam, sigma2, beta) {
  X <- as.matrix(X)
  r <- .sxd_cov(X, W, beta, rho, lam, sigma2, TRUE, TRUE)
  c(list(statistic = r$cov[ncol(X) + 1, ncol(X) + 1]), r)
}

#' @rdname Sacdet
#' @export
Sacboot <- function(y, X, W, B = 99, seed = 0, level = 0.95) {
  r <- .sxd_boot(y, X, W, "sac", B, seed, level)
  .sxd_ci(r$fit$rho, r$draws[, 1], level)
}

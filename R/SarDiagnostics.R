#' Spatial lag (SAR) model diagnostics
#'
#' Front ends for the spatial lag model \eqn{y = \rho W y + X\beta + e}; R
#' arm of the Python modules \code{morie.fn.sar*}. \code{Sardet} and
#' \code{Sarjac}: the log-Jacobian \eqn{\log|I - \rho W|} by LU.
#' \code{Sarlrt}: likelihood ratio \eqn{2(l_1 - l_0)} against chi-square.
#' \code{Sarwald}: \eqn{(\rho/se)^2} against chi-square(1). \code{Sarr2}:
#' Nagelkerke's rescaled R-squared. \code{Sarfilt}: the filter
#' \eqn{(I - \rho W)y}. \code{Sarres}: Moran's I of residuals, exact test
#' (as \code{spdep::lm.morantest}) when the design is given. \code{Sarsc}:
#' LM (score) test of \eqn{\rho = 0} after OLS with its error-robust form.
#' \code{Sarimp}, \code{Sarspil}: LeSage-Pace average direct, indirect and
#' total impacts and the indirect/direct ratio. \code{Sarsim}: impacts of
#' parameter draws from \eqn{N((\rho, \beta), V)} (Philox normals).
#' \code{Sarvar}: inverse analytic information matrix of
#' \eqn{(\beta, \rho, \sigma^2)}. \code{Sarboot}: residual-bootstrap
#' percentile interval for \eqn{\rho} (Philox resampling).
#'
#' @param W Spatial weights matrix.
#' @param rho Spatial lag parameter.
#' @param ll_sar,ll_ols Log-likelihoods of the SAR and the OLS model.
#' @param df Degrees of freedom.
#' @param se_rho Standard error of \code{rho}.
#' @param ll_model,ll_null Log-likelihoods of the model and the null model.
#' @param n Number of observations.
#' @param y Response (or series to filter).
#' @param resid Model residuals.
#' @param X Design matrix including any intercept column (optional for
#'   \code{Sarres}).
#' @param coef Slopes of the lag model, without intercept.
#' @param nsim Number of simulation draws.
#' @param vcov Covariance of \code{c(rho, coef)}.
#' @param seed Philox seed.
#' @param sigma2 Error variance.
#' @param beta Regression coefficients including the intercept.
#' @param B Bootstrap replicates.
#' @param level Confidence level.
#' @return A list whose \code{statistic} is the headline value, with the
#'   components of the Python result's \code{extra}.
#' @references Ord, K. (1975). Estimation methods for models of spatial
#'   interaction. Journal of the American Statistical Association 70,
#'   120-126.
#'
#'   Anselin, L. (1988). Spatial Econometrics: Methods and Models. Kluwer,
#'   Dordrecht.
#'
#'   Anselin, L., Bera, A. K., Florax, R. and Yoon, M. J. (1996). Simple
#'   diagnostic tests for spatial dependence. Regional Science and Urban
#'   Economics 26, 77-104.
#'
#'   LeSage, J. and Pace, R. K. (2009). Introduction to Spatial
#'   Econometrics. CRC Press, Boca Raton.
#'
#'   Nagelkerke, N. J. D. (1991). A note on a general definition of the
#'   coefficient of determination. Biometrika 78, 691-692.
#'
#'   Efron, B. and Tibshirani, R. J. (1993). An Introduction to the
#'   Bootstrap. Chapman and Hall.
#' @examples
#' W <- matrix(c(0, .5, 0, 1, 0, 1, 0, .5, 0), 3)
#' Sardet(matrix(c(0, 1, 1, 0), 2), 0.5)$statistic
#' Sarimp(2, 0.4, W)$total
#' Sarlrt(-10, -12.5)$p_value
#' @export
Sardet <- function(W, rho) {
  list(statistic = .sxd_logdet(as.matrix(W), rho), rho = rho)
}

#' @rdname Sardet
#' @export
Sarjac <- function(W, rho) {
  Sardet(W, rho)
}

#' @rdname Sardet
#' @export
Sarlrt <- function(ll_sar, ll_ols, df = 1) {
  .sxd_lrt(ll_sar, ll_ols, df)
}

#' @rdname Sardet
#' @export
Sarwald <- function(rho, se_rho) {
  .sxd_wald1(rho, se_rho)
}

#' @rdname Sardet
#' @export
Sarr2 <- function(ll_model, ll_null, n) {
  .sxd_nagelkerke(ll_model, ll_null, n)
}

#' @rdname Sardet
#' @export
Sarfilt <- function(y, W, rho = 0.3) {
  .sxd_filter(y, W, rho)
}

#' @rdname Sardet
#' @export
Sarres <- function(resid, W, X = NULL) {
  .sxd_resmoran(resid, W, X)
}

#' @rdname Sardet
#' @export
Sarsc <- function(y, X, W) {
  r <- LMSpatialTests(y, X, W)
  list(statistic = r$RSlag$statistic, p_value = r$RSlag$p_value, df = 1,
       robust_statistic = r$adjRSlag$statistic, robust_p_value = r$adjRSlag$p_value)
}

#' @rdname Sardet
#' @export
Sarimp <- function(coef, rho, W) {
  r <- .sxd_impacts(coef, rho, W)
  list(statistic = r$total[1], direct = r$direct, indirect = r$indirect, total = r$total)
}

#' @rdname Sardet
#' @export
Sarspil <- function(coef, rho, W) {
  r <- .sxd_impacts(coef, rho, W)
  ratio <- r$indirect / r$direct
  list(statistic = ratio[1], ratio = ratio, share = r$indirect / r$total,
       direct = r$direct, indirect = r$indirect, total = r$total)
}

#' @rdname Sardet
#' @export
Sarsim <- function(coef, rho, W, nsim = 99, vcov = NULL, seed = 0) {
  if (is.null(vcov)) stop("Sarsim needs vcov, the covariance of (rho, beta)")
  b <- as.numeric(coef)
  mu <- c(rho, b)
  k <- length(mu)
  vcov <- as.matrix(vcov)
  if (nrow(vcov) != k) stop("vcov must be (k + 1) x (k + 1) for (rho, beta_1..k)")
  L <- t(chol(vcov))
  W <- as.matrix(W)
  z <- .morie_random_normal(nsim * k, seed = seed)
  dr <- lapply(c("direct", "indirect", "total"), function(i) matrix(0, nsim, length(b)))
  names(dr) <- c("direct", "indirect", "total")
  for (s in seq_len(nsim)) {
    phi <- mu + as.vector(L %*% z[(s - 1) * k + seq_len(k)])
    r <- SpatialImpacts(phi[1], phi[-1], W)
    for (key in names(dr)) dr[[key]][s, ] <- r[[key]]
  }
  mn <- lapply(dr, colMeans)
  sdv <- lapply(dr, function(m) apply(m, 2, stats::sd))
  list(statistic = mn$total[1], mean = mn, sd = sdv, point = SpatialImpacts(rho, b, W), nsim = nsim)
}

#' @rdname Sardet
#' @export
Sarvar <- function(X, W, rho, sigma2, beta) {
  X <- as.matrix(X)
  r <- .sxd_cov(X, W, beta, rho, 0, sigma2, TRUE, FALSE)
  c(list(statistic = r$cov[ncol(X) + 1, ncol(X) + 1]), r)
}

#' @rdname Sardet
#' @export
Sarboot <- function(y, X, W, B = 99, seed = 0, level = 0.95) {
  r <- .sxd_boot(y, X, W, "lag", B, seed, level)
  .sxd_ci(r$fit$rho, r$draws[, 1], level)
}

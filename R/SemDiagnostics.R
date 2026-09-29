#' Spatial error (SEM) model diagnostics
#'
#' Front ends for the spatial error model \eqn{y = X\beta + u},
#' \eqn{u = \lambda W u + e}; R arm of the Python modules
#' \code{morie.fn.sem*}. \code{Semjac}: \eqn{\log|I - \lambda W|} by LU.
#' \code{Semconv}: is \eqn{\lambda} inside \eqn{(1/e_{min}, 1/e_{max})}
#' (real parts of the eigenvalues of W)? \code{Semlrt}: likelihood ratio
#' against OLS. \code{Semwald}: \eqn{(\lambda/se)^2}. \code{Semflt}: the
#' spatial Cochrane-Orcutt filter \eqn{(I - \lambda W)y}. \code{Semres}:
#' Moran's I of the filtered residuals \eqn{(I - \lambda W)e}, exact test
#' when the (filtered) design is given. \code{Semlm}, \code{Semsc}: LM
#' (score) test of \eqn{\lambda = 0} from OLS residuals,
#' \eqn{(n e'We/e'e)^2 / tr(W'W + WW)}. \code{Semspec}: Wald test of the
#' common-factor restriction \eqn{\theta = -\lambda\beta} in the spatial
#' Durbin model. \code{Semvar}: inverse analytic information matrix of
#' \eqn{(\beta, \lambda, \sigma^2)}. \code{Semboot}: residual-bootstrap
#' percentile interval for \eqn{\lambda}.
#'
#' @param W Spatial weights matrix.
#' @param lam Spatial error parameter.
#' @param ll_sem,ll_ols Log-likelihoods of the SEM and the OLS model.
#' @param df Degrees of freedom.
#' @param se_lam Standard error of \code{lam}.
#' @param y Response (or series to filter).
#' @param resid Model residuals.
#' @param X Design matrix including any intercept column (optional for
#'   \code{Semres}).
#' @param beta Spatial Durbin slopes of the lagged regressors.
#' @param theta Coefficients of \eqn{WX}.
#' @param vcov Covariance of \code{c(lam, beta, theta)}.
#' @param sigma2 Error variance.
#' @param B Bootstrap replicates.
#' @param seed Philox seed.
#' @param level Confidence level.
#' @return A list whose \code{statistic} is the headline value, with the
#'   components of the Python result's \code{extra}.
#' @references Anselin, L. (1988). Spatial Econometrics: Methods and Models.
#'   Kluwer, Dordrecht.
#'
#'   Burridge, P. (1980). On the Cliff-Ord test for spatial correlation.
#'   Journal of the Royal Statistical Society B 42, 107-108.
#'
#'   Burridge, P. (1981). Testing for a common factor in a spatial
#'   autoregression model. Environment and Planning A 13, 795-800.
#'
#'   Ord, K. (1975). Estimation methods for models of spatial interaction.
#'   Journal of the American Statistical Association 70, 120-126.
#' @examples
#' W <- matrix(c(0, .5, 0, 1, 0, 1, 0, .5, 0), 3)
#' Semjac(W, 0.5)$statistic
#' Semconv(W, 0.5)$upper
#' Semlm(c(0.3, -0.2, 0.5), W)$statistic
#' @export
Semjac <- function(W, lam) {
  list(statistic = .sxd_logdet(as.matrix(W), lam), lam = lam)
}

#' @rdname Semjac
#' @export
Semconv <- function(W, lam) {
  .sxd_conv(W, lam)
}

#' @rdname Semjac
#' @export
Semlrt <- function(ll_sem, ll_ols, df = 1) {
  .sxd_lrt(ll_sem, ll_ols, df)
}

#' @rdname Semjac
#' @export
Semwald <- function(lam, se_lam) {
  .sxd_wald1(lam, se_lam)
}

#' @rdname Semjac
#' @export
Semflt <- function(y, W, lam = 0.3) {
  .sxd_filter(y, W, lam)
}

#' @rdname Semjac
#' @export
Semres <- function(resid, W, lam = 0.3, X = NULL) {
  W <- as.matrix(W)
  e <- as.numeric(resid)
  e <- e - lam * as.vector(W %*% e)
  if (!is.null(X)) {
    X <- as.matrix(X)
    X <- X - lam * (W %*% X)
  }
  .sxd_resmoran(e, W, X)
}

#' @rdname Semjac
#' @export
Semlm <- function(resid, W) {
  .sxd_lmerr(resid, W)
}

#' @rdname Semjac
#' @export
Semsc <- function(resid, W) {
  .sxd_lmerr(resid, W)
}

#' @rdname Semjac
#' @export
Semspec <- function(beta, theta, vcov, lam) {
  .sxd_cf(beta, theta, vcov, lam)
}

#' @rdname Semjac
#' @export
Semvar <- function(X, W, lam, sigma2) {
  X <- as.matrix(X)
  r <- .sxd_cov(X, W, NULL, 0, lam, sigma2, FALSE, TRUE)
  c(list(statistic = r$cov[ncol(X) + 1, ncol(X) + 1]), r)
}

#' @rdname Semjac
#' @export
Semboot <- function(y, X, W, B = 99, seed = 0, level = 0.95) {
  r <- .sxd_boot(y, X, W, "error", B, seed, level)
  .sxd_ci(r$fit$lam, r$draws[, 2], level)
}

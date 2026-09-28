.sd_durbin <- function(X, W) {
  lag <- which(apply(X, 2, function(v) length(unique(v)) > 1))
  list(Z = cbind(X, W %*% X[, lag, drop = FALSE]), lag = unname(lag))
}

#' Spatial Durbin error, general nesting and CAR regression with tests
#'
#' \code{SdemML}: spatial Durbin error model \eqn{y = X\beta + WX_*\theta + u},
#' \eqn{u = \lambda Wu + e}, fitted as the error model on the columns of \eqn{X} and \eqn{WX_*}
#' (\code{spatialreg::errorsarlm(Durbin = TRUE)}), with covariance
#' \eqn{\sigma^2(Z_*^\top Z_*)^{-1}} and impacts (direct \eqn{\beta}, indirect
#' \eqn{\theta}, total) with standard errors. \code{GnsML}: general nesting
#' model, the SAC model on the columns of \eqn{X} and \eqn{WX_*} (\code{sacsarlm(Durbin = TRUE)}),
#' with \code{\link{SpatialImpacts}}. \code{CarML}: CAR regression
#' (\code{spautolm(family = "CAR")}) by profile likelihood over
#' \eqn{(1/e_{min}, 1/e_{max})}, with the LR test of \eqn{\lambda = 0}.
#' \code{SpatialLrTest} and \code{SpatialWaldTest}: chi-square tests.
#' \code{ResidualMoran}: Moran's I of OLS residuals with exact moments
#' (\code{spdep::lm.morantest}). \code{LogJacobian}: \eqn{\log|I - \rho W| +
#' \log|I - \lambda W|}. Identical to the Python arm \code{morie.fn.spdurbin}.
#'
#' @param y Response.
#' @param X Design matrix including any intercept column.
#' @param W Spatial weights matrix (symmetric for \code{CarML}).
#' @param interval Search interval for the spatial parameter(s).
#' @param ll_full,ll_restricted Log-likelihoods.
#' @param df Degrees of freedom.
#' @param estimate Estimates tested against zero.
#' @param vcov Their covariance matrix.
#' @param rho,lam Autoregressive parameters.
#' @return List.
#' @references LeSage, J. and Pace, R. K. (2009). Introduction to Spatial
#'   Econometrics. CRC Press.
#'
#'   Elhorst, J. P. (2014). Spatial Econometrics: From Cross-Sectional Data to
#'   Spatial Panels. Springer.
#'
#'   Besag, J. (1974). Spatial interaction and the statistical analysis of
#'   lattice systems. Journal of the Royal Statistical Society B 36, 192-236.
#'
#'   Cliff, A. D. and Ord, J. K. (1981). Spatial Processes: Models and
#'   Applications. Pion.
#' @examples
#' SpatialLrTest(-10, -12.5, 1)$pvalue
#' LogJacobian(matrix(c(0, 1, 1, 0), 2), 0.5)
#' @export
SdemML <- function(y, X, W, interval = c(-0.999, 0.999)) {
  X <- as.matrix(X)
  W <- as.matrix(W)
  D <- .sd_durbin(X, W)
  r <- SpatialRegressionML(y, D$Z, W, "error", interval)
  Zs <- D$Z - r$lambda * (W %*% D$Z)
  V <- r$sigma2 * solve(crossprod(Zs))
  p <- ncol(X)
  k <- length(D$lag)
  ti <- p + seq_len(k)
  b <- r$beta
  list(coefficients = b, se = r$se, cov = unname(V), lambda = r$lambda, sigma2 = r$sigma2, loglik = r$loglik,
       aic = r$aic, bic = r$bic, lagged = D$lag,
       impacts = list(direct = b[D$lag], indirect = b[ti], total = b[D$lag] + b[ti],
                      se_direct = sqrt(diag(V)[D$lag]), se_indirect = sqrt(diag(V)[ti]),
                      se_total = sqrt(diag(V)[D$lag] + diag(V)[ti] + 2 * V[cbind(D$lag, ti)])))
}

#' @rdname SdemML
#' @export
GnsML <- function(y, X, W, interval = c(-0.999, 0.999)) {
  X <- as.matrix(X)
  W <- as.matrix(W)
  D <- .sd_durbin(X, W)
  r <- SpatialRegressionML(y, D$Z, W, "sac", interval)
  p <- ncol(X)
  b <- r$beta
  imp <- SpatialImpacts(r$rho, b[D$lag], W, b[p + seq_along(D$lag)])
  list(coefficients = b, se = r$se, rho = r$rho, lambda = r$lambda, sigma2 = r$sigma2, loglik = r$loglik,
       aic = r$aic, bic = r$bic, lagged = D$lag, impacts = imp)
}

#' @rdname SdemML
#' @export
CarML <- function(y, X, W, interval = NULL) {
  y <- as.numeric(y)
  X <- as.matrix(X)
  W <- as.matrix(W)
  n <- length(y)
  p <- ncol(X)
  if (is.null(interval)) {
    ev <- eigen(W, symmetric = TRUE, only.values = TRUE)$values
    interval <- c(1 / min(ev), 1 / max(ev))
  }
  prof <- function(lam) {
    AX <- X - lam * (W %*% X)
    Ay <- y - lam * as.vector(W %*% y)
    XtAX <- crossprod(X, AX)
    b <- as.vector(solve(XtAX, crossprod(X, Ay)))
    res <- y - as.vector(X %*% b)
    list(b = b, s2 = sum(res * (res - lam * as.vector(W %*% res))) / n, XtAX = XtAX)
  }
  nll <- function(lam) 0.5 * n * log(2 * pi * prof(lam)$s2) + 0.5 * n - 0.5 * .sr_logdet(W, lam)
  o <- stats::optimize(nll, interval, tol = 1e-12)
  pr <- prof(o$minimum)
  lr <- 2 * (-o$objective + nll(0))
  list(coefficients = pr$b, se = sqrt(diag(pr$s2 * solve(pr$XtAX))), lambda = o$minimum, sigma2 = pr$s2,
       loglik = -o$objective, aic = 2 * o$objective + 2 * (p + 2),
       lr_test = list(statistic = lr, df = 1, pvalue = 1 - stats::pchisq(lr, 1)))
}

#' @rdname SdemML
#' @export
SpatialLrTest <- function(ll_full, ll_restricted, df) {
  lr <- 2 * (ll_full - ll_restricted)
  list(statistic = lr, df = df, pvalue = 1 - stats::pchisq(lr, df))
}

#' @rdname SdemML
#' @export
SpatialWaldTest <- function(estimate, vcov) {
  b <- as.numeric(estimate)
  w <- sum(b * solve(as.matrix(vcov), b))
  list(statistic = w, df = length(b), pvalue = 1 - stats::pchisq(w, length(b)))
}

#' @rdname SdemML
#' @export
ResidualMoran <- function(y, X, W) {
  y <- as.numeric(y)
  X <- as.matrix(X)
  W <- as.matrix(W)
  n <- length(y)
  k <- ncol(X)
  M <- diag(n) - X %*% solve(crossprod(X), t(X))
  e <- as.vector(M %*% y)
  S0 <- sum(W)
  I <- n / S0 * sum(e * (W %*% e)) / sum(e^2)
  MW <- M %*% W
  trMW <- sum(diag(MW))
  E <- n / S0 * trMW / (n - k)
  V <- (n / S0)^2 * (sum(diag(MW %*% (M %*% t(W)))) + sum(diag(MW %*% MW)) + trMW^2) / ((n - k) * (n - k + 2)) - E^2
  z <- (I - E) / sqrt(V)
  list(I = I, expected = E, variance = V, z = z, pvalue = 1 - stats::pnorm(z))
}

#' @rdname SdemML
#' @export
LogJacobian <- function(W, rho, lam = 0) {
  W <- as.matrix(W)
  (if (rho != 0) .sr_logdet(W, rho) else 0) + (if (lam != 0) .sr_logdet(W, lam) else 0)
}

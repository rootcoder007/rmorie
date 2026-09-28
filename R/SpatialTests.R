#' Spatial Durbin ML with spillover indices, and Breusch-Pagan tests
#'
#' \code{SdmML}: \eqn{y = \rho W y + X\beta + W X\theta + e} by maximum
#' likelihood (\code{SpatialRegressionML} lag model on \eqn{[X, WX]},
#' constant columns not lagged), with average direct, indirect and total
#' impacts and the spillover index indirect/total, as
#' \code{spatialreg::lagsarlm(Durbin = TRUE)} and \code{impacts}.
#' \code{BreuschPagan}: original (half the explained sum of squares of
#' \eqn{e^2/\sigma^2 - 1} on \eqn{Z}) or Koenker's studentized
#' (\eqn{n R^2}) test, as \code{lmtest::bptest}. \code{SpatialBPTest}: the
#' test on lag-model residuals (\eqn{Z = X}) or error-model residuals
#' (\eqn{Z = X - \lambda W X}), as \code{spatialreg::bptest.Sarlm}. Identical
#' to the Python arm \code{morie.fn.sptests}.
#'
#' @param y Response.
#' @param X Design matrix including the intercept column.
#' @param W Spatial weights matrix.
#' @param interval Search interval for \eqn{\rho}.
#' @param residuals Residuals.
#' @param Z Variance regressors including the intercept.
#' @param studentize Koenker's studentized version.
#' @param model \code{"lag"} or \code{"error"}.
#' @return List.
#' @references LeSage, J. and Pace, R. K. (2009). Introduction to Spatial
#'   Econometrics. CRC Press.
#'
#'   Breusch, T. S. and Pagan, A. R. (1979). A simple test for
#'   heteroscedasticity and random coefficient variation. Econometrica 47,
#'   1287-1294.
#'
#'   Koenker, R. (1981). A note on studentizing a test for
#'   heteroscedasticity. Journal of Econometrics 17, 107-112.
#' @examples
#' BreuschPagan(c(1, -1, 2, -2, 3, -3), cbind(1, c(0, 0, 1, 1, 0, 0), c(0, 0, 0, 0, 1, 1)))$statistic
#' @export
SdmML <- function(y, X, W, interval = c(-0.999, 0.999)) {
  X <- as.matrix(X)
  W <- as.matrix(W)
  lagged <- which(apply(X, 2, function(v) any(v != v[1])))
  Z <- cbind(X, W %*% X[, lagged, drop = FALSE])
  fit <- SpatialRegressionML(y, Z, W, "lag", interval = interval)
  p <- ncol(X)
  beta <- fit$beta[seq_len(p)]
  theta <- fit$beta[-seq_len(p)]
  imp <- SpatialImpacts(fit$rho, beta[lagged], W, theta = theta)
  list(beta = beta, theta = theta, rho = fit$rho, sigma2 = fit$sigma2, loglik = fit$loglik, aic = fit$aic,
       se = fit$se, direct = imp$direct, indirect = imp$indirect, total = imp$total,
       spillover_index = ifelse(imp$total != 0, imp$indirect / imp$total, NaN), lagged = lagged)
}

#' @rdname SdmML
#' @export
BreuschPagan <- function(residuals, Z, studentize = TRUE) {
  e <- as.numeric(residuals)
  Z <- as.matrix(Z)
  n <- nrow(Z)
  s2 <- sum(e^2) / n
  w <- if (studentize) e^2 - s2 else e^2 / s2 - 1
  fv <- as.vector(Z %*% solve(crossprod(Z), crossprod(Z, w)))
  bp <- if (studentize) n * sum(fv^2) / sum(w^2) else 0.5 * sum(fv^2)
  list(statistic = bp, df = ncol(Z) - 1, p_value = stats::pchisq(bp, ncol(Z) - 1, lower.tail = FALSE),
       studentize = studentize)
}

#' @rdname SdmML
#' @export
SpatialBPTest <- function(y, X, W, model = "lag", studentize = TRUE) {
  if (!model %in% c("lag", "error")) stop("model must be lag or error", call. = FALSE)
  y <- as.numeric(y)
  X <- as.matrix(X)
  W <- as.matrix(W)
  fit <- SpatialRegressionML(y, X, W, model)
  if (model == "lag") {
    e <- as.vector(y - fit$rho * W %*% y - X %*% fit$beta)
    Z <- X
  } else {
    Z <- X - fit$lambda * W %*% X
    e <- as.vector(y - fit$lambda * W %*% y - Z %*% fit$beta)
  }
  out <- BreuschPagan(e, Z, studentize)
  out$residuals <- e
  out$model <- model
  out
}

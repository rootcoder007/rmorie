#' Spatial two-stage least squares for the spatial lag model
#'
#' \eqn{Wy} is endogenous in \eqn{y = \rho W y + X\beta + e}; it is
#' instrumented by X and the spatial lags \eqn{WX} (and \eqn{W^2X} when
#' \code{w2x}) of the non-constant columns, plus \eqn{W1}, \eqn{W^21} when
#' W is not row-standardised (Kelejian and Prucha 1998); a constant lag
#' duplicates the intercept and is dropped. \eqn{\delta = (Z_p'Z_p)^{-1}
#' Z_p'y} with \eqn{Z_p} the columns \eqn{\hat{Wy}} and X, residuals from Z (columns Wy and X),
#' \eqn{Var = s^2 (Z_p'Z_p)^{-1}}, \eqn{s^2 = e'e/(n-k)}, or the HC0 / HC1
#' sandwich. This is \code{spatialreg::stsls}.
#'
#' @param y Response.
#' @param X Design matrix.
#' @param W Spatial weights.
#' @param w2x Also use \eqn{W^2 X}.
#' @param robust \code{NULL}, \code{"HC0"} or \code{"HC1"}.
#' @param sig2n_k Divide by \eqn{n - k}.
#' @return List with \code{coefficients} (rho first), \code{se},
#'   \code{cov}, \code{sigma2}, \code{residuals}, \code{instruments}.
#' @references Kelejian, H. H. and Prucha, I. R. (1998). A generalized
#'   spatial two-stage least squares procedure for estimating a spatial
#'   autoregressive model with autoregressive disturbances. Journal of Real
#'   Estate Finance and Economics 17, 99-121.
#' @examples
#' W <- matrix(0, 5, 5); W[cbind(1:5, c(2:5, 1))] <- .5; W[cbind(1:5, c(5, 1:4))] <- .5
#' SpatialTwoStageLS(c(1, 2.2, 1.4, 3.1, .9), cbind(1, c(.1, .6, .2, .9, .3)), W)$coefficients
#' @export
SpatialTwoStageLS <- function(y, X, W, w2x = TRUE, robust = NULL, sig2n_k = TRUE) {
  y <- as.numeric(y)
  X <- as.matrix(X)
  W <- as.matrix(W)
  n <- length(y)
  p <- ncol(X)
  const <- which(apply(X, 2, function(v) all(v == v[1])))
  rowstd <- all(abs(rowSums(W) - 1) < 1e-12)
  inst <- NULL
  for (k in seq_len(p)) {
    if (k %in% const && rowstd) next
    wc <- as.vector(W %*% X[, k])
    cand <- if (w2x) list(wc, as.vector(W %*% wc)) else list(wc)
    for (cc in cand) {
      if (!(length(const) && max(cc) - min(cc) <= 1e-12 * max(1, abs(cc[1])))) inst <- cbind(inst, cc)
    }
  }
  Q <- cbind(X, inst)
  Wy <- as.vector(W %*% y)
  yhat <- as.vector(Q %*% solve(crossprod(Q), crossprod(Q, Wy)))
  Zp <- cbind(yhat, X)
  Z <- cbind(Wy, X)
  ZZi <- solve(crossprod(Zp))
  delta <- as.vector(ZZi %*% crossprod(Zp, y))
  e <- y - as.vector(Z %*% delta)
  sse <- sum(e^2)
  df <- if (sig2n_k) n - (p + 1) else n
  cov <- if (is.null(robust)) ZZi * sse / df else {
    om <- e^2 * (if (robust == "HC1") n / df else 1)
    ZZi %*% crossprod(Zp, Zp * om) %*% ZZi
  }
  list(coefficients = delta, se = sqrt(diag(cov)), cov = cov, sigma2 = sse / df, residuals = e,
       instruments = if (is.null(inst)) 0L else ncol(inst))
}

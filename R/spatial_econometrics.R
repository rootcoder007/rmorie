# Spatial autocorrelation inference and spatial-econometric estimators.
#
# R mirror of the spatial block in morie/src/morie/fn/_robust_core.py.
# Both arms were verified against the R reference packages themselves --
# spdep::Szero, spdep::moran.test, spatialreg::stsls and
# spatialreg::GMerrorsar -- so morie can perform the analyses the
# Bivand, Pebesma & Gomez-Rubio examples demonstrate without depending
# on those packages at run time.

#' Spatial weights constants
#'
#' The S0, S1 and S2 totals of a spatial weights matrix:
#' `S0 = sum_ij w_ij` (spdep's `Szero`),
#' `S1 = 0.5 sum_ij (w_ij + w_ji)^2` and
#' `S2 = sum_i (sum_j w_ij + sum_j w_ji)^2`.  Every asymptotic test of
#' spatial autocorrelation is a function of these three.
#' @param W numeric spatial weights matrix
#' @return list with `S0`, `S1`, `S2` and `n`
#' @export
#' @examples
#' morie_weights_totals(W = 5L)
morie_weights_totals <- function(W) {
  W <- as.matrix(W)
  if (nrow(W) != ncol(W)) stop("W must be square")
  n <- nrow(W)
  list(S0 = sum(W),
       S1 = 0.5 * sum((W + t(W))^2),
       S2 = sum((rowSums(W) + colSums(W))^2),
       n = n)
}

#' Moran's I and its asymptotic test
#'
#' `morie_morans_i` is the statistic
#' `I = (n/S0) * (z' W z) / (z'z)` with `z = x - mean(x)`.
#' `morie_morans_i_test` mirrors `spdep::moran.test`: under the null
#' `E\[I\] = -1/(n-1)` and the variance takes the randomisation form
#' (conditioning on the observed values, treating only their
#' arrangement as random) or the normality form.
#' @param x numeric vector of observations
#' @param W numeric spatial weights matrix
#' @return `morie_morans_i` a number; `morie_morans_i_test` a list with
#'   `estimate`, `expectation`, `variance`, `statistic` and `p_value`
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' morie_morans_i(V, V)
morie_morans_i <- function(x, W) {
  W <- as.matrix(W)
  n <- length(x)
  if (nrow(W) != n) stop("W must be n x n to match x")
  z <- x - mean(x)
  (n / sum(W)) * as.numeric(t(z) %*% W %*% z) / sum(z^2)
}

#' @rdname morie_morans_i
#' @export
morie_morans_i_test <- function(x, W, randomisation = TRUE,
                                alternative = "greater") {
  W <- as.matrix(W)
  n <- length(x)
  if (nrow(W) != n) stop("W must be n x n to match x")
  tt <- morie_weights_totals(W)
  S0 <- tt$S0; S1 <- tt$S1; S2 <- tt$S2
  I <- morie_morans_i(x, W)
  ei <- -1 / (n - 1)
  if (randomisation) {
    z <- x - mean(x)
    s2z <- sum(z^2)
    b2 <- n * sum(z^4) / (s2z^2)
    num <- n * ((n^2 - 3 * n + 3) * S1 - n * S2 + 3 * S0^2) -
      b2 * ((n^2 - n) * S1 - 2 * n * S2 + 6 * S0^2)
    v <- num / ((n - 1) * (n - 2) * (n - 3) * S0^2) - 1 / (n - 1)^2
  } else {
    v <- (n^2 * S1 - n * S2 + 3 * S0^2) / (S0^2 * (n^2 - 1)) -
      1 / (n - 1)^2
  }
  zval <- (I - ei) / sqrt(v)
  p <- switch(alternative,
              greater = stats::pnorm(zval, lower.tail = FALSE),
              less = stats::pnorm(zval),
              2 * stats::pnorm(abs(zval), lower.tail = FALSE))
  list(estimate = I, expectation = ei, variance = v,
       statistic = zval, p_value = p, S0 = S0, S1 = S1, S2 = S2,
       randomisation = randomisation, n = n)
}

#' Spatial two-stage least squares
#'
#' Fits the spatial lag model `y = rho W y + X beta + e`.  `W y` is
#' endogenous, so it is instrumented with `\[X, WX, W^2 X\]` -- the
#' Kelejian and Prucha (1998) instrument set that `spatialreg::stsls`
#' uses.  Ordinary least squares on this model is inconsistent.
#' @param y numeric response
#' @param X predictor matrix
#' @param W spatial weights matrix
#' @param add_intercept prepend an intercept column
#' @return list with `rho`, `beta`, `residuals` and `sigma2`
#' @export
#' @examples
#' set.seed(1)
#' n <- 20
#' W <- matrix(0, n, n)
#' for (i in 1:(n - 1)) { W[i, i + 1] <- 1; W[i + 1, i] <- 1 }
#' W <- W / pmax(rowSums(W), 1)
#' X <- matrix(rnorm(n), n, 1)
#' y <- as.numeric(solve(diag(n) - 0.3 * W) %*% (2 * X[, 1] + rnorm(n)))
#' morie_spatial_2sls(y, X, W)
morie_spatial_2sls <- function(y, X, W, add_intercept = TRUE) {
  X <- as.matrix(X); W <- as.matrix(W)
  n <- length(y)
  if (nrow(X) != n) stop("X has ", nrow(X), " rows but y has ", n)
  if (nrow(W) != n || ncol(W) != n)
    stop("W must be ", n, " x ", n, " to match y")
  if (add_intercept) X <- cbind(1, X)
  p <- ncol(X)
  Wy <- as.numeric(W %*% y)
  inst <- X
  for (j in seq_len(p)) {
    if (add_intercept && j == 1L) next
    wx <- as.numeric(W %*% X[, j])
    inst <- cbind(inst, wx, as.numeric(W %*% wx))
  }
  g <- solve(crossprod(inst), crossprod(inst, Wy))
  Wy_hat <- as.numeric(inst %*% g)
  D <- cbind(Wy_hat, X)
  coefs <- as.numeric(solve(crossprod(D), crossprod(D, y)))
  rho <- coefs[1]
  beta <- coefs[-1]
  resid <- as.numeric(y - rho * Wy - X %*% beta)
  list(rho = rho, beta = beta, coefficients = coefs,
       residuals = resid, sigma2 = sum(resid^2) / n, n = n)
}

#' Generalised-moments estimator for the spatial error model
#'
#' Fits `y = X beta + u` with `u = lambda W u + e` by the Kelejian and
#' Prucha (1999) moment conditions, solved as nonlinear least squares.
#' Unlike maximum likelihood this never needs `det(I - lambda W)`.
#' Mirrors `spatialreg::GMerrorsar`.
#' @inheritParams morie_spatial_2sls
#' @return list with `lambda`, `beta`, `residuals` and `sigma2`
#' @export
#' @examples
#' set.seed(1)
#' n <- 20
#' W <- matrix(0, n, n)
#' for (i in 1:(n - 1)) { W[i, i + 1] <- 1; W[i + 1, i] <- 1 }
#' W <- W / pmax(rowSums(W), 1)
#' X <- matrix(rnorm(n), n, 1)
#' u <- as.numeric(solve(diag(n) - 0.4 * W) %*% rnorm(n))
#' y <- as.numeric(2 * X[, 1] + u)
#' morie_gm_error_sar(y, X, W)
morie_gm_error_sar <- function(y, X, W, add_intercept = TRUE) {
  X <- as.matrix(X); W <- as.matrix(W)
  n <- length(y)
  if (nrow(X) != n) stop("X has ", nrow(X), " rows but y has ", n)
  if (nrow(W) != n || ncol(W) != n)
    stop("W must be ", n, " x ", n, " to match y")
  if (add_intercept) X <- cbind(1, X)
  beta <- as.numeric(solve(crossprod(X), crossprod(X, y)))
  u <- as.numeric(y - X %*% beta)
  wu <- as.numeric(W %*% u)
  wwu <- as.numeric(W %*% wu)
  trwpw <- sum(W^2)
  bigG <- matrix(0, 3, 3)
  bigG[, 1] <- c(2 * sum(u * wu), 2 * sum(wwu * wu),
                 sum(u * wwu) + sum(wu * wu)) / n
  bigG[, 2] <- -c(sum(wu * wu), sum(wwu * wwu), sum(wwu * wu)) / n
  bigG[, 3] <- c(1, trwpw / n, 0)
  litg <- c(sum(u * u), sum(wu * wu), sum(u * wu)) / n
  crit <- function(lam, sig)
    sum((bigG %*% c(lam, lam^2, sig) - litg)^2)

  lam <- sum(u * wu) / sqrt(sum(u^2) * sum(wu^2))
  sig <- stats::var(u)
  for (it in 1:400) {
    a <- sum(bigG[, 3]^2)
    b <- sum(bigG[, 3] * (litg - bigG[, 1] * lam - bigG[, 2] * lam^2))
    sig_new <- if (a > 0) b / a else sig
    o <- stats::optimize(function(l) crit(l, sig_new),
                         c(-0.999, 0.999), tol = .Machine$double.eps^0.5)
    lam_new <- o$minimum
    if (abs(lam_new - lam) < 1e-14 && abs(sig_new - sig) < 1e-14) {
      lam <- lam_new; sig <- sig_new; break
    }
    lam <- lam_new; sig <- sig_new
  }
  Xf <- X - lam * (W %*% X)
  yf <- as.numeric(y - lam * (W %*% y))
  beta <- as.numeric(solve(crossprod(Xf), crossprod(Xf, yf)))
  resid <- as.numeric(y - X %*% beta)
  list(lambda = lam, beta = beta, residuals = resid, sigma2 = sig,
       criterion = crit(lam, sig), n = n)
}

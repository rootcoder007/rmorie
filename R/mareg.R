# SPDX-License-Identifier: AGPL-3.0-or-later
# Explain between-study heterogeneity with study-level covariates
#
# A random-effects pool reports heterogeneity; a meta-regression asks
# where it comes from. The residual \eqn{au^2} is the part the
# prime moderators fail to explain, and it belongs in the weights, otherwise
# prime the standard errors are those of a fixed-effect fit and are too small.
# prime The moderators are study-level, so this is ecological regression: a
# prime covariate that predicts the effect across studies says nothing about
# prime the same covariate within a study.
# prime
# prime Formula: \code{y_i = x_i prime beta + u_i + e_i}, \code{Var(u) = tau^2},
# prime \code{Var(e_i) = v_i}, weights \code{1/(v_i + tau^2)}. \code{tau^2} is
# prime the moment estimator \code{max(0, (Q_E - (n - p)) / (tr W -
# prime tr((X primeWX)^{-1} X'W^2 X)))} -- van Houwelingen, Arends and Stijnen
#' (2002) Section 4.
#'
#' @param yi Study effect estimates.
#' @param vi Their sampling variances, strictly positive.
#' @param X Moderator matrix; supply the intercept column yourself.
#' @return List with \code{beta}, \code{se}, \code{tau2}, \code{R2},
#'   \code{ll}, \code{QE}, \code{QM}, \code{n}, \code{p}.
#' @references van Houwelingen, H. C., Arends, L. R. and Stijnen, T.
#'   (2002). Statistics in Medicine 21(4):589-624. \doi{10.1002/sim.1040}.
#' @export
#' @examples
#' Mareg(yi = c(1, 2, 3, 4, 5, 6, 7, 8), vi = c(1, 2, 3, 4, 5, 6, 7, 8), X = c(1, 2, 3, 4, 5, 6, 7, 8))
Mareg <- function(yi, vi, X) {
  y <- as.numeric(yi); v <- as.numeric(vi); Xm <- as.matrix(X)
  n <- length(y)
  if (n == 0L) stop("no studies")
  if (length(v) != n || nrow(Xm) != n)
    stop("yi, vi and X must have the same number of rows")
  if (any(v <= 0)) stop("sampling variances must be strictly positive")
  p <- ncol(Xm)
  if (p > n) stop("more moderators than studies")
  w0 <- 1 / v
  f0 <- .ma_wls(Xm, y, w0)
  resid <- y - as.numeric(Xm %*% f0$beta)
  QE <- sum(w0 * resid^2)
  XtW2X <- crossprod(Xm * w0^2, Xm)
  trterm <- sum(diag(f0$cov %*% XtW2X))
  denom <- sum(w0) - trterm
  tau2 <- 0
  if (denom > 0) tau2 <- max(0, (QE - (n - p)) / denom)
  w <- 1 / (v + tau2)
  f1 <- .ma_wls(Xm, y, w)
  se <- sqrt(pmax(diag(f1$cov), 0))
  se[diag(f1$cov) <= 0] <- NA_real_
  fit <- as.numeric(Xm %*% f1$beta)
  ll <- -0.5 * sum(log(2 * pi * (v + tau2)) + (y - fit)^2 / (v + tau2))
  one <- matrix(1, n, 1)
  fn <- .ma_wls(one, y, w0)
  r1 <- y - fn$beta[1]
  Q1 <- sum(w0 * r1^2)
  d1 <- sum(w0) - sum(w0^2) / sum(w0)
  tau2_null <- 0
  if (d1 > 0) tau2_null <- max(0, (Q1 - (n - 1)) / d1)
  R2 <- if (tau2_null > 0) max(0, 1 - tau2 / tau2_null) else 0
  .t1_result(beta = f1$beta, se = se, tau2 = tau2, R2 = R2, ll = ll,
             QE = QE, QM = Q1 - QE, tau2_null = tau2_null, n = n, p = p,
             method = "Random-effects meta-regression")
}

# SPDX-License-Identifier: AGPL-3.0-or-later

#' Partially linear model of a conditional quantile
#'
#' Horowitz (2009), Semiparametric and Nonparametric Methods in
#' Econometrics, Section 3.6.3, pages 90-91.  The model is
#' Q_tau(Y | X, Z) = X'beta + g(Z), equivalently Y = X'beta + g(Z) + U
#' with P(U <= 0 | X = x, Z = z) = tau.
#'
#' A "Robinson approach" is NOT available here, and page 90 says so in
#' terms: Robinson's differencing works for the conditional-mean model
#' (3.2a) "because the mean of a sum of random variables is the sum of
#' the individual means.  The quantile of the sum of random variables
#' is not the sum of the individual quantiles.  Consequently,
#' differencing cannot be used to eliminate g from (3.2b)".  The
#' estimator implemented here is the one the section actually gives,
#' due to Chen, Linton and Van Keilegom (2003), based on
#'
#'   E X {tau - I\[Y - X'beta - g(Z) <= 0\]} = 0                  (3.38)
#'
#' with g replaced by a nonparametric estimator RE-COMPUTED at every
#' candidate b:
#'
#'   bhat = argmin_b || n^-1 sum_i X_i
#'                      {tau - I\[Y_i - X_i'b - ghat(Z_i, b) <= 0\]} ||^2
#'
#' where ghat(z, b) is the kernel-weighted tau-quantile of Y - X'b
#' given Z = z.  Re-estimating g inside the loop is the point: with g
#' held fixed at a first-pass value the criterion is not the one (3.38)
#' identifies.
#'
#' The criterion is a STEP function of b, so a gradient-based
#' optimiser is inapplicable.  A fixed-schedule coordinate search is
#' used, with no tolerance-based exit and no random restart, so both
#' language arms take the same path.  For the same reason the linear
#' predictor is accumulated in an explicit fixed order rather than by a
#' matrix product: at a tie the indicator is decided by the last bit of
#' the residual, and the two arms must round identically.
#'
#' @param x Numeric vector or n by p matrix of covariates entering
#'   linearly.
#' @param y Numeric vector, the response.
#' @param z Numeric vector, the covariate entering through g.
#' @param bandwidth Numeric or NULL; bandwidth for g, default
#'   Silverman's rule on z.
#' @param tau Numeric quantile level in (0, 1).
#' @param niter Integer, coordinate-search sweeps.
#' @return Named list with beta_tau, g_tau_hat, criterion, tau,
#'   bandwidth, n, p, method.
#' @keywords internal
#' @examples
#' n <- 40
#' z <- sin(0.9 * seq_len(n))
#' x <- cbind(cos(1.3 * seq_len(n)), sin(2.1 * seq_len(n)))
#' y <- 1.5 * x[, 1] - 0.8 * x[, 2] + z^2
#' Hrzplrq(x, y, z, niter = 4L)$beta_tau
#' @export
Hrzplrq <- function(x, y, z, bandwidth = NULL, tau = 0.5, niter = 12L) {
  y <- as.numeric(y)
  z <- as.numeric(z)
  n <- length(y)
  if (n < 4L) stop(sprintf("need at least 4 observations, got %d.", n))
  if (length(z) != n) {
    stop(sprintf("y has %d points but z has %d.", n, length(z)))
  }
  X <- if (is.null(dim(x))) matrix(as.numeric(x), ncol = 1L) else as.matrix(x)
  if (nrow(X) != n) {
    stop(sprintf("x must have %d rows, got %d.", n, nrow(X)))
  }
  p <- ncol(X)
  tau <- as.numeric(tau)
  if (!(tau > 0 && tau < 1)) {
    stop(sprintf("tau must lie strictly in (0, 1), got %g.", tau))
  }
  hz <- if (is.null(bandwidth)) .hrz_silverman(z) else as.numeric(bandwidth)
  if (hz <= 0) stop(sprintf("bandwidth must be positive, got %g.", hz))

  # Kernel weights for g are fixed: they depend on Z only, not on b.
  Wz <- .hrz3_kmat(z, z, hz)

  resid <- function(b) {
    out <- numeric(n)
    for (i in seq_len(n)) {
      s <- 0
      for (k in seq_len(p)) s <- s + X[i, k] * b[k]
      out[i] <- y[i] - s
    }
    out
  }

  ghat <- function(r) {
    vapply(seq_len(n), function(i) .hrz3_wquant(r, Wz[i, ], tau), 0)
  }

  crit <- function(b) {
    r <- resid(b)
    g <- ghat(r)
    acc <- numeric(p)
    for (i in seq_len(n)) {
      ind <- if ((r[i] - g[i]) <= 0) 1 else 0
      for (k in seq_len(p)) acc[k] <- acc[k] + X[i, k] * (tau - ind)
    }
    sum((acc / n)^2)
  }

  XtX <- t(X) %*% X
  Xty <- as.numeric(t(X) %*% y)
  b0 <- as.numeric(.s03ridgesolve(XtX, Xty))
  cm <- .hrz_coord_min(crit, b0, niter = as.integer(niter), delta = 1,
                       shrink = 0.5, steps = 3L)
  b_hat <- as.numeric(cm$par)
  g_hat <- ghat(resid(b_hat))

  list(beta_tau = b_hat, g_tau_hat = g_hat, criterion = as.numeric(cm$value),
       tau = tau, bandwidth = hz, n = n, p = p,
       method = "Horowitz (2009) eq. (3.38), Chen-Linton-Van Keilegom")
}

# canonical full-name alias (Py<->R API parity)
#' @rdname Hrzplrq
#' @keywords internal
#' @export
morie_horowitz_plr_quantile <- Hrzplrq

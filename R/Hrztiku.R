# SPDX-License-Identifier: AGPL-3.0-or-later

#' Tikhonov regularization for NPIV when T is unknown
#'
#' Horowitz (2009), Semiparametric and Nonparametric Methods in
#' Econometrics, Section 5.4.1, pages 171-172.  For Y = g(X) + U with
#' E(U | W = w) = 0, both the operator T and the joint density f_XW are
#' unknown and are replaced by Nadaraya-Watson kernel estimators.  With
#'
#'   rhat(z) = n^-1 sum_i Y_i fhat^(-i)_XW(z, W_i)              (5.71)
#'
#' and that(x, z) = int_0^1 fhat_XW(x, w) fhat_XW(z, w) dw defining
#' That, the estimator is
#'
#'   ghat = (That + a_n)^-1 rhat                                (5.72)
#'
#' The leave-one-out density is used in rhat "to avoid biases that
#' arise if the estimator of f_XW is not statistically independent of
#' its argument" (p. 171).  Dropping it is the easiest way to get a
#' plausible-looking but biased answer.
#'
#' Because that is already the Gram kernel of fhat_XW, That is
#' self-adjoint and positive semi-definite, so (5.72) coincides with
#' the normal-equation form and no separate T*T is formed.
#'
#' Two printing errors were corrected, both confirmed against a
#' rendered image of page 171: the displayed fhat_XW carries 1/(n h_n)
#' while summing a PRODUCT of two kernels (a bivariate density needs
#' 1/(n h_n^2), and the constant does not cancel because rhat is linear
#' and That quadratic in fhat); and the displayed that integrates dz
#' where it must be dw, matching (5.43) on page 157.
#'
#' @param x Numeric vector, the endogenous regressor.
#' @param y Numeric vector, the response.
#' @param w Numeric vector, the instrument.
#' @param bandwidth Numeric or NULL; kernel bandwidth on the [0, 1]
#'   scale.  Default `1.06 n^(-1/6)/sqrt(12)`: after the mid-rank map
#'   the marginals are exactly uniform, so Silverman's constant fixes
#'   the scale and not merely the rate.
#' @param alpha Numeric, the regularisation constant a_n in (5.72).
#'   Must be positive; at 0 the problem is ill-posed.
#' @param grid Integer, number of quadrature points on [0, 1].
#' @return Named list with g_hat, grid_points, r_hat, fxw, raw_mass,
#'   alpha, bandwidth, n, m, method.
#' @keywords internal
#' @examples
#' n <- 30
#' x <- sin(1.7 * seq_len(n))
#' w <- cos(1.1 * seq_len(n))
#' y <- 2 * x
#' Hrztiku(x, y, w, alpha = 1e-2, grid = 11)$g_hat[1]
#' @export
Hrztiku <- function(x, y, w, bandwidth = NULL, alpha = 1e-3, grid = 25L) {
  x <- as.numeric(x)
  y <- as.numeric(y)
  w <- as.numeric(w)
  n <- length(x)
  if (length(y) != n || length(w) != n) {
    stop(sprintf("x, y, w must have the same length; got %d, %d, %d.",
                 n, length(y), length(w)))
  }
  if (n < 3L) stop(sprintf("need at least 3 observations, got %d.", n))
  alpha <- as.numeric(alpha)
  if (alpha <= 0) {
    stop(sprintf(
      "alpha must be positive; the problem is ill-posed at 0, got %g.",
      alpha))
  }
  h <- if (is.null(bandwidth)) .hrz3_bw01(n) else as.numeric(bandwidth)
  if (h <= 0) stop(sprintf("bandwidth must be positive, got %g.", h))

  u <- .hrz3_u01(x)
  v <- .hrz3_u01(w)
  gw <- .hrz3_grid_w(grid)
  z <- gw$z
  wq <- gw$w
  m <- length(z)

  KX <- .hrz3_kmat(z, u, h)
  KWo <- .hrz3_kmat(v, v, h)

  fg <- .hrz3_fxw_grid(u, v, z, wq, h)
  fxw <- fg$f
  raw_mass <- fg$mass

  # Leave-one-out density at (z_k, W_i), then rhat by eq. (5.71).
  S <- KX %*% t(KWo)
  Floo <- (S - KX * rep(diag(KWo), each = m)) / ((n - 1) * h * h * raw_mass)
  r_hat <- as.numeric(Floo %*% y) / n

  # that(x, z) = int fhat(x, w) fhat(z, w) dw, eq. (5.43) form.
  that <- fxw %*% (wq * t(fxw))

  # (That h)(z_l) = sum_k wq_k that(z_k, z_l) h_k; solve (T + a I) g = r.
  A <- t(that * wq)
  A <- A + diag(alpha, m)
  g_hat <- as.numeric(solve(A, r_hat))

  list(g_hat = g_hat, grid_points = z, r_hat = r_hat, fxw = fxw,
       raw_mass = raw_mass, alpha = alpha, bandwidth = h,
       n = n, m = m,
       method = "Horowitz (2009) eq. (5.72), g = (That + a_n)^{-1} rhat")
}

# canonical full-name alias (Py<->R API parity)
#' @rdname Hrztiku
#' @keywords internal
#' @export
morie_horowitz_tikhonov_unknown_T <- Hrztiku

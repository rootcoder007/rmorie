# SPDX-License-Identifier: AGPL-3.0-or-later

#' Transformation model with nonparametric T (Horowitz's 1996
#' estimator)
#'
#' Horowitz (2009), Semiparametric and Nonparametric Methods in
#' Econometrics, Section 6.3.1, pages 216-218.  The model is
#' T(Y) = X'beta + U with T an unknown strictly increasing function and
#' U independent of X with unknown CDF F.
#'
#' With Z = X'beta and G(. | z) the CDF of Y given Z = z, the model
#' implies G(y | z) = F\[T(y) - z\], hence G_y = T' F' and G_z = -F', so
#' T'(y) = -G_y(y | z) / G_z(y | z) and
#'
#'   T(y) = - int_\{y0\}^\{y\} G_y(v|z) / G_z(v|z) dv               (6.57)
#'
#' Averaging over z against a weight w supported on a compact S_w with
#' int_\{S_w\} w = 1 (6.58) gives
#'
#'   T(y) = - int_\{y0\}^\{y\} int_\{S_w\} w(z) G_y(v|z)/G_z(v|z) dz dv (6.59)
#'
#' and the estimator replaces G_y, G_z by the kernel estimators
#' (6.61)-(6.62):
#'
#'   Tn(y) = - int_\{y0\}^\{y\} int_\{S_w\} w(z)
#'             Gny(v|z)/Gnz(v|z) dz dv                          (6.60)
#'
#' The leading minus signs in (6.57), (6.59) and (6.60) were read off a
#' RENDERED IMAGE of page 217, not from an extracted text layer;
#' G_z < 0, so the sign is what makes T increasing, and dropping it
#' silently inverts the estimate.
#'
#' Averaging over z is not cosmetic.  The pointwise ratio Gny/Gnz
#' converges more slowly than n^(-1/2); integrating over z and v
#' creates the averaging effect that restores the n^(-1/2) rate.  That
#' is why the estimator is based on (6.59) and not on (6.57).
#'
#' T is estimated only on a compact interval \[y2, y1\] strictly inside
#' the support of Y (p. 219): T may be unbounded at the boundary and
#' G_z is likely to vanish there.  The default takes the 10th to 90th
#' percentiles, and y0 is the central grid point, so Tn(y0) = 0 holds
#' exactly as required by HT5(e).
#'
#' beta is estimated by the density-weighted average derivative of
#' Section 2.6.1 with the scale normalisation |beta_1| = 1 (HT2(a));
#' the text notes that (6.1) with nonparametric T and F "is a
#' semiparametric single-index model", so any Chapter 2 estimator is
#' admissible.
#'
#' @param x Numeric vector or n by d matrix of covariates.  The first
#'   column carries the scale normalisation.
#' @param y Numeric vector, the response.
#' @param ny Integer, grid points on \[y2, y1\]; forced odd so that y0 is
#'   a grid point.
#' @param nz Integer, quadrature points on the weight support S_w.
#' @param bandwidth Numeric or NULL; common bandwidth, default
#'   Silverman's rule per variable.
#' @return Named list with T_hat, beta_hat, y_grid, y0, y2, y1, index,
#'   i0, monotone, bandwidth_y, bandwidth_z, n, d, method.
#' @keywords internal
#' @examples
#' n <- 40
#' x <- cbind(sin(1.3 * seq_len(n)), cos(0.6 * seq_len(n)))
#' y <- exp(x[, 1] + 0.5 * x[, 2])
#' Hrztmod(x, y, ny = 7L, nz = 7L)$monotone
#' @export
Hrztmod <- function(x, y, ny = 21L, nz = 21L, bandwidth = NULL) {
  y <- as.numeric(y)
  n <- length(y)
  X <- if (is.null(dim(x))) matrix(as.numeric(x), ncol = 1L) else as.matrix(x)
  if (nrow(X) != n) stop(sprintf("x must have %d rows, got %d.", n, nrow(X)))
  d <- ncol(X)
  if (n < 10L) stop(sprintf("need at least 10 observations, got %d.", n))
  ny <- as.integer(ny)
  nz <- as.integer(nz)
  if (ny < 3L || nz < 3L) {
    stop(sprintf("ny and nz must be >= 3, got %d and %d.", ny, nz))
  }
  if (ny %% 2L == 0L) ny <- ny + 1L

  hy <- if (is.null(bandwidth)) .hrz_silverman(y) else as.numeric(bandwidth)
  hb <- if (is.null(bandwidth)) {
    .hrz_silverman(X[, 1L])
  } else {
    as.numeric(bandwidth)
  }
  beta <- .hrz3_index_dir(X, y, hb)
  Z <- as.numeric(X %*% beta)
  hz <- if (is.null(bandwidth)) .hrz_silverman(Z) else as.numeric(bandwidth)

  y2 <- .s03quantile7(y, 0.10)
  y1 <- .s03quantile7(y, 0.90)
  if (!(y1 > y2)) {
    stop("Y has no spread between its 10th and 90th percentiles.")
  }
  za <- .s03quantile7(Z, 0.25)
  zb <- .s03quantile7(Z, 0.75)
  if (!(zb > za)) stop("the index has no spread on the weight support S_w.")

  dv <- (y1 - y2) / (ny - 1L)
  ygrid <- y2 + (seq_len(ny) - 1L) * dv
  ygrid[1L] <- y2
  ygrid[ny] <- y1
  i0 <- (ny - 1L) %/% 2L
  y0 <- ygrid[i0 + 1L]
  dz <- (zb - za) / (nz - 1L)
  zgrid <- za + (seq_len(nz) - 1L) * dz
  wz <- rep(dz, nz)
  wz[1L] <- dz / 2
  wz[nz] <- dz / 2
  wt <- 1 / (zb - za)     # w(z) uniform on S_w, satisfies (6.58)

  inner <- numeric(ny)
  for (k in seq_len(ny)) {
    v <- ygrid[k]
    KY <- exp(-0.5 * ((y - v) / hy)^2) / .hrz3_sqrt2pi
    ind <- as.numeric(y <= v)
    acc <- 0
    for (q in seq_len(nz)) {
      uu <- (Z - zgrid[q]) / hz
      kv <- exp(-0.5 * uu * uu) / .hrz3_sqrt2pi
      dk <- (uu / hz) * kv          # d/dz K((Z_i - z)/h)
      A <- sum(ind * kv)
      B <- sum(kv)
      Az <- sum(ind * dk)
      Bz <- sum(dk)
      if (B <= 1e-300) next
      Gnz <- (Az * B - A * Bz) / (B * B)
      Gny <- sum(KY * kv) / (hy * B)
      if (abs(Gnz) < 1e-300) next
      acc <- acc + wz[q] * wt * (Gny / Gnz)
    }
    inner[k] <- acc
  }

  T <- numeric(ny)
  if (i0 + 2L <= ny) {
    for (k in (i0 + 2L):ny) {
      T[k] <- T[k - 1L] - 0.5 * dv * (inner[k - 1L] + inner[k])
    }
  }
  if (i0 >= 1L) {
    for (k in i0:1L) {
      T[k] <- T[k + 1L] + 0.5 * dv * (inner[k] + inner[k + 1L])
    }
  }

  monotone <- all(diff(T) >= -1e-12)

  list(T_hat = T, beta_hat = beta, y_grid = ygrid, y0 = y0, y2 = y2,
       y1 = y1, index = Z, i0 = i0, monotone = monotone,
       bandwidth_y = hy, bandwidth_z = hz, n = n, d = d,
       method = "Horowitz (2009) eq. (6.60), Horowitz (1996) estimator of T")
}

# canonical full-name alias (Py<->R API parity)
#' @rdname Hrztmod
#' @keywords internal
#' @export
morie_horowitz_transformation_model <- Hrztmod

# SPDX-License-Identifier: AGPL-3.0-or-later
#' Spectral density of a stationary field from its covariance function
#'
#' Building the field as a sum of sinusoids with random amplitudes and
#' uniform random phases gives C(h) = sum_j sigma_j^2 cos(w_j h)
#' (eq 2.27), and in the limit C(h) = int cos(w h) s(w) dw with inverse
#' s(w) = (1 / 2pi) int cos(w h) C(h) dh. Both C and s are even, so only
#' cosine terms survive.
#'
#' Both integrals are numerical. The omega grid cannot outrun the h grid:
#' cos(w h) sampled at spacing dh aliases above the Nyquist frequency
#' pi / dh, and integrating past it sums noise rather than tail mass, so
#' a wider range makes the answer worse. The range is capped there and
#' the limit reported; `integrated_density` must equal C(0), and the
#' shortfall is the tail truncated away.
#'
#' @param cov_func Function C(h) on the line.
#' @param omega Frequencies at which to return the density.
#' @param h_max,n Half-width and node count of the quadrature grid.
#' @return Named list: omega, spectral_density, variance,
#'   integrated_density, nyquist_omega.
#' @references Schabenberger & Gotway (2005), Sec 2.5.3, eq (2.27), pp. 66-68.
#' @examples
#' spwkth(function(h) exp(-2 * abs(h)), omega = c(0, 1, 3))$spectral_density
#' @export
spwkth <- function(cov_func, omega = NULL, h_max = 200, n = 40001) {
  if (!is.function(cov_func)) stop("`cov_func` must be a function C(h)")
  if (is.null(omega)) omega <- seq(0, 10, length.out = 201)
  h <- seq(-h_max, h_max, length.out = n)
  ch <- as.numeric(cov_func(abs(h)))
  trap <- function(y, x) sum(diff(x) * (utils::head(y, -1) + utils::tail(y, -1)) / 2)
  s <- vapply(omega, function(w) trap(cos(w * h) * ch, h) / (2 * pi), numeric(1))
  var <- as.numeric(cov_func(0))[1]
  dh <- h[2] - h[1]
  w_nyq <- 0.5 * pi / dh
  wide <- seq(-w_nyq, w_nyq, length.out = 20001)
  sw <- vapply(wide, function(w) trap(cos(w * h) * ch, h) / (2 * pi), numeric(1))
  list(omega = omega, spectral_density = s, variance = var,
       integrated_density = trap(sw, wide), nyquist_omega = w_nyq)
}

# SPDX-License-Identifier: AGPL-3.0-or-later
#' Hasselmann stochastic climate model: AR(1) red-noise fit
#'
#' x_t = phi x_(t-1) + eps_t with phi from the lag-one autocorrelation,
#' innovation variance c0 (1 - phi^2), decorrelation time -dt / log(phi) and
#' the red spectrum sigma2 dt / (1 - 2 phi cos(2 pi f dt) + phi^2).  Source
#' consulted: Hasselmann (1976), Stochastic climate models Part I, Theory,
#' Tellus 28(6), 473-485.
#'
#' @param x time series of the slow (climate) variable.
#' @param phi optional lag-one coefficient; Yule-Walker estimate if NULL.
#' @param dt sampling interval.
#' @param freq optional frequencies for the red spectrum.
#' @return list: estimate, phi, tau, sigma2_eps, var, c0, c1, spectrum, freq,
#'   n, method.
#' @keywords internal
#' @examples
#' ar1cl(c(1, -1, 1, -1, 1, -1))
#' @export
ar1cl <- function(x, phi = NULL, dt = 1, freq = NULL) {
  xs <- as.numeric(x); n <- length(xs); m <- mean(xs)
  c0 <- sum((xs - m)^2) / n
  c1 <- sum((xs[-1] - m) * (xs[-n] - m)) / n
  ph <- if (is.null(phi)) (if (c0 > 0) c1 / c0 else NA_real_) else as.numeric(phi)
  varx <- if (n > 1) c0 * n / (n - 1) else NA_real_
  s2 <- c0 * (1 - ph^2)
  tau <- if (!is.na(ph) && ph > 0 && ph < 1) -dt / log(ph) else Inf
  fv <- if (is.null(freq)) c(0, 0.125, 0.25, 0.375, 0.5) else as.numeric(freq)
  w <- 2 * pi * fv * dt
  spec <- s2 * dt / (1 - 2 * ph * cos(w) + ph^2)
  list(estimate = as.numeric(ph), phi = as.numeric(ph), tau = as.numeric(tau),
       sigma2_eps = as.numeric(s2), var = as.numeric(varx),
       c0 = as.numeric(c0), c1 = as.numeric(c1), spectrum = spec, freq = fv,
       n = as.integer(n),
       method = "AR(1) stochastic climate model (Hasselmann 1976)")
}

# CANONICAL TEST
# stopifnot(ar1cl(c(1, -1, 1, -1, 1, -1))$phi < -0.7)
# stopifnot(abs(ar1cl(c(1, -1, 1, -1), phi = 0)$sigma2_eps - 1) < 1e-12)

#' @rdname ar1cl
#' @keywords internal
#' @export
morie_ar1_climate <- ar1cl

# SPDX-License-Identifier: AGPL-3.0-or-later
#' Sum-of-sinusoids (spectral) representation, eqs (2.26)-(2.27).
#'
#' Z(s) = mu + sum_j A_j cos(w_j s + phi_j) with random amplitudes A_j
#' and phases phi_j uniform on (0, 2pi), all mutually independent. The
#' Y_j have zero mean because the integral of cos(a + phi) over a full
#' period vanishes, and the covariance collapses to a cosine sum
#' C(h) = sum_j sigma_j^2 cos(w_j h) with sigma_j^2 = E[A_j^2] / 2. So
#' Var[Z(s)] = C(0) = sum_j sigma_j^2: the spectrum is a budget for the
#' variance, which is the content of the representation.
#'
#' @param h Lags at which to evaluate C.
#' @param sigma2 Variance at each frequency; a decaying set by default.
#' @param omega Frequencies, same length as `sigma2`.
#' @param mu Process mean, carried into the realisation.
#' @param seed Optional seed for amplitudes and phases.
#' @return Named list: h, covariance, variance, sum_sigma2, omega,
#'   sigma2, realisation.
#' @references Schabenberger & Gotway (2005), Sec 2.5, eqs (2.26)-(2.27).
#' @examples
#' spspec(c(0, 1, 2))$variance
#' @export
spspec <- function(h, sigma2 = NULL, omega = NULL, mu = 0, seed = NULL) {
  h <- as.numeric(h)
  if (is.null(omega)) omega <- seq(0.2, 4, length.out = 12)
  if (is.null(sigma2)) sigma2 <- exp(-omega)
  if (length(sigma2) != length(omega)) {
    stop("`sigma2` and `omega` must have the same length")
  }
  if (any(sigma2 < 0)) stop("`sigma2` entries must be non-negative variances")
  cov <- vapply(h, function(hh) sum(sigma2 * cos(omega * hh)), numeric(1))
  if (!is.null(seed)) set.seed(seed)
  amp <- sqrt(2 * sigma2)
  phase <- stats::runif(length(omega), 0, 2 * pi)
  realisation <- mu + vapply(h, function(s) sum(amp * cos(omega * s + phase)),
                             numeric(1))
  list(h = h, covariance = cov, variance = sum(sigma2),
       sum_sigma2 = sum(sigma2), omega = omega, sigma2 = sigma2,
       realisation = realisation)
}

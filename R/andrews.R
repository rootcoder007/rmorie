# SPDX-License-Identifier: AGPL-3.0-or-later
#' Andrews sine psi function
#'
#' Formula: rho(r) = c^2(1 - cos(r/c)), psi(r) = c sin(r/c) for |r| <= c pi, and 0 otherwise
#'
#' @param r Scaled residuals.
#' @param c Tuning constant; 1.339 gives 95% Gaussian efficiency.

#' @param r See Usage.
#' @param c See Usage.
#' @return List with ``psi``, ``rho``, ``psi_deriv``, ``rejected`` (count with |r| > c pi), ``c``, ``n``.
#' @references Andrews, Bickel, Hampel, Huber, Rogers and Tukey (1972), Robust Estimates of Location, Princeton University Press; Andrews (1974), Technometrics 16:523-531. Not held locally; the form and the tuning constant 1.339 are as implemented by statsmodels' AndrewWave norm, the reference implementation, whose source was read: rho = a^2(1 - cos(z/a)), psi = a sin(z/a), weights = sin(z/a)/(z/a), psi_deriv = cos(z/a), all zero (rho constant) beyond |z| > a pi.
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' Andrewspsi(V)
Andrewspsi <- function(r, c = 1.339) {
  r <- .t1_vec(r)
  c <- as.numeric(c)
  if (c <= 0) stop("c must be positive")
  lim <- c * pi
  inside <- abs(r) <= lim
  .t1_result(psi = ifelse(inside, c * sin(r / c), 0),
             rho = ifelse(inside, c^2 * (1 - cos(r / c)), 2 * c^2),
             psi_deriv = ifelse(inside, cos(r / c), 0),
             rejected = sum(!inside), c = c, n = length(r),
             method = "Andrews sine psi")
}

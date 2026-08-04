# SPDX-License-Identifier: AGPL-3.0-or-later
#' Andrews sine psi function.
#'
#' Formula: psi(r) = sin(r/c) for |r| <= c pi, and 0 otherwise
#'
#' @param r Scaled residuals.
#' @param c Tuning constant; 1.339 gives 95% Gaussian efficiency.

#' @return List with ``psi``, ``rejected`` (count with |r| > c pi), ``c``, ``n``.
#' @references Andrews, Bickel, Hampel, Huber, Rogers and Tukey (1972), Robust Estimates of Location, Princeton University Press; Andrews (1974), Technometrics 16:523-531. Not held locally; the form and the tuning constant 1.339 are as documented by statsmodels' AndrewWave norm, the reference implementation.
#' @export
Andrewspsi <- function(r, c = 1.339) {
  r <- .t1_vec(r); c <- as.numeric(c)
  if (c <= 0) stop("c must be positive")
  lim <- c * pi
  .t1_result(psi = ifelse(abs(r) <= lim, sin(r / c), 0),
             rejected = sum(abs(r) > lim), c = c, n = length(r),
             method = "Andrews sine psi")
}

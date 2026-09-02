# SPDX-License-Identifier: AGPL-3.0-or-later
#' Geographically weighted regression: locally varying coefficients
#'
#' Same estimator as [gwreg()]; this delegates rather than carrying a
#' second implementation. The model is fitted locally in the spatial
#' sense while allowing general covariates -- the weights determine how
#' far each observation influences the coefficients estimated at a given
#' point, and the locality is based on spatial position, not on
#' covariate values.
#'
#' @param x Covariates (n by p).
#' @param y Response, length n.
#' @param coords Coordinates (n by 2).
#' @param bandwidth Kernel bandwidth.
#' @param kernel Kernel family.
#' @return The result of `gwreg()`.
#' @references Schabenberger & Gotway (2005), Sec 6.1.3 "Spatially
#'   Explicit Models", eq (6.9), pp. 316-317, citing Fotheringham et al. (2002).
#' @examples
#' n <- 30
#' spgwr(cbind(1, runif(n)), rnorm(n), matrix(runif(2 * n), n, 2))
#' @export
spgwr <- function(x, y, coords, bandwidth = NULL, kernel = "gaussian") {
  gwreg(x, y, coords, bandwidth, kernel)
}

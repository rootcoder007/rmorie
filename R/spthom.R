# SPDX-License-Identifier: AGPL-3.0-or-later
#' Thomas process: the Neyman-Scott process with Gaussian offspring.
#'
#' Poisson(mu) offspring per parent displaced by an isotropic Gaussian of
#' standard deviation sigma, parents Poisson(rho), giving
#'
#'   K(r) = pi r^2 + (1 - exp(-r^2 / (4 sigma^2))) / rho
#'
#' This is the Gaussian special case of the general Neyman-Scott form, so
#' it delegates to [spnscl()] rather than restating the algebra.
#'
#' Note this is the theoretical K-function, not a simulator.
#'
#' @param r Distances, non-negative.
#' @param rho Parent intensity, positive.
#' @param mu Mean offspring per parent, positive.
#' @param sigma Displacement standard deviation, positive.
#' @return As [spnscl()], plus `k_function` as an alias of `k`.
#' @references Thomas, M. (1949) A generalization of Poisson's binomial
#'   limit for use in ecology. Biometrika 36(1-2):18-25.
#'   \doi{10.1093/biomet/36.1-2.18}. The cluster-process framework is
#'   Schabenberger & Gotway (2005) Sec 3.7.2, pp. 126-128, which does not
#'   name this special case.
#' @examples
#' spthom(seq(0, 1, length.out = 5), rho = 10, mu = 5, sigma = 0.1)$k
#' @export
spthom <- function(r, rho = 10, mu = 5, sigma = 0.1) {
  out <- spnscl(r, rho, mu, sigma)
  out$k_function <- out$k
  out
}

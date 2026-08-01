# SPDX-License-Identifier: AGPL-3.0-or-later
#' Neyman-Scott cluster process: second-order behaviour.
#'
#' Parents form a homogeneous Poisson process of intensity rho; each
#' produces N offspring displaced independently by a radially symmetric
#' density f. With a stationary parent process the cluster process is
#' stationary with intensity lambda = rho E\[N\], and the second-order
#' intensity is
#'
#'   lambda_2(h) = rho^2 E\[N\]^2 + rho E\[N(N-1)\] f(h)
#'
#' For N ~ Poisson(mu) with Gaussian displacements of standard deviation
#' sigma in the plane this integrates to
#'
#'   K(r) = pi r^2 + (1 - exp(-r^2 / (4 sigma^2))) / rho
#'
#' The first term is the CSR contribution and the second is the excess
#' from clustering: strictly positive, so a cluster process always sits
#' ABOVE the Poisson K-function, and vanishing as rho grows -- many
#' sparse clusters look Poisson.
#'
#' @param r Distances, non-negative.
#' @param rho Parent intensity, positive.
#' @param mu Mean offspring per parent, positive.
#' @param sigma Offspring displacement standard deviation, positive.
#' @return Named list: r, k, k_csr, excess, lambda, rho, mu, sigma.
#' @references Schabenberger & Gotway (2005), Sec 3.7.2, pp. 126-128.
#' @examples
#' spnscl(seq(0, 1, length.out = 5), rho = 10, mu = 5, sigma = 0.1)$excess
#' @export
spnscl <- function(r, rho = 10, mu = 5, sigma = 0.1) {
  if (rho <= 0 || mu <= 0 || sigma <= 0) {
    stop("`rho`, `mu` and `sigma` must all be > 0")
  }
  r <- as.numeric(r)
  if (any(r < 0)) stop("`r` must be non-negative")
  excess <- (1 - exp(-(r^2) / (4 * sigma^2))) / rho
  list(r = r, k = pi * r^2 + excess, k_csr = pi * r^2, excess = excess,
       lambda = rho * mu, rho = rho, mu = mu, sigma = sigma)
}

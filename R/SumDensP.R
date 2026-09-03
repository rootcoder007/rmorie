# SPDX-License-Identifier: AGPL-3.0-or-later

#' Strip probability for a sum density
#'
#' The convolution density at z, times the strip width dz.
#'
#' @param grid_x,density_x,grid_y,density_y the two densities.
#' @param z strip centre.
#' @param dz strip width.
#' @return list(probability, rho_z).
#' @references Morin, D. J. (2016). Probability: For the Enthusiastic
#'   Beginner. Createspace. Eq (6.66).
#' @examples
#' g <- seq(-8, 8, length.out = 1601); SumDensP(g, stats::dnorm(g), g, stats::dnorm(g),
#' 1, 0.01)$probability
#' @export
SumDensP <- function(grid_x, density_x, grid_y, density_y, z, dz) {
  rho <- SumDens(grid_x, density_x, grid_y, density_y, z)$density
  dz <- as.numeric(dz)
  if (length(dz) != 1L || is.na(dz)) stop("dz must be a single value.", call. = FALSE)
  list(probability = rho * dz, rho_z = rho)
}

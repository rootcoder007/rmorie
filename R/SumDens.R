# SPDX-License-Identifier: AGPL-3.0-or-later

#' Convolution density of a sum
#'
#' rho_Z(z) = integral rho_X(x) rho_Y(z - x) dx, trapezoid on the x
#' grid with rho_Y linearly interpolated and zero outside its grid.
#'
#' @param grid_x,density_x the density of X.
#' @param grid_y,density_y the density of Y.
#' @param z evaluation point.
#' @return list(z, density).
#' @references Morin, D. J. (2016). Probability: For the Enthusiastic
#'   Beginner. Createspace. Eq (6.65).
#' @examples
#' g <- seq(-8, 8, length.out = 1601); SumDens(g, stats::dnorm(g), g, stats::dnorm(g), 1)$density
#' @export
SumDens <- function(grid_x, density_x, grid_y, density_y, z) {
  gx <- as.numeric(grid_x); dx <- as.numeric(density_x)
  gy <- as.numeric(grid_y); dy <- as.numeric(density_y)
  z <- as.numeric(z)
  if (length(gx) != length(dx) || length(gy) != length(dy) ||
        length(gx) < 2L || length(gy) < 2L) {
    stop("grid/density pairs must be equal-length, n >= 2.", call. = FALSE)
  }
  if (length(z) != 1L || is.na(z)) stop("z must be a single value.", call. = FALSE)
  vals <- dx * stats::approx(gy, dy, xout = z - gx, yleft = 0, yright = 0)$y
  dens <- sum(diff(gx) * (vals[-1] + vals[-length(vals)]) / 2)
  list(z = z, density = dens)
}

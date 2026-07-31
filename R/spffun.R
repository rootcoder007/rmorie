# SPDX-License-Identifier: AGPL-3.0-or-later
#' F-function: the empty space (point-to-nearest-event) distribution.
#'
#' F(r) is the CDF of the distance from an ARBITRARY location to the
#' nearest event, in contrast to G, which measures from an arbitrary
#' EVENT. It is estimated from a regular grid of sample locations.
#'
#' Under CSR both have the form 1 - exp(-lambda pi r^2), but they respond
#' to departures in opposite directions: clustering leaves large empty
#' gaps, so F_hat falls BELOW the CSR curve while G_hat rises above it.
#'
#' @param points Event coordinates (n by 2).
#' @param region c(xmin, ymin, xmax, ymax) or vertices.
#' @param r Distances at which to evaluate the CDF.
#' @param n_grid Sample grid is n_grid by n_grid.
#' @return Named list: r, f, f_csr, empty_space_distances, lambda_est.
#' @references Schabenberger & Gotway (2005), Sec 3.3.4, pp. 97-98.
#' @examples
#' spffun(matrix(runif(400), 200, 2) * 10, region = c(0, 0, 10, 10), n_grid = 20)
#' @export
spffun <- function(points, region = NULL, r = NULL, n_grid = 40) {
  p <- as.matrix(points)
  reg <- .sp_region(region, p)
  gx <- seq(reg[1], reg[3], length.out = n_grid)
  gy <- seq(reg[2], reg[4], length.out = n_grid)
  grid <- as.matrix(expand.grid(x = gx, y = gy))
  d <- apply(grid, 1, function(g) sqrt(min(colSums((t(p) - g)^2))))
  if (is.null(r)) r <- seq(0, max(d), length.out = 25)
  r <- as.numeric(r)
  f <- vapply(r, function(y) sum(d <= y) / length(d), numeric(1))
  lam <- .sp_intensity(p, reg)
  list(r = r, f = f, f_csr = 1 - exp(-lam * pi * r^2),
       empty_space_distances = d, lambda_est = lam)
}

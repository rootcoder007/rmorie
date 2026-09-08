# SPDX-License-Identifier: AGPL-3.0-or-later
#' G-function: the nearest-neighbour distance distribution
#'
#' G_hat(y0) = #(y_i <= y0) / n, the empirical probability that an
#' event's nearest-neighbour distance is at most y0. Under CSR with
#' intensity lambda the theoretical form is G(y) = 1 - exp(-lambda pi y^2).
#' In a clustered pattern G_hat sits ABOVE this curve at small y; in a
#' regular pattern it sits below.
#'
#' @param points Event coordinates (n by 2).
#' @param r Distances at which to evaluate the CDF.
#' @param region Used only for the CSR reference intensity.
#' @return Named list: r, g, g_csr, nn_distances, mean_nn, lambda_est.
#' @references Schabenberger & Gotway (2005), Sec 3.3.4, pp. 97-98.
#' @examples
#' spgfun(matrix(runif(400), 200, 2) * 10, region = c(0, 0, 10, 10))
#' @export
spgfun <- function(points, r = NULL, region = NULL) {
  p <- as.matrix(points)
  nn <- .sp_nn(p)
  reg <- .sp_region(region, p)
  if (is.null(r)) r <- seq(0, max(nn), length.out = 25)
  r <- as.numeric(r)
  g <- vapply(r, function(y) sum(nn <= y) / length(nn), numeric(1))
  lam <- .sp_intensity(p, reg)
  list(r = r, g = g, g_csr = 1 - exp(-lam * pi * r^2),
       nn_distances = nn, mean_nn = mean(nn), lambda_est = lam)
}

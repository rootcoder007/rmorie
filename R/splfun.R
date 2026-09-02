# SPDX-License-Identifier: AGPL-3.0-or-later
#' L-function, the variance-stabilised K-function
#'
#' L(h) = sqrt(K(h) / pi). Under CSR K(h) = pi h^2, so L(h) = h exactly
#' and L(h) - h is a horizontal reference line at zero. The book
#' recommends plotting L(h) - h against h: clustering appears as positive
#' values at short distances, regularity as negative ones.
#'
#' @param points Event coordinates (n by 2).
#' @param lambda_est Intensity; estimated when NULL.
#' @param r Distances at which to evaluate.
#' @param region c(xmin, ymin, xmax, ymax) or vertices.
#' @param correction "border" or "none".
#' @return Named list: r, l, l_minus_r, k, lambda_est.
#' @references Schabenberger & Gotway (2005), Sec 3.4.2, p. 103.
#' @examples
#' pts <- matrix(runif(400), 200, 2) * 10
#' splfun(pts, r = seq(0.1, 1.5, length.out = 8), region = c(0, 0, 10, 10))
#' @export
splfun <- function(points, lambda_est = NULL, r = NULL, region = NULL,
                   correction = "border") {
  reg <- .sp_region(region, points)
  kr <- spkfun(points, lambda_est, r, reg, correction)
  ell <- sqrt(pmax(kr$k, 0) / pi)
  list(r = kr$r, l = ell, l_minus_r = ell - kr$r, k = kr$k,
       lambda_est = kr$lambda_est)
}

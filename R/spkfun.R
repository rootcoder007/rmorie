# SPDX-License-Identifier: AGPL-3.0-or-later
#' Ripley's K-function.
#'
#' K(h) = (2 pi / lambda^2) integral_0^h x lambda_2(x) dx. For a simple
#' process lambda K(h) is the expected number of EXTRA events within
#' distance h of an arbitrary event. Under the homogeneous Poisson
#' process that is lambda pi h^2, so K(h) = pi h^2 -- the reference
#' curve every CSR comparison is made against. Clustering shows as
#' K(h) > pi h^2 at short lags, regularity as K(h) < pi h^2.
#'
#' @param points Event coordinates (n by 2).
#' @param lambda_est Intensity; estimated by eq (3.8) when NULL.
#' @param r Distances at which to evaluate; a default grid when NULL.
#' @param region c(xmin, ymin, xmax, ymax) or vertices; bounding box of
#'   `points` when NULL.
#' @param correction "border" (default) or "none" (naive, biased).
#' @return Named list: r, k, k_csr, lambda_est, correction.
#' @references Schabenberger & Gotway (2005), Secs 3.4.1-3.4.2,
#'   eqs (3.7)-(3.8), pp. 101-102.
#' @examples
#' pts <- matrix(runif(400), 200, 2) * 10
#' spkfun(pts, r = seq(0.1, 1.5, length.out = 8), region = c(0, 0, 10, 10))
#' @export
spkfun <- function(points, lambda_est = NULL, r = NULL, region = NULL,
                   correction = "border") {
  reg <- .sp_region(region, points)
  if (is.null(r)) {
    side <- min(reg[3] - reg[1], reg[4] - reg[2])
    r <- seq(0, side / 4, length.out = 20)
  }
  r <- as.numeric(r)
  k <- .sp_k(points, reg, r, correction)
  lam <- .sp_intensity(points, reg)
  if (!is.null(lambda_est)) {
    k <- k * (lam / as.numeric(lambda_est))
    lam <- as.numeric(lambda_est)
  }
  list(r = r, k = k, k_csr = pi * r^2, lambda_est = lam,
       correction = correction)
}

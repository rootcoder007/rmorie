# SPDX-License-Identifier: AGPL-3.0-or-later
#' Empirical semivariogram: half the mean squared difference.
#'
#' gamma(h) = 0.5 E[(Z(s) - Z(s+h))^2], estimated by Matheron's method of
#' moments over lag bins. The factor of one half is what makes gamma
#' comparable to a variance rather than to a squared difference.
#'
#' @param coords Coordinate matrix (n by d).
#' @param z Numeric vector of length n.
#' @param n_bins Number of lag bins.
#' @param max_dist Largest lag retained; defaults to half the maximum
#'   pair distance.
#' @return Named list: lag, gamma, n_pairs.
#' @references Schabenberger & Gotway (2005), Sec 1.4.3 / Ch 4.
#' @examples
#' spsemv(coords = matrix(runif(200), 100, 2), z = rnorm(100), n_bins = 5)
#' @export
spsemv <- function(coords, z, n_bins = 15, max_dist = NULL) {
  .sp_empirical_variogram(coords, z, n_bins, max_dist)
}

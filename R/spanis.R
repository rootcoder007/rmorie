# SPDX-License-Identifier: AGPL-3.0-or-later
#' Geometric anisotropy: correcting direction dependence by a linear map.
#'
#' If Z1(s) is stationary with isotropic covariance C1, then
#' Z(s) = Z1(Bs) has C(h) = C1(||Bh||), which is geometrically
#' anisotropic -- its iso-correlation contours are ellipses rather than
#' circles. The transformation is reversed by s* = As with A = B^-1.
#'
#' Returns the empirical semivariogram in the corrected space alongside
#' the uncorrected one, so the improvement is visible rather than
#' asserted.
#'
#' @param coords Coordinate matrix (n by d).
#' @param z Numeric vector of length n.
#' @param A_matrix The (d by d) correction A = B^-1. Defaults to identity.
#' @param n_bins Number of lag bins.
#' @param max_dist Largest lag retained.
#' @return Named list: lag, gamma, n_pairs, gamma_raw, coords_corrected,
#'   A_matrix.
#' @references Schabenberger & Gotway (2005), Sec 4.3.7, p. 151.
#' @examples
#' spanis(coords = matrix(runif(200), 100, 2), z = rnorm(100),
#'        A_matrix = diag(2), n_bins = 5)
#' @export
spanis <- function(coords, z, A_matrix = NULL, n_bins = 15, max_dist = NULL) {
  coords <- as.matrix(coords)
  z <- as.numeric(z)
  d <- ncol(coords)
  A <- if (is.null(A_matrix)) diag(d) else as.matrix(A_matrix)
  if (!identical(dim(A), c(d, d))) {
    stop("`A_matrix` must be ", d, " by ", d, " to match `coords`")
  }
  if (abs(det(A)) < 1e-300) {
    stop("`A_matrix` is singular; it must be invertible (A = B^-1)")
  }
  star <- coords %*% t(A)
  out <- .sp_empirical_variogram(star, z, n_bins, max_dist)
  raw <- .sp_empirical_variogram(coords, z, n_bins, max_dist)
  list(lag = out$lag, gamma = out$gamma, n_pairs = out$n_pairs,
       gamma_raw = raw$gamma, coords_corrected = star, A_matrix = A)
}

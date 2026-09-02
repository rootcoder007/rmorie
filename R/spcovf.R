# SPDX-License-Identifier: AGPL-3.0-or-later
#' Empirical covariance function of a second-order stationary field
#'
#' For a second-order stationary random field the covariance depends on
#' the lag alone, C(h) = Cov\[Z(s), Z(s+h)\], with C(0) = Var\[Z(s)\] the
#' sill. It is estimated here by binning pairs on lag and averaging the
#' centred cross-products.
#'
#' The relationship to the semivariogram is reported alongside, because
#' it only holds under SECOND-ORDER stationarity: gamma(h) = C(0) - C(h).
#' An intrinsically stationary process has a semivariogram but need not
#' have a covariance function at all, so the empirical semivariogram is
#' computed independently rather than derived from C, and the gap between
#' the two is a diagnostic.
#'
#' @param coords Coordinate matrix (n by d).
#' @param z Numeric vector of length n.
#' @param n_bins Number of lag bins.
#' @param max_dist Largest lag retained; half the maximum pair distance
#'   by default.
#' @return Named list: lag, covariance, semivariogram (estimated
#'   directly), implied_semivariogram (C(0) - C(h)), sill, n_pairs.
#' @references Schabenberger & Gotway (2005), Secs 1.4.2, 2.4.
#' @examples
#' spcovf(matrix(runif(200), 100, 2), rnorm(100), n_bins = 5)$sill
#' @export
spcovf <- function(coords, z, n_bins = 15, max_dist = NULL) {
  coords <- as.matrix(coords)
  z <- as.numeric(z)
  if (nrow(coords) != length(z)) {
    stop("`coords` and `z` must have the same number of rows")
  }
  d <- as.numeric(stats::dist(coords))
  mu <- mean(z)
  n <- length(z)
  idx <- which(upper.tri(matrix(0, n, n)), arr.ind = TRUE)
  # stats::dist enumerates column-major lower triangle; match that order
  ij <- which(lower.tri(matrix(0, n, n)), arr.ind = TRUE)
  cross <- (z[ij[, 1]] - mu) * (z[ij[, 2]] - mu)
  if (is.null(max_dist)) max_dist <- if (length(d)) max(d) / 2 else 1
  keep <- d <= max_dist
  d <- d[keep]; cross <- cross[keep]
  edges <- seq(0, max_dist, length.out = n_bins + 1)
  b <- pmin(pmax(findInterval(d, edges, rightmost.closed = TRUE), 1), n_bins)
  lag <- rep(NA_real_, n_bins); cov <- rep(NA_real_, n_bins)
  cnt <- integer(n_bins)
  for (k in seq_len(n_bins)) {
    m <- b == k
    cnt[k] <- sum(m)
    if (cnt[k] > 0) { lag[k] <- mean(d[m]); cov[k] <- mean(cross[m]) }
  }
  sill <- stats::var(z)
  gam <- .sp_empirical_variogram(coords, z, n_bins, max_dist)$gamma
  list(lag = lag, covariance = cov, semivariogram = gam,
       implied_semivariogram = sill - cov, sill = sill, n_pairs = cnt)
}

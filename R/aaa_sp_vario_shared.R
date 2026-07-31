# SPDX-License-Identifier: AGPL-3.0-or-later
#' Internal: isotropic correlogram R(h) for the Schabenberger models.
#'
#' The book parameterises the exponential and gaussian models by the
#' PRACTICAL range alpha: the lag at which correlation has fallen to
#' exp(-3) = 0.049787 ("0.05 or less", p. 143). The spherical model is
#' different -- it has a TRUE range and is exactly zero at h = alpha.
#'
#' @param h Numeric vector of non-negative lags.
#' @param rng Practical (or true) range, positive.
#' @param model One of "exponential", "gaussian", "spherical".
#' @return Numeric vector R(h).
#' @references Schabenberger & Gotway (2005), Sec 4.3, eqs (4.10)-(4.13).
#' @noRd
.sp_correlogram <- function(h, rng, model) {
  h <- as.numeric(h)
  if (any(h < 0)) stop("lag distances `h` must be non-negative")
  if (rng <= 0) stop("`range` must be > 0")
  switch(model,
    exponential = exp(-3 * h / rng),
    gaussian = exp(-3 * (h / rng)^2),
    spherical = {
      u <- h / rng
      ifelse(h <= rng, 1 - 1.5 * u + 0.5 * u^3, 0)
    },
    stop("unknown model: ", model)
  )
}

#' Internal: semivariogram gamma(h) = c0 + sigma0^2 (1 - R(h)), gamma(0) = 0.
#'
#' The nugget is a discontinuity AT the origin (Sec 4.3.6, p. 150).
#'
#' @param h Numeric vector of non-negative lags.
#' @param nugget Nugget c0, non-negative.
#' @param sill Partial sill sigma0^2, non-negative.
#' @param rng Range, positive.
#' @param model Correlogram family.
#' @return Numeric vector gamma(h).
#' @references Schabenberger & Gotway (2005), Sec 4.3.
#' @noRd
.sp_semivariogram <- function(h, nugget, sill, rng, model) {
  if (nugget < 0) stop("`nugget` must be >= 0")
  if (sill < 0) stop("`sill` must be >= 0")
  h <- as.numeric(h)
  g <- nugget + sill * (1 - .sp_correlogram(h, rng, model))
  g[h == 0] <- 0
  g
}

#' Internal: empirical semivariogram by the method of moments.
#'
#' gamma_hat(h) = 1 / (2 |N(h)|) * sum (Z(s_i) - Z(s_j))^2
#'
#' @param coords Coordinate matrix (n by d).
#' @param z Numeric vector of length n.
#' @param n_bins Number of lag bins.
#' @param max_dist Largest lag retained; default half the maximum pair distance.
#' @return List with lag, gamma, n_pairs.
#' @references Schabenberger & Gotway (2005), Ch 4.
#' @noRd
.sp_empirical_variogram <- function(coords, z, n_bins = 15, max_dist = NULL) {
  z <- as.numeric(z)
  coords <- as.matrix(coords)
  if (nrow(coords) != length(z)) {
    stop("`coords` and `z` must have the same number of rows")
  }
  d <- as.numeric(stats::dist(coords))
  sq <- as.numeric(stats::dist(z))^2
  if (is.null(max_dist)) max_dist <- if (length(d)) max(d) / 2 else 1
  keep <- d <= max_dist
  d <- d[keep]
  sq <- sq[keep]
  edges <- seq(0, max_dist, length.out = n_bins + 1)
  idx <- pmin(pmax(findInterval(d, edges, rightmost.closed = TRUE), 1), n_bins)
  lag <- rep(NA_real_, n_bins)
  gam <- rep(NA_real_, n_bins)
  cnt <- integer(n_bins)
  for (b in seq_len(n_bins)) {
    m <- idx == b
    cnt[b] <- sum(m)
    if (cnt[b] > 0) {
      lag[b] <- mean(d[m])
      gam[b] <- sum(sq[m]) / (2 * cnt[b])
    }
  }
  list(lag = lag, gamma = gam, n_pairs = cnt)
}

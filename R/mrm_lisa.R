# SPDX-License-Identifier: AGPL-3.0-or-later

#' LISA (Local Indicators of Spatial Association) on polygon-level
#' crime data + per-year polygon Moran's I time series
#'
#' R parity of \code{morie.mrm_tps_lisa()} and
#' \code{morie.mrm_tps_polygon_moran_per_year()}.  Local Moran's I per
#' polygon centroid with 999-permutation MC significance, plus a
#' convenience wrapper for the per-year time series used by the morie
#' empirical paper Section 7.11.
#'
#' @references
#' Anselin, L. (1995). Local indicators of spatial association --
#' LISA. \emph{Geographical Analysis}, 27(2), 93--115.
#'
#' @return The LISA callables return named \code{list}s with per-polygon
#'   local Moran's I, permutation p-values, cluster classifications, and
#'   (for the per-year wrapper) the time series of global Moran's I.
#' @examples
#' if (FALSE) {
#'   ncr <- read.csv("Neighbourhood_Crime_Rates_Open_Data.csv")
#'   mrm_tps_lisa(ncr,
#'     count_col = "ASSAULT_2024",
#'     lat_col = "lat", lon_col = "lon"
#'   )
#' }
#' @name mrm_lisa
NULL


#' Internal helper: Haversine Km Lisa
#' @noRd
.haversine_km_lisa <- function(lat1, lon1, lat2, lon2) {
  R <- 6371
  rad <- pi / 180
  dlat <- (lat2 - lat1) * rad
  dlon <- (lon2 - lon1) * rad
  a <- sin(dlat / 2)^2 + cos(lat1 * rad) * cos(lat2 * rad) * sin(dlon / 2)^2
  2 * R * asin(pmin(1, sqrt(a)))
}


#' Internal helper: Knn Weights Lisa
#' @noRd
.knn_weights_lisa <- function(lat, lon, k) {
  n <- length(lat)
  W <- matrix(0, n, n)
  for (i in seq_len(n)) {
    d <- .haversine_km_lisa(lat[i], lon[i], lat, lon)
    nn <- order(d)[2:(k + 1L)]
    W[i, nn] <- 1.0 / k
  }
  W
}


#' Local Moran's I per polygon + quadrant + 999-permutation
#' significance
#'
#' @param data data.frame with one row per polygon.
#' @param count_col Column with per-polygon counts (e.g.
#'   "ASSAULT_2024").
#' @param lat_col,lon_col WGS84 centroid columns.
#' @param id_col Optional polygon-ID column (passed through to output).
#' @param k k-NN spatial-weights neighbourhood (default 6).
#' @param n_permutations MC permutations (default 999, the
#'   spatial-statistics convention).  Local p-values use conditional
#'   randomization (Anselin 1995): \eqn{z_i} stays fixed and its
#'   neighbours are drawn from the other \eqn{n - 1} values; the folded
#'   pseudo p-value is \eqn{(\min(\#\ge, \#\le) + 1) / (R + 1)}, as GeoDa
#'   and \code{spdep::localmoran_perm}.
#' @param seed RNG seed.
#' @return A list with elements \code{n_polygons}, \code{global_moran_I},
#'   \code{permutations}, \code{knn_k}, \code{table} (per-polygon
#'   data.frame), \code{quadrants_all}, \code{quadrants_significant_p05},
#'   \code{n_significant_p05}.
#' @export
#' @examples
#' if (FALSE) {
#'   ncr <- read.csv("Neighbourhood_Crime_Rates_Open_Data.csv")
#'   res <- mrm_tps_lisa(ncr,
#'     count_col = "ASSAULT_2024",
#'     lat_col = "lat", lon_col = "lon"
#'   )
#' }
mrm_tps_lisa <- function(
  data,
  count_col,
  lat_col = "lat", lon_col = "lon",
  id_col = NULL,
  k = 6L, n_permutations = 999L, seed = 42L
) {
  stopifnot(is.data.frame(data), count_col %in% names(data))
  stopifnot(lat_col %in% names(data), lon_col %in% names(data))
  .rmorie_local_seed(as.integer(seed))

  keep <- stats::complete.cases(data[, c(count_col, lat_col, lon_col)])
  d <- data[keep, , drop = FALSE]
  n <- nrow(d)
  if (n < 5L) stop("need >= 5 polygons")

  lat <- d[[lat_col]]
  lon <- d[[lon_col]]
  x <- as.numeric(d[[count_col]])
  W <- .knn_weights_lisa(lat, lon, k)
  # population sd, so I_i = z_i * lag_i is spdep::localmoran's
  # (x_i - xbar) / m2 * sum_j w_ij (x_j - xbar) with m2 = sum((x - xbar)^2) / n
  z <- (x - mean(x)) / sqrt(mean((x - mean(x))^2))
  lag <- as.vector(W %*% z)
  I_local <- z * lag
  I_global <- sum(I_local) / sum(z^2)

  quad <- character(n)
  quad[(z > 0) & (lag > 0)] <- "HH"
  quad[(z > 0) & (lag <= 0)] <- "HL"
  quad[(z <= 0) & (lag > 0)] <- "LH"
  quad[(z <= 0) & (lag <= 0)] <- "LL"

  # conditional randomization (Anselin 1995; GeoDa, spdep::localmoran_perm):
  # z_i stays at i and its neighbours are drawn from the other n - 1
  # values; folded pseudo p = (min(#>=, #<=) + 1) / (R + 1)
  p_local <- vapply(seq_len(n), function(i) {
    nb <- which(W[i, ] != 0)
    w <- W[i, nb]
    others <- z[-i]
    Ip <- vapply(seq_len(n_permutations), function(b) {
      z[i] * sum(w * others[sample.int(n - 1L, length(nb))])
    }, numeric(1))
    # ties (the observed neighbour set, repeated values) count both ways
    tol <- 1e-10 * max(1, abs(I_local[i]))
    (min(sum(Ip >= I_local[i] - tol), sum(Ip <= I_local[i] + tol)) + 1) /
      (n_permutations + 1)
  }, numeric(1))

  tbl <- data.frame(
    id = if (!is.null(id_col)) d[[id_col]] else seq_len(n),
    lat = lat, lon = lon, x = x, z = z, lag_z = lag,
    I_local = I_local, quadrant = quad, p_value = p_local,
    significant_p05 = p_local <= 0.05,
    stringsAsFactors = FALSE
  )

  qa <- c(
    HH = sum(quad == "HH"), HL = sum(quad == "HL"),
    LH = sum(quad == "LH"), LL = sum(quad == "LL")
  )
  qs <- c(
    HH = sum((quad == "HH") & (p_local <= 0.05)),
    HL = sum((quad == "HL") & (p_local <= 0.05)),
    LH = sum((quad == "LH") & (p_local <= 0.05)),
    LL = sum((quad == "LL") & (p_local <= 0.05))
  )

  list(
    n_polygons = n,
    global_moran_I = round(I_global, 4),
    permutations = as.integer(n_permutations),
    knn_k = as.integer(k),
    table = tbl,
    quadrants_all = as.list(qa),
    quadrants_significant_p05 = as.list(qs),
    n_significant_p05 = sum(qs)
  )
}


#' Per-year global Moran's I time series across a polygon surface
#'
#' Convenience wrapper that loops \code{mrm_tps_lisa} over a vector
#' of per-year count columns.
#'
#' @param data Polygon-level data.frame.
#' @param year_cols Character vector of per-year count column names
#'   (e.g. \code{c("ASSAULT_2014", ..., "ASSAULT_2024")}).
#' @param lat_col,lon_col,k,n_permutations,seed as in
#'   \code{mrm_tps_lisa}.
#' @return data.frame with columns \code{year}, \code{n_events},
#'   \code{moran_I}, \code{global_p_value} (one-sided permutation test of
#'   positive autocorrelation, as \code{spdep::moran.mc}).
#' @examples
#' # 4 x 4 polygon grid with two yearly count columns.
#' set.seed(2026)
#' grid <- expand.grid(
#'   lat = 43.6 + (0:3) * 0.02,
#'   lon = -79.4 + (0:3) * 0.02
#' )
#' grid$ASSAULT_2023 <- rpois(nrow(grid), lambda = grid$lat * 10)
#' grid$ASSAULT_2024 <- rpois(nrow(grid), lambda = grid$lat * 12)
#' res <- mrm_tps_polygon_moran_per_year(
#'   grid,
#'   year_cols = c("ASSAULT_2023", "ASSAULT_2024"),
#'   lat_col = "lat", lon_col = "lon",
#'   k = 4L, n_permutations = 99L, seed = 42L
#' )
#' res
#' @export
mrm_tps_polygon_moran_per_year <- function(
  data, year_cols,
  lat_col = "lat", lon_col = "lon",
  k = 6L, n_permutations = 999L, seed = 42L
) {
  rows <- list()
  for (c in year_cols) {
    yr <- regmatches(c, regexpr("\\d{4}", c))
    yr <- if (length(yr) > 0) as.integer(yr) else c
    res <- tryCatch(
      mrm_tps_lisa(data,
        count_col = c, lat_col = lat_col,
        lon_col = lon_col, k = k,
        n_permutations = n_permutations, seed = seed
      ),
      error = function(e) NULL
    )
    if (is.null(res)) next
    # Global p-value via permutation of the z-surface (mirrors
    # mrm_lisa.py:204-219 -- was missing in the R port; added 2026-05-22).
    .rmorie_local_seed(seed)
    # the polygons mrm_tps_lisa used: count and centroid all present
    keep <- stats::complete.cases(data[, c(c, lat_col, lon_col)])
    x <- as.numeric(data[[c]][keep])
    zsd <- sqrt(mean((x - mean(x))^2))
    if (length(x) < 2L || !is.finite(zsd) || zsd == 0) {
      p_global <- NA_real_
    } else {
      z <- (x - mean(x)) / zsd
      W <- .knn_weights_lisa(as.numeric(data[[lat_col]][keep]),
                             as.numeric(data[[lon_col]][keep]), k)
      I_obs <- sum(z * (W %*% z)) / sum(z * z)
      # one-sided test of positive autocorrelation, as spdep::moran.mc
      # (E[I] = -1/(n-1), so |I| >= |I_obs| is not a two-sided test)
      ge <- 0L
      for (b in seq_len(n_permutations)) {
        zp <- sample(z)
        if (sum(zp * (W %*% zp)) / sum(zp * zp) >= I_obs) ge <- ge + 1L
      }
      p_global <- round((ge + 1L) / (n_permutations + 1L), 4)
    }
    rows[[length(rows) + 1L]] <- data.frame(
      year = yr,
      n_events = as.integer(sum(data[[c]], na.rm = TRUE)),
      moran_I = res$global_moran_I,
      global_p_value = p_global,
      stringsAsFactors = FALSE
    )
  }
  do.call(rbind, rows)
}

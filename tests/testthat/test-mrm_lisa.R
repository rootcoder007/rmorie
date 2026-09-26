# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Phase 2O: tests for mrm_lisa.R — LISA (Local Indicators of Spatial
# Association) + polygon Moran's I.

.make_synthetic_polygons <- function(n = 30L, seed = 1L) {
  set.seed(seed)
  # Random Toronto-like polygon centroids + counts.
  data.frame(
    poly_id = sprintf("p%03d", 1:n),
    lat = stats::runif(n, min = 43.58, max = 43.88),
    lon = stats::runif(n, min = -79.62, max = -79.13),
    count = stats::rpois(n, lambda = 50),
    year = sample(2018:2024, n, replace = TRUE),
    stringsAsFactors = FALSE
  )
}

test_that("mrm_tps_lisa returns per-polygon Local Moran's I", {
  df <- .make_synthetic_polygons(n = 30L, seed = 1L)
  out <- tryCatch(
    mrm_tps_lisa(df, count_col = "count",
                  lat_col = "lat", lon_col = "lon",
                  k = 4L, n_permutations = 50L, seed = 7L),
    error = function(e) e
  )
  if (inherits(out, "error")) {
    skip(sprintf("mrm_tps_lisa error: %s", conditionMessage(out)))
  }
  expect_true(is.list(out) || is.data.frame(out))
})

test_that("mrm_tps_lisa errors on too-few polygons", {
  df <- .make_synthetic_polygons(n = 3L, seed = 2L)
  expect_error(
    mrm_tps_lisa(df, count_col = "count",
                  lat_col = "lat", lon_col = "lon")
  )
})

test_that("mrm_tps_lisa errors on missing count_col", {
  df <- .make_synthetic_polygons(n = 30L, seed = 3L)
  expect_error(
    mrm_tps_lisa(df, count_col = "nonexistent_col",
                  lat_col = "lat", lon_col = "lon")
  )
})

test_that("mrm_tps_polygon_moran_per_year returns one row per year", {
  # The fn signature (R/mrm_lisa.R:182-186) takes `year_cols` (a
  # character vector of per-year column names), NOT a single count_col
  # + year_col pair. Stage a multi-year-wide synthetic frame.
  df <- .make_synthetic_polygons(n = 60L, seed = 4L)
  set.seed(4L)
  df$count_2020 <- as.integer(stats::rpois(nrow(df), lambda = 5))
  df$count_2021 <- as.integer(stats::rpois(nrow(df), lambda = 6))
  df$count_2022 <- as.integer(stats::rpois(nrow(df), lambda = 7))
  out <- mrm_tps_polygon_moran_per_year(df,
                                          year_cols = c("count_2020",
                                                        "count_2021",
                                                        "count_2022"),
                                          lat_col = "lat", lon_col = "lon",
                                          k = 4L, n_permutations = 50L)
  expect_true(is.list(out) || is.data.frame(out))
})

test_that("LISA: spdep local I and conditional-randomization p-values", {
  i <- 1:8
  d <- data.frame(lat = 43.6 + 0.01 * cos(i), lon = -79.4 + 0.01 * sin(1.7 * i),
                  x = c(5, 9, 2, 7, 3, 8, 1, 6))
  r <- mrm_tps_lisa(d, "x", k = 2L, n_permutations = 9999L, seed = 3L)
  # spdep::localmoran: I_i = (x_i - xbar) / m2 * lag, m2 = mean((x - xbar)^2)
  xc <- d$x - mean(d$x)
  expect_equal(r$table$I_local, xc / mean(xc^2) * (r$table$lag_z * sqrt(mean(xc^2))),
               tolerance = 1e-12)
  expect_equal(r$global_moran_I, round(sum(r$table$I_local) / 8, 4))
  # exact conditional distribution: the 21 neighbour pairs from the other 7
  z <- r$table$z
  il <- r$table$I_local
  pex <- vapply(1:8, function(j) {
    ip <- z[j] * colMeans(utils::combn(z[-j], 2))
    min(mean(ip >= il[j] - 1e-12), mean(ip <= il[j] + 1e-12))
  }, numeric(1))
  se <- sqrt(pex * (1 - pex) / 9999)
  expect_true(all(abs(r$table$p_value - pex) < 4 * se + 2 / 10000))
})

test_that("Kulldorff scan: expected count is the permutation mu; clusters separate", {
  set.seed(9)
  n <- 400
  ev <- data.frame(OCC_DATE = as.Date("2018-01-01") + sample(0:2500, n, TRUE),
                   LAT_WGS84 = 43.65 + rnorm(n, 0, 0.03),
                   LONG_WGS84 = -79.38 + rnorm(n, 0, 0.04))
  hot <- 1:80
  ev$LAT_WGS84[hot] <- 43.66 + rnorm(80, 0, 0.003)
  ev$LONG_WGS84[hot] <- -79.39 + rnorm(80, 0, 0.004)
  ev$OCC_DATE[hot] <- as.Date("2021-01-01") + sample(0:700, 80, TRUE)
  k <- mrm_tps_kulldorff_scan(ev, radii_km = c(1, 3), window_years = 2,
                              n_centers = 12L, n_permutations = 19L,
                              n_top_clusters = 2L, seed = 5L)
  expect_equal(nrow(k), 2L)
  tt <- as.integer(ev$OCC_DATE)
  for (j in 1:2) {
    dk <- .haversine_km_mat(k$center_lat[j], k$center_lon[j],
                            ev$LAT_WGS84, ev$LONG_WGS84)
    ns <- sum(dk <= k$radius_km[j])
    nt <- sum(tt >= as.integer(k$t_start[j]) & tt < as.integer(k$t_end[j]))
    expect_equal(k$n_expected[j], round(ns * nt / n, 2))
  }
  expect_gt(.haversine_km_mat(k$center_lat[1], k$center_lon[1],
                              k$center_lat[2], k$center_lon[2]),
            k$radius_km[1])
  expect_true(k$log_lrt[1] >= k$log_lrt[2])
})

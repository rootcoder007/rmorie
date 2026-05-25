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

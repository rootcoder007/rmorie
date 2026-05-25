# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Unit tests for R/tps_spatial_advanced.R.
# Synthetic Toronto-bbox point patterns drive every callable so the
# coverage report exercises code-paths without a TPS data bridge.

set.seed(1L)

.mk_tps_points <- function(n = 200L, seed = 1L) {
  set.seed(seed)
  data.frame(
    HOOD_158   = sample(letters[1:15], n, replace = TRUE),
    LAT_WGS84  = 43.6 + runif(n, 0, 0.2),
    LONG_WGS84 = -79.4 + runif(n, 0, 0.2)
  )
}

.mk_tps_polys <- function(seed = 1L) {
  set.seed(seed)
  data.frame(
    HOOD_ID = letters[1:16],
    lat = rep(43.6 + (0:3) * 0.02, 4),
    lon = rep(-79.4 + (0:3) * 0.02, each = 4),
    ASSAULT_RATE_2023  = rpois(16, 30),
    ASSAULT_RATE_2024  = rpois(16, 32),
    HOMICIDE_RATE_2023 = rpois(16, 2),
    HOMICIDE_RATE_2024 = rpois(16, 2)
  )
}

test_that("morie_tps_ripley_k returns rich result on synthetic incidents", {
  set.seed(1L)
  df <- .mk_tps_points(n = 200L)
  rr <- morie_tps_ripley_k(df, ds_name = "Synth", radii_km = c(0.5, 1, 2))
  expect_s3_class(rr, "morie_tps_spatial_advanced_result")
  expect_true(is.numeric(rr$K))
  expect_equal(length(rr$radii_km), 3L)
  expect_true(rr$n > 0L)
  expect_true(is.data.frame(rr$table))
})

test_that("morie_tps_ripley_k short-circuits when n < 50", {
  set.seed(1L)
  df <- .mk_tps_points(n = 20L)
  rr <- morie_tps_ripley_k(df)
  expect_s3_class(rr, "morie_tps_spatial_advanced_result")
  expect_true(any(grepl("only", rr$warnings)))
})

test_that("morie_tps_ripley_k subsamples when n > max_n", {
  set.seed(1L)
  df <- .mk_tps_points(n = 600L)
  rr <- morie_tps_ripley_k(df, max_n = 200L, radii_km = c(0.5, 1))
  expect_lte(rr$n, 200L)
})

test_that("morie_tps_getis_ord_g_star runs on synthetic hoods", {
  set.seed(1L)
  df <- .mk_tps_points(n = 400L)
  rr <- morie_tps_getis_ord_g_star(df, ds_name = "Synth", k_neighbours = 3L)
  expect_s3_class(rr, "morie_tps_spatial_advanced_result")
  expect_true(rr$n >= 5L)
})

test_that("morie_tps_getis_ord_g_star warns when hood_col absent", {
  set.seed(1L)
  df <- .mk_tps_points()
  df$HOOD_158 <- NULL
  rr <- morie_tps_getis_ord_g_star(df)
  expect_true(any(grepl("missing", rr$warnings)))
})

test_that("morie_tps_getis_ord_g_star warns when too few hoods", {
  df <- data.frame(
    HOOD_158 = c("a","b","c"),
    LAT_WGS84 = c(43.6, 43.7, 43.8),
    LONG_WGS84 = c(-79.4, -79.3, -79.5)
  )
  rr <- morie_tps_getis_ord_g_star(df)
  expect_true(any(grepl("valid hoods", rr$warnings)))
})

test_that("morie_tps_dbscan_clusters dispatches sensibly", {
  skip_if_not_installed("dbscan")
  set.seed(1L)
  df <- .mk_tps_points(n = 200L)
  rr <- morie_tps_dbscan_clusters(df, eps_km = 1, min_samples = 5L)
  expect_s3_class(rr, "morie_tps_spatial_advanced_result")
  if (requireNamespace("dbscan", quietly = TRUE)) {
    expect_true("table" %in% names(rr))
  } else {
    expect_true(any(grepl("dbscan", rr$warnings)))
  }
})

test_that("morie_tps_dbscan_clusters warns with tiny input", {
  df <- .mk_tps_points(n = 10L)
  rr <- morie_tps_dbscan_clusters(df, eps_km = 0.5, min_samples = 2L)
  expect_s3_class(rr, "morie_tps_spatial_advanced_result")
})

test_that("morie_tps_dbscan_clusters subsamples", {
  set.seed(1L)
  df <- .mk_tps_points(n = 500L)
  rr <- morie_tps_dbscan_clusters(df, eps_km = 1, min_samples = 5L,
                                  max_n = 100L)
  expect_lte(rr$n, 100L)
})

test_that("morie_tps_polygon_morans_i runs on synthetic polygons", {
  polys <- .mk_tps_polys()
  rr <- morie_tps_polygon_morans_i(polys, value_col = "ASSAULT_RATE_2024",
                                   centroid_lat_col = "lat",
                                   centroid_lon_col = "lon")
  expect_s3_class(rr, "morie_tps_spatial_advanced_result")
  expect_true(is.finite(rr$moran_I) || is.na(rr$moran_I))
})

test_that("morie_tps_polygon_morans_i warns when value_col absent", {
  polys <- .mk_tps_polys()
  rr <- morie_tps_polygon_morans_i(polys, value_col = "MISSING",
                                   centroid_lat_col = "lat",
                                   centroid_lon_col = "lon")
  expect_true(any(grepl("not in attribute table", rr$warnings)))
})

test_that("morie_tps_polygon_morans_i errors on bad polygons input", {
  rr <- morie_tps_polygon_morans_i(list(a = 1), value_col = "X")
  expect_true(any(grepl("not recognised|data.frame", rr$warnings)))
})

test_that("morie_tps_bivariate_moran runs on synthetic polygons", {
  polys <- .mk_tps_polys()
  rr <- morie_tps_bivariate_moran(polys,
                                  x_col = "ASSAULT_RATE_2024",
                                  y_col = "HOMICIDE_RATE_2024",
                                  centroid_lat_col = "lat",
                                  centroid_lon_col = "lon")
  expect_s3_class(rr, "morie_tps_spatial_advanced_result")
})

test_that("morie_tps_bivariate_moran warns on missing columns", {
  polys <- .mk_tps_polys()
  rr <- morie_tps_bivariate_moran(polys, x_col = "NOPE", y_col = "ZIP",
                                  centroid_lat_col = "lat",
                                  centroid_lon_col = "lon")
  expect_true(any(grepl("missing column", rr$warnings)))
})

test_that("morie_tps_moran_sweep_heatmap returns a matrix", {
  polys <- .mk_tps_polys()
  rr <- morie_tps_moran_sweep_heatmap(
    polys,
    category_prefixes = c("ASSAULT_RATE", "HOMICIDE_RATE"),
    years = c(2023L, 2024L),
    centroid_lat_col = "lat",
    centroid_lon_col = "lon"
  )
  expect_s3_class(rr, "morie_tps_spatial_advanced_result")
  expect_true(is.matrix(rr$matrix))
  expect_equal(dim(rr$matrix), c(2L, 2L))
})

test_that("morie_tps_moran_sweep_heatmap defaults exercise built-in lists", {
  polys <- .mk_tps_polys()
  rr <- morie_tps_moran_sweep_heatmap(polys,
                                       centroid_lat_col = "lat",
                                       centroid_lon_col = "lon")
  expect_s3_class(rr, "morie_tps_spatial_advanced_result")
  expect_true(nrow(rr$matrix) >= 1L)
})

test_that("print.morie_tps_spatial_advanced_result fires", {
  set.seed(1L)
  df <- .mk_tps_points(n = 200L)
  rr <- morie_tps_ripley_k(df, ds_name = "PrintCheck", radii_km = c(0.5))
  expect_output(print(rr), "Ripley")
})
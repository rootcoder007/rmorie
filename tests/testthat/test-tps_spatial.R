# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Unit tests for R/tps_spatial.R: global + local Moran's I and Gaussian
# KDE. Synthetic Toronto-bbox incidents drive every call.

set.seed(1L)

.mk_tps_inc <- function(n = 400L, seed = 1L, n_hoods = 15L) {
  set.seed(seed)
  data.frame(
    HOOD_158   = sample(letters[1:n_hoods], n, replace = TRUE),
    LAT_WGS84  = 43.6 + runif(n, 0, 0.2),
    LONG_WGS84 = -79.4 + runif(n, 0, 0.2)
  )
}

test_that("morie_tps_morans_i_neighbourhood: synthetic incident data", {
  df <- .mk_tps_inc(400L)
  rr <- morie_tps_morans_i_neighbourhood(df, ds_name = "Synth")
  expect_s3_class(rr, "morie_tps_spatial_result")
  expect_true(rr$n >= 5L)
  expect_true(is.numeric(rr$moran_I) || is.na(rr$moran_I))
})

test_that("morie_tps_morans_i_neighbourhood warns on missing hood_col", {
  df <- .mk_tps_inc(); df$HOOD_158 <- NULL
  rr <- morie_tps_morans_i_neighbourhood(df)
  expect_true(any(grepl("missing", rr$warnings)))
})

test_that("morie_tps_morans_i_neighbourhood warns when no lat/lon", {
  df <- .mk_tps_inc()
  df$LAT_WGS84 <- NULL; df$LONG_WGS84 <- NULL
  rr <- morie_tps_morans_i_neighbourhood(df)
  expect_true(any(grepl("missing", rr$warnings)))
})

test_that("morie_tps_morans_i_neighbourhood warns when too few hoods", {
  df <- .mk_tps_inc(20L, n_hoods = 3L)
  rr <- morie_tps_morans_i_neighbourhood(df)
  expect_true(any(grepl("valid neighbourhoods|missing", rr$warnings)))
})

test_that("morie_tps_morans_i_neighbourhood: use_spdep path runs when avail", {
  df <- .mk_tps_inc(400L)
  rr <- morie_tps_morans_i_neighbourhood(df, use_spdep = TRUE)
  expect_s3_class(rr, "morie_tps_spatial_result")
})

test_that("morie_tps_local_morans_i returns per-hood table", {
  df <- .mk_tps_inc(400L)
  rr <- morie_tps_local_morans_i(df, ds_name = "Synth", top_n = 5L)
  expect_s3_class(rr, "morie_tps_spatial_result")
  expect_true(rr$n >= 5L)
})

test_that("morie_tps_local_morans_i warns on missing hood_col", {
  df <- .mk_tps_inc(); df$HOOD_158 <- NULL
  rr <- morie_tps_local_morans_i(df)
  expect_true(any(grepl("missing", rr$warnings)))
})

test_that("morie_tps_local_morans_i warns when lat/lon missing", {
  df <- .mk_tps_inc()
  df$LAT_WGS84 <- NULL; df$LONG_WGS84 <- NULL
  rr <- morie_tps_local_morans_i(df)
  expect_true(any(grepl("missing", rr$warnings)))
})

test_that("morie_tps_kde_density runs over 50+ geocoded incidents", {
  set.seed(1L)
  df <- data.frame(
    LAT_WGS84  = 43.65 + rnorm(120, 0, 0.02),
    LONG_WGS84 = -79.40 + rnorm(120, 0, 0.02)
  )
  rr <- morie_tps_kde_density(df, bandwidth = 0.01, ds_name = "Synth")
  expect_s3_class(rr, "morie_tps_spatial_result")
  expect_true(rr$n >= 50L)
  expect_true(is.numeric(rr$max_density))
})

test_that("morie_tps_kde_density warns on <50 geocoded", {
  df <- data.frame(LAT_WGS84 = c(43.7, 43.71), LONG_WGS84 = c(-79.4, -79.41))
  rr <- morie_tps_kde_density(df)
  expect_true(any(grepl("geocoded", rr$warnings)))
})

test_that("morie_tps_kde_density warns when lat/lon absent", {
  rr <- morie_tps_kde_density(data.frame(x = 1:5))
  expect_true(any(grepl("missing", rr$warnings)))
})

test_that("print.morie_tps_spatial_result fires", {
  df <- .mk_tps_inc(400L)
  rr <- morie_tps_morans_i_neighbourhood(df, ds_name = "PrintCheck")
  expect_output(print(rr), "Moran")
})
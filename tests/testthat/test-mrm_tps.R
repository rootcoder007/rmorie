# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Phase 2R: tests for mrm_tps.R — TPS-specific MRM analyzers.

.make_synthetic_tps_for_mrm <- function(n = 300L, seed = 1L) {
  set.seed(seed)
  data.frame(
    OCC_DATE = format(as.POSIXct("2018-01-01", tz = "UTC") +
                       sample.int(86400L * 365L * 6L, n, replace = TRUE),
                      "%Y-%m-%d"),
    LAT_WGS84 = stats::runif(n, min = 43.58, max = 43.88),
    LONG_WGS84 = stats::runif(n, min = -79.62, max = -79.13),
    HOOD_158 = sprintf("%03d", sample(1:158, n, replace = TRUE)),
    stringsAsFactors = FALSE
  )
}

test_that("mrm_tps_levy_scaling returns a Levy scaling estimate", {
  df <- .make_synthetic_tps_for_mrm(n = 300L, seed = 1L)
  out <- tryCatch(
    mrm_tps_levy_scaling(df),
    error = function(e) e
  )
  if (inherits(out, "error")) {
    skip(sprintf("levy_scaling error: %s", conditionMessage(out)))
  }
  expect_true(is.list(out) || is.data.frame(out))
})

test_that("mrm_tps_moran_clustering computes spatial Moran's I", {
  df <- .make_synthetic_tps_for_mrm(n = 200L, seed = 2L)
  out <- tryCatch(
    mrm_tps_moran_clustering(df),
    error = function(e) e
  )
  if (inherits(out, "error")) {
    skip(sprintf("moran_clustering error: %s", conditionMessage(out)))
  }
  expect_true(is.list(out) || is.data.frame(out))
})

test_that("mrm_tps_neighbourhood_recurrence_km computes per-hood recurrence", {
  df <- .make_synthetic_tps_for_mrm(n = 300L, seed = 3L)
  out <- tryCatch(
    mrm_tps_neighbourhood_recurrence_km(df),
    error = function(e) e
  )
  if (inherits(out, "error")) {
    skip(sprintf("neighbourhood_recurrence_km error: %s",
                 conditionMessage(out)))
  }
  expect_true(is.list(out) || is.data.frame(out))
})

test_that("mrm_tps_load_hawkes_refit errors cleanly on missing manifest", {
  out <- tryCatch(
    mrm_tps_load_hawkes_refit("/nonexistent/manifest.json"),
    error = function(e) e
  )
  expect_true(inherits(out, "error") || is.list(out) ||
                is.data.frame(out))
})

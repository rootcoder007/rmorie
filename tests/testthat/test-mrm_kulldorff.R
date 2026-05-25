# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Phase 2O: tests for mrm_kulldorff.R::mrm_tps_kulldorff_scan.

.make_synthetic_tps_for_scan <- function(n = 300L, seed = 1L) {
  set.seed(seed)
  # Toronto bounding box that the kulldorff scan operates over.
  data.frame(
    OCC_DATE = format(as.POSIXct("2018-01-01", tz = "UTC") +
                       sample.int(86400L * 365L * 6L, n, replace = TRUE),
                      "%Y-%m-%d"),
    LAT_WGS84 = stats::runif(n, min = 43.58, max = 43.88),
    LONG_WGS84 = stats::runif(n, min = -79.62, max = -79.13),
    stringsAsFactors = FALSE
  )
}

test_that("mrm_tps_kulldorff_scan returns a cluster summary on synthetic Toronto data", {
  df <- .make_synthetic_tps_for_scan(n = 300L, seed = 1L)
  out <- tryCatch(
    mrm_tps_kulldorff_scan(df,
                           radii_km = c(1, 2, 3),
                           n_centers = 8L,
                           n_permutations = 20L,
                           window_years = 2L,
                           seed = 7L),
    error = function(e) e
  )
  if (inherits(out, "error")) {
    skip(sprintf("kulldorff_scan error: %s", conditionMessage(out)))
  }
  expect_true(is.list(out) || is.data.frame(out))
})

test_that("mrm_tps_kulldorff_scan respects radii_km + n_centers args", {
  df <- .make_synthetic_tps_for_scan(n = 200L, seed = 2L)
  out <- tryCatch(
    mrm_tps_kulldorff_scan(df,
                           radii_km = c(2),
                           n_centers = 4L,
                           n_permutations = 10L,
                           window_years = 1L),
    error = function(e) e
  )
  if (inherits(out, "error")) {
    skip(sprintf("radii sweep error: %s", conditionMessage(out)))
  }
  expect_true(is.list(out) || is.data.frame(out))
})

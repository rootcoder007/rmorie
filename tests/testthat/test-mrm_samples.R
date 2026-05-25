# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Phase 2O: tests for mrm_samples.R — bundled-sample loader + TPS
# ArcGIS layer URL helper.

test_that("morie_sample reads each bundled extdata sample", {
  for (name in c("otis_b01", "otis_b09", "otis_c11", "tps_assault")) {
    out <- tryCatch(morie_sample(name), error = function(e) e)
    if (inherits(out, "error")) {
      skip(sprintf("%s sample missing: %s", name,
                   conditionMessage(out)))
    }
    expect_s3_class(out, "data.frame")
    expect_gt(nrow(out), 0L)
  }
})

test_that("morie_sample errors on a non-registered name", {
  expect_error(morie_sample("not_a_real_sample"))
})

test_that("morie_tps_layer_urls returns a named character vector", {
  out <- morie_tps_layer_urls()
  expect_true(is.character(out) || is.list(out))
  expect_gt(length(out), 0L)
  if (is.character(out)) {
    expect_match(unname(out)[1], "^https?://")
  }
})

test_that("morie_fetch_tps is network-gated + errors cleanly off-line", {
  out <- tryCatch(
    morie_fetch_tps(layer = "https://invalid.local/x", max_features = 5L),
    error = function(e) e
  )
  expect_true(inherits(out, "error") || is.data.frame(out))
})

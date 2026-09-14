# SPDX-License-Identifier: AGPL-3.0-or-later
# Coverage tests for R/ingest_tps.R -- TPS ArcGIS ingest helpers.

set.seed(1)

test_that("tps_layers returns name+url data.frame", {
  # 8s of the suite here, and r-universe's macOS x86_64 builder is
  # about 1.8 times slower. The check there is killed at sixty minutes
  # and the suite alone was twenty-six of them. The heavy files run in
  # our own CI, which sets NOT_CRAN, where the clock is ours.
  skip_on_cran()
  set.seed(1)
  df <- morie_ingest_tps_layers()
  expect_s3_class(df, "data.frame")
  expect_true(all(c("name", "url") %in% colnames(df)))
  expect_gte(nrow(df), 3L)
  expect_true(all(grepl("FeatureServer", df$url)))
})

test_that("features_to_rows handles empty + with-geometry payload", {
  skip_on_cran()
  set.seed(1)
  expect_equal(length(rmorie:::.morie_tps_features_to_rows(list(), FALSE)), 0L)
  features <- list(
    list(attributes = list(a = 1, b = "x"), geometry = list(x = 10, y = 20)),
    list(attributes = list(a = 2, b = "y"), geometry = list(x = 11, y = 21))
  )
  out <- rmorie:::.morie_tps_features_to_rows(features, TRUE)
  expect_length(out, 2L)
  expect_true("geom_x" %in% names(out[[1]]))
})

test_that("features_to_rows tolerates missing attributes", {
  skip_on_cran()
  set.seed(1)
  features <- list(list(geometry = list(x = 1, y = 2)))
  out <- rmorie:::.morie_tps_features_to_rows(features, TRUE)
  expect_length(out, 1L)
})

test_that("arcgis_query errors without httr2", {
  skip_on_cran()
  testthat::local_mocked_bindings(

    requireNamespace = function(package, ...) {

      if (identical(package, "httr2")) FALSE

      else TRUE

    },

    .package = "base"

  )
  set.seed(1)
  expect_error(rmorie:::.morie_tps_arcgis_query("http://x"), "httr2")
})

test_that("arcgis_query fails clean off-network", {
  skip_on_cran()
  set.seed(1)
  res <- tryCatch(
    rmorie:::.morie_tps_arcgis_query("http://127.0.0.1:1/layer", timeout = 1),
    error = function(e) NULL
  )
  expect_null(res)
})

test_that("ingest_tps_feature_layer routes through TPS helper (mocked)", {
  skip_on_cran()
  testthat::skip_if_not_installed("httr2")
  set.seed(1)
  testthat::local_mocked_bindings(
    .morie_dataset_tps_fetch = function(...) {
      data.frame(EVENT_UNIQUE_ID = "MOCK", OCC_YEAR = 2024L,
                  stringsAsFactors = FALSE)
    },
    .package = "rmorie"
  )
  url <- morie_ingest_tps_layers()$url[1]
  res <- morie_ingest_tps_feature_layer(url, max_features = 1L)
  expect_s3_class(res, "data.frame")
})

test_that("ingest_tps_fetch routes through TPS helper (mocked)", {
  skip_on_cran()
  testthat::skip_if_not_installed("httr2")
  set.seed(1)
  testthat::local_mocked_bindings(
    .morie_dataset_tps_fetch = function(...) {
      data.frame(EVENT_UNIQUE_ID = "MOCK", OCC_YEAR = 2024L,
                  stringsAsFactors = FALSE)
    },
    .package = "rmorie"
  )
  res <- morie_ingest_tps_fetch("major-crime", max_features = 1L)
  expect_s3_class(res, "data.frame")
})

test_that("ingest_tps_fetch rejects unknown layer name", {
  skip_on_cran()
  set.seed(1)
  expect_error(morie_ingest_tps_fetch("__nope__"), regexp = ".")
})
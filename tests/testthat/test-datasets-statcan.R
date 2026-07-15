# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Phase 3DDD3: StatCan CCJS WDS REST API loaders.

test_that("morie_datasets_statcan_ccjs_cubes returns 10-row registry", {
  c <- morie_datasets_statcan_ccjs_cubes()
  expect_s3_class(c, "data.frame")
  expect_equal(nrow(c), 10L)
  expect_setequal(names(c),
                  c("product_id", "cube_title_en",
                    "dimensions", "frequency"))
  expect_type(c$product_id, "integer")
  # All 10 product_ids are 8-digit catalogue IDs in the 35xxxxxx range
  # (CCJS subject matter code 35).
  expect_true(all(c$product_id > 35000000L &
                     c$product_id < 36000000L))
})

test_that("registered cubes include the canonical CCJS flagships", {
  c <- morie_datasets_statcan_ccjs_cubes()
  expect_true(35100177L %in% c$product_id)  # Incident-based
  expect_true(35100068L %in% c$product_id)  # Police personnel
  expect_true(35100026L %in% c$product_id)  # Homicide victims
})

# ---------------------------------------------------------------------------
# Deterministic unit tests: mock the internal HTTP layer so the WDS-loader
# logic is exercised offline (no live network). These ALWAYS run -- including
# on CRAN and CI -- and give real coverage of status-checking + object
# extraction. Live smoke tests against the real endpoint live at the bottom,
# gated by skip_on_ci() so external flakiness never red-fails the build.
# ---------------------------------------------------------------------------

test_that("morie_datasets_statcan_cube_metadata parses a WDS getCubeMetadata response", {
  testthat::local_mocked_bindings(
    .morie_dataset_http_post_json = function(url, body, ...) {
      list(list(
        status = "SUCCESS",
        object = list(
          productId   = 35100002L,
          cubeTitleEn = paste0("Incident-based cybercrime, by cyber-related ",
                               "violation and selected police services"),
          cubeTitleFr = "Cybercriminalite declaree par la police"
        )
      ))
    },
    .package = "rmorie"
  )
  m <- morie_datasets_statcan_cube_metadata(35100002L)
  expect_equal(m$status, "SUCCESS")
  expect_true(!is.null(m$object$cubeTitleEn))
  expect_match(m$object$cubeTitleEn, "cybercrime", ignore.case = TRUE)
})

test_that("morie_datasets_statcan_cube_metadata errors on a non-SUCCESS status", {
  testthat::local_mocked_bindings(
    .morie_dataset_http_post_json = function(url, body, ...) {
      list(list(status = "FAILED", object = "no such product"))
    },
    .package = "rmorie"
  )
  expect_error(morie_datasets_statcan_cube_metadata(99999999L),
               "status=FAILED")
})

test_that("morie_datasets_statcan_cube_metadata errors on an empty response", {
  testthat::local_mocked_bindings(
    .morie_dataset_http_post_json = function(url, body, ...) list(),
    .package = "rmorie"
  )
  expect_error(morie_datasets_statcan_cube_metadata(35100002L),
               "empty or malformed")
})

test_that("morie_datasets_statcan_full_csv_url builds the canonical en zip URL", {
  testthat::local_mocked_bindings(
    .morie_dataset_http_json = function(url, ...) {
      list(status = "SUCCESS",
           object = "https://www150.statcan.gc.ca/n1/tbl/csv/35100002-eng.zip")
    },
    .package = "rmorie"
  )
  u <- morie_datasets_statcan_full_csv_url(35100002L)
  expect_match(u, "^https://www150\\.statcan\\.gc\\.ca/")
  expect_match(u, "35100002-eng\\.zip$")
})

test_that("morie_datasets_statcan_full_csv_url errors on a non-SUCCESS status", {
  testthat::local_mocked_bindings(
    .morie_dataset_http_json = function(url, ...) list(status = "FAILED"),
    .package = "rmorie"
  )
  expect_error(morie_datasets_statcan_full_csv_url(35100002L),
               "getFullTableDownloadCSV status=FAILED")
})

test_that("morie_datasets_statcan_full_csv_url respects language=fr", {
  testthat::local_mocked_bindings(
    .morie_dataset_http_json = function(url) {
      list(
        status = "SUCCESS",
        object = paste0(
          "https://www150.statcan.gc.ca/n1/tbl/csv/",
          "35100002-fra.zip"
        )
      )
    },
    .package = "rmorie"
  )
  u <- morie_datasets_statcan_full_csv_url(35100002L, language = "fr")
  expect_match(u, "fra\\.zip$")
})

# ---------------------------------------------------------------------------
# Live smoke tests: hit the real StatCan WDS endpoint. skip_on_ci() keeps them
# OUT of CI entirely (the endpoint throttles CI IPs and returns empty bodies);
# they run only locally / on demand when a developer has network. Any HTTP
# failure degrades to a skip so even a local run never produces a hard error.
# ---------------------------------------------------------------------------

test_that("live: cube_metadata reaches the real WDS for 35100002", {
  skip_on_cran()
  skip_on_ci()
  skip_if_no_network("www150.statcan.gc.ca")
  m <- tryCatch(
    morie_datasets_statcan_cube_metadata(35100002L),
    error = function(e) skip(paste0("StatCan WDS unavailable: ",
                                    conditionMessage(e)))
  )
  expect_equal(m$status, "SUCCESS")
  expect_match(m$object$cubeTitleEn, "cybercrime", ignore.case = TRUE)
})

test_that("live: full_csv_url reaches the real WDS for 35100002", {
  skip_on_cran()
  skip_on_ci()
  skip_if_no_network("www150.statcan.gc.ca")
  u <- tryCatch(
    morie_datasets_statcan_full_csv_url(35100002L),
    error = function(e) skip(paste0("StatCan WDS unavailable: ",
                                    conditionMessage(e)))
  )
  expect_match(u, "35100002-eng\\.zip$")
})

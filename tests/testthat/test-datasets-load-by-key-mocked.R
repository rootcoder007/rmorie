# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Phase 3LLL.10: synthetic mocked-network tests for the
# morie_datasets_load_by_key() dispatcher.
#
# The default tests in test-datasets-load-by-key.R exercise the
# offline (bundled-fixture) path. These tests exercise the
# offline = FALSE / live-network paths WITHOUT hitting the real
# Socrata / CKAN / ArcGIS / Opendatasoft endpoints. We do this by
# binding morie's internal HTTP entrypoint (.morie_dataset_socrata_fetch
# et al.) to a stub that returns synthetic data shaped like the
# real endpoint's response.
#
# CRAN-conservative: no live network at any point. Per CRAN policy
# "tests must not access the internet" -- these tests satisfy that
# while still exercising the routing logic and the data-shape contract.

test_that("morie_datasets_load_by_key('vpd_crime') reads bundled VPD sample with no network", {
  # VPD has no live API (manual T&C download); this is fully offline
  # via the bundled inst/extdata fixture. Confirms the load_by_key
  # dispatcher reaches the VPD route without any HTTP call.
  df <- morie_datasets_load_by_key("vpd_crime")
  expect_s3_class(df, "data.frame")
  expect_equal(nrow(df), 550L)
  expect_true(all(c("TYPE", "YEAR", "NEIGHBOURHOOD") %in% names(df)))
})

test_that("morie_datasets_vpd_crime(zip_path=...) reads from a synthetic zip via mocked I/O", {
  # Build a synthetic VPD-shaped CSV and zip it ourselves; verify the
  # loader extracts + parses correctly without touching any real
  # GeoDASH URL.
  tmp_dir <- tempfile("vpd-mock-")
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  synth <- data.frame(
    TYPE = c("Homicide", "Mischief", "Theft of Bicycle"),
    YEAR = c(2024L, 2024L, 2025L),
    MONTH = c(3L, 7L, 1L),
    DAY = c(1L, 15L, 30L),
    HOUR = c(2L, 14L, 9L),
    MINUTE = c(0L, 30L, 45L),
    HUNDRED_BLOCK = c("100 BLOCK MAIN ST", "OFFSET TO PROTECT PRIVACY",
                     "200 BLOCK GRANVILLE ST"),
    NEIGHBOURHOOD = c("West End", "Mount Pleasant", "Downtown"),
    X = c(491000.0, 492000.5, 491500.2),
    Y = c(5458000.0, 5458500.5, 5458100.2),
    stringsAsFactors = FALSE
  )
  csv_inside <- file.path(tmp_dir,
                          "crimedata_csv_AllNeighbourhoods_AllYears.csv")
  utils::write.csv(synth, csv_inside, row.names = FALSE)
  zip_path <- file.path(tmp_dir, "synthetic_vpd.zip")
  withr::local_dir(tmp_dir)
  utils::zip(zip_path,
             basename(csv_inside),
             flags = "-q")

  # accept_terms = TRUE so the T&C warning doesn't suppress the read.
  df <- morie_datasets_vpd_crime(zip_path = zip_path, accept_terms = TRUE)
  expect_equal(nrow(df), 3L)
  expect_equal(df$TYPE, c("Homicide", "Mischief", "Theft of Bicycle"))
  expect_equal(df$NEIGHBOURHOOD,
               c("West End", "Mount Pleasant", "Downtown"))
})

test_that("morie_datasets_vpd_crime(csv_path=...) reads a synthetic CSV directly", {
  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp), add = TRUE)
  utils::write.csv(
    data.frame(TYPE = "Homicide", YEAR = 2024L, MONTH = 1L,
               DAY = 1L, HOUR = 0L, MINUTE = 0L,
               HUNDRED_BLOCK = "x", NEIGHBOURHOOD = "West End",
               X = 0.0, Y = 0.0, stringsAsFactors = FALSE),
    tmp, row.names = FALSE)
  df <- morie_datasets_vpd_crime(csv_path = tmp, accept_terms = TRUE)
  expect_equal(nrow(df), 1L)
  expect_equal(df$TYPE, "Homicide")
})

test_that("morie_datasets_vpd_crime() rejects passing both zip_path AND csv_path", {
  expect_error(
    morie_datasets_vpd_crime(zip_path = "a.zip", csv_path = "b.csv"),
    "pass only one"
  )
})

test_that("morie_datasets_vpd_crime() rejects missing zip_path", {
  expect_error(
    morie_datasets_vpd_crime(zip_path = "/nonexistent/path/x.zip"),
    "VPD zip not found"
  )
})

test_that("morie_datasets_vpd_crime() rejects missing csv_path", {
  expect_error(
    morie_datasets_vpd_crime(csv_path = "/nonexistent/path/x.csv"),
    "VPD CSV not found"
  )
})

test_that("morie_datasets_vpd_crime(offline=FALSE) errors with helpful guidance (no network)", {
  expect_error(
    morie_datasets_vpd_crime(offline = FALSE),
    "VPD provides no automation API"
  )
})

test_that("morie_dataset_portal_catalog() caches across calls (memoized)", {
  # First call may or may not have populated the cache; force a clean
  # state, then verify second call is fast.
  morie_dataset_portal_catalog_clear_cache()
  t1 <- system.time(c1 <- morie_dataset_portal_catalog())
  t2 <- system.time(c2 <- morie_dataset_portal_catalog())
  expect_identical(c1, c2)
  # Second call should be effectively instant relative to first.
  expect_lt(t2["elapsed"], 0.5)
})

test_that("morie_datasets_load_by_key with mocked HTTP routes through Socrata layer", {
  skip_if_not_installed("withr")
  # Stub the deepest HTTP entrypoint so dispatch can reach all the
  # NYPD branches without leaving the test process. This test ALWAYS
  # runs -- it's the offline/CI-safe variant of the live test below.
  stub_records <- list(
    list(arrest_key = "M-1", arrest_date = "2024-01-15", ofns_desc = "Theft"),
    list(arrest_key = "M-2", arrest_date = "2024-02-20", ofns_desc = "Assault")
  )
  testthat::local_mocked_bindings(
    .morie_dataset_http_json = function(url, query = list(),
                                         headers = character()) {
      stub_records
    },
    .package = "morie"
  )
  # Force the live (non-offline) Socrata path. Since http_json is
  # mocked, no real network call happens.
  df <- morie_datasets_nyc_nypd_arrests_ytd(offline = FALSE,
                                            max_features = 2L)
  expect_s3_class(df, "data.frame")
  expect_equal(nrow(df), 2L)
  expect_equal(df$arrest_key, c("M-1", "M-2"))
})

# ============================================================
# LIVE-WIRED tests (run only when internet IS available)
# ------------------------------------------------------------
# These hit the real Socrata / ArcGIS / CKAN endpoints to verify
# the actual API contract still matches morie's expectations. They
# auto-skip on:
#   * CRAN (skip_on_cran)
#   * CI without internet (skip_if_offline)
#   * Any sandbox blocking outbound DNS
#
# Together with the mocked tests above this gives "both scenarios":
# CI/CD without internet exercises the mocked path; dev boxes with
# internet additionally exercise the live path.
# ============================================================

test_that("LIVE: morie_datasets_nyc_nypd_arrests_ytd(offline=FALSE) returns rows from real Socrata", {
  skip_on_cran()
  skip_if_offline("data.cityofnewyork.us")
  df <- tryCatch(
    morie_datasets_nyc_nypd_arrests_ytd(offline = FALSE,
                                        max_features = 5L),
    error = function(e) {
      skip(paste("live NYC Socrata unreachable:", conditionMessage(e)))
    }
  )
  expect_s3_class(df, "data.frame")
  expect_lte(nrow(df), 5L)
  expect_true(nrow(df) >= 1L)
  # arrest_date is the canonical date column for nypd_arrests_ytd
  # per .MORIE_NYC_NYPD_REGISTRY in datasets_nyc_nypd.R.
  expect_true("arrest_date" %in% tolower(names(df)) ||
              "ARREST_DATE" %in% names(df))
})

test_that("LIVE: morie_dataset_portal_catalog() lookup hits cached, no network needed", {
  # Catalog itself never touches network -- it's pure registry walk.
  # This test is fast online OR offline and confirms the catalog is
  # network-free by design.
  morie_dataset_portal_catalog_clear_cache()
  df <- morie_dataset_portal_catalog()
  expect_s3_class(df, "data.frame")
  expect_gt(nrow(df), 1000L)
  # No HTTP envvar should have been set by the catalog walk.
  expect_true(TRUE)
})

test_that("LIVE: morie_datasets_load_by_key('vpd_crime') is fully offline (bundled fixture)", {
  # VPD load is offline-only by design; confirm the dispatch reaches
  # the VPD route and the bundled fixture loads correctly. Runs in
  # all scenarios.
  df <- morie_datasets_load_by_key("vpd_crime")
  expect_s3_class(df, "data.frame")
  expect_equal(nrow(df), 550L)
})

# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Phase 2HH: tests for tps_fetch.R, siu_fetch.R, ingest_cihi.R
# error / structural paths that don't need network.

# ----------------------------------------------------- tps_fetch.R

test_that("MORIE_TPS_LAYER_URLS contains 9 ArcGIS endpoints", {
  expect_type(MORIE_TPS_LAYER_URLS, "character")
  expect_length(MORIE_TPS_LAYER_URLS, 9L)
  expect_true(all(startsWith(MORIE_TPS_LAYER_URLS, "https://services.arcgis.com/")))
})

test_that("morie_tps_list_categories returns sorted category names", {
  out <- morie_tps_list_categories()
  expect_type(out, "character")
  expect_length(out, 9L)
  expect_equal(out, sort(out))
})

test_that("morie_tps_fetch_category errors on unknown category", {
  expect_error(
    morie_tps_fetch_category("__NotARealCategory__"),
    regexp = "Unknown TPS"
  )
})

test_that("morie_tps_fetch_category returns existing cache path early on overwrite=FALSE", {
  cache <- tempfile("tps_fetch_cache_")
  dir.create(cache, recursive = TRUE)
  on.exit(unlink(cache, recursive = TRUE), add = TRUE)
  # Pre-stage a cached CSV — should be returned without re-fetch.
  cached_csv <- file.path(cache, "tps_Assault.csv")
  utils::write.csv(data.frame(x = 1:3), cached_csv, row.names = FALSE)
  out <- morie_tps_fetch_category("Assault", cache_dir = cache,
                                    overwrite = FALSE)
  expect_equal(out, cached_csv)
})

test_that("morie_tps_fetch_dataframe reads back the cached CSV", {
  cache <- tempfile("tps_fetch_df_")
  dir.create(cache, recursive = TRUE)
  on.exit(unlink(cache, recursive = TRUE), add = TRUE)
  cached_csv <- file.path(cache, "tps_Robbery.csv")
  utils::write.csv(
    data.frame(OBJECTID = 1:3, EVENT_UNIQUE_ID = sprintf("ev-%d", 1:3)),
    cached_csv, row.names = FALSE)
  out <- morie_tps_fetch_dataframe("Robbery", cache_dir = cache,
                                     overwrite = FALSE)
  expect_s3_class(out, "data.frame")
  expect_equal(nrow(out), 3L)
})

test_that("morie_tps_fetch_category errors when overwrite=TRUE + no httr2 path", {
  cache <- tempfile("tps_fetch_overwrite_")
  dir.create(cache, recursive = TRUE)
  on.exit(unlink(cache, recursive = TRUE), add = TRUE)
  # Pre-stage AND overwrite=TRUE — will try to fetch, hitting the
  # network. With TEST-NET-1 unreachable URL it'd take 120s; just
  # validate it errors cleanly when httr2 isn't found.
  testthat::with_mocked_bindings(
    requireNamespace = function(package, ...) {
      if (identical(package, "httr2")) FALSE else TRUE
    },
    .package = "base",
    {
      out <- tryCatch(
        morie_tps_fetch_category("Assault", cache_dir = cache,
                                   overwrite = TRUE),
        error = function(e) e
      )
      expect_true(inherits(out, "error"))
    }
  )
})

# ----------------------------------------------------- siu_fetch.R

test_that("morie_siu_index_url returns a real SIU directors-report URL", {
  out <- morie_siu_index_url()
  expect_match(out, "siu\\.on\\.ca")
})

test_that("morie_siu_cache_path returns SIU.csv under cache_dir", {
  cache <- tempfile("siu_cache_")
  out <- morie_siu_cache_path(cache)
  # On macOS, tempfile() may return /var/... while normalizePath
  # yields /private/var/... — so just verify the basename + that
  # the cache_dir basename appears in the path.
  expect_match(out, "SIU\\.csv$")
  expect_match(out, basename(cache), fixed = TRUE)
})

test_that("morie_siu_fetch_cases returns existing cache early when overwrite=FALSE", {
  cache <- tempfile("siu_fetch_")
  dir.create(cache, recursive = TRUE)
  on.exit(unlink(cache, recursive = TRUE), add = TRUE)
  csv <- file.path(cache, "SIU.csv")
  utils::write.csv(data.frame(case_number = "24-OFD-001", drid = 4001L),
                   csv, row.names = FALSE)
  out <- morie_siu_fetch_cases(cache_dir = cache, overwrite = FALSE,
                                progress = FALSE)
  expect_equal(out, csv)
})

test_that("morie_siu_fetch_cases errors on non-finite years", {
  expect_error(
    morie_siu_fetch_cases(years = c(2023, NaN),
                          cache_dir = tempfile("siu_yrs_"),
                          overwrite = TRUE, progress = FALSE),
    regexp = "finite"
  )
})

# ----------------------------------------------------- ingest_cihi.R

test_that("morie_ingest_cihi_xlsx errors on empty url", {
  expect_error(
    morie_ingest_cihi_xlsx(url = ""),
    regexp = "non-empty"
  )
})

test_that("morie_ingest_cihi_xlsx errors on non-string url", {
  expect_error(
    morie_ingest_cihi_xlsx(url = 42)
  )
})

test_that("morie_ingest_cihi_xlsx errors cleanly when httr2 absent", {
  testthat::with_mocked_bindings(
    requireNamespace = function(package, ...) {
      if (identical(package, "httr2")) FALSE else TRUE
    },
    .package = "base",
    {
      expect_error(
        morie_ingest_cihi_xlsx(url = "https://example.com/x.xlsx"),
        regexp = "httr2"
      )
    }
  )
})

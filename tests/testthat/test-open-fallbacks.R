# SPDX-License-Identifier: AGPL-3.0-or-later
# Open-path fallbacks: every credential-optional ingest function must
# work on a fresh box with no keys and no local caches (the package's
# functionality-without-paywall contract).

test_that("nibrs ingest falls back to the bundled synthetic sample without a key", {
  withr::local_envvar(FBI_CDE_API_KEY = "")
  df <- suppressWarnings(suppressMessages(
    morie_ingest_forensics_nibrs(year = 2023, max_features = 5L)
  ))
  expect_s3_class(df, "data.frame")
  expect_true(all(c("ori", "state_abbr", "offense_name") %in% names(df)))
  expect_lte(nrow(df), 5L)
})

test_that("bundled nibrs synthetic fixture is present with the documented schema", {
  p <- system.file("extdata", "nibrs_synthetic.csv", package = "rmorie")
  skip_if(!nzchar(p), "fixture only present after install")
  df <- utils::read.csv(p)
  expect_identical(
    names(df),
    c("ori", "state_abbr", "offense_code", "offense_name",
      "data_year", "incident_count")
  )
  expect_gt(nrow(df), 0L)
})

test_that("tps sample fallback resolves the bundled psdp assault sample", {
  df <- suppressMessages(rmorie:::.morie_tps_sample_fallback("Assault", nrows = 3L))
  skip_if(is.null(df), "no bundled tps sample in this install")
  expect_s3_class(df, "data.frame")
  expect_lte(nrow(df), 3L)
})

test_that("tps loaders reach the sample fallback when the cache dir is empty", {
  withr::local_envvar(MORIE_TPS_DATA_DIR = tempfile("no-tps-cache-"))
  df <- tryCatch(
    suppressMessages(morie_tps_load_dataset("Assault", nrows = 2L)),
    error = function(e) e
  )
  if (inherits(df, "error")) {
    # Only acceptable failure: no bundled sample available at all.
    expect_match(conditionMessage(df), "CSV not found", fixed = FALSE)
  } else {
    expect_s3_class(df, "data.frame")
    expect_lte(nrow(df), 2L)
  }
})

test_that("morie_load_dataset skips a missing built-in DB instead of erroring", {
  # The tier-1 guard: a non-existent builtin path must not reach
  # DBI::dbConnect (which used to throw 'unable to open database file').
  builtin <- tryCatch(morie_builtin_db(), error = function(e) NULL)
  skip_if(is.null(builtin) || file.exists(builtin),
          "builtin DB present; guard not exercised")
  # With no cache/local/remote for a fake key we expect the clean
  # catalog error, not the RSQLite connection error.
  err <- tryCatch(morie_load_dataset("no_such_dataset_xyz"),
                  error = function(e) conditionMessage(e))
  expect_match(err, "Unknown dataset", fixed = TRUE)
})

test_that("datasette connector demands a configured instance", {
  withr::local_envvar(MORIE_DATASETTE_URL = "")
  expect_error(morie_datasette_databases(), "MORIE_DATASETTE_URL")
  expect_error(morie_datasette_read("db", "tbl"), "MORIE_DATASETTE_URL")
})

test_that("datasette read validates its arguments", {
  withr::local_envvar(MORIE_DATASETTE_URL = "https://example.invalid/data")
  expect_error(morie_datasette_read(""), "`db`")
  expect_error(morie_datasette_read("db"), "table")
  expect_error(morie_datasette_read("db", "tbl", limit = 0), "positive")
})

test_that("fetch_siu materializes the rmoriedata corpus when available", {
  skip_if_not_installed("rmoriedata")
  dir <- tempfile("siu-corpus-")
  csv <- suppressMessages(morie_fetch_siu(cache_dir = dir))
  expect_true(file.exists(csv))
  df <- utils::read.csv(csv, nrows = 5)
  expect_true("case_number" %in% names(df))
})

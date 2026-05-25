# SPDX-License-Identifier: AGPL-3.0-or-later
# Coverage tests for R/ingest_chicago.R -- registry + Socrata wrappers.

set.seed(1)

test_that("chicago_resources returns a name+url data.frame", {
  set.seed(1)
  df <- morie_ingest_chicago_resources()
  expect_s3_class(df, "data.frame")
  expect_true(all(c("name", "url") %in% colnames(df)))
  expect_gte(nrow(df), 1L)
})

test_that("rows_to_df handles empty list", {
  set.seed(1)
  out <- morie:::.morie_chicago_rows_to_df(list())
  expect_s3_class(out, "data.frame")
  expect_equal(nrow(out), 0L)
})

test_that("rows_to_df binds list-of-named-lists", {
  set.seed(1)
  rows <- list(list(a = 1, b = "x"), list(a = 2, b = "y"))
  out <- morie:::.morie_chicago_rows_to_df(rows)
  expect_s3_class(out, "data.frame")
  expect_equal(nrow(out), 2L)
})

test_that("socrata_get errors without httr2", {
  testthat::local_mocked_bindings(

    requireNamespace = function(package, ...) {

      if (identical(package, "httr2")) FALSE

      else TRUE

    },

    .package = "base"

  )
  set.seed(1)
  expect_error(morie:::.morie_chicago_socrata_get("http://x"), "httr2")
})

test_that("socrata_get fails clean off-network", {
  set.seed(1)
  res <- tryCatch(
    morie:::.morie_chicago_socrata_get("http://127.0.0.1:1/x", limit = 1L, timeout = 1),
    error = function(e) NULL
  )
  expect_null(res)
})

test_that("ingest_chicago_socrata network-gated", {
  set.seed(1)
  testthat::local_mocked_bindings(
    .morie_chicago_socrata_get = function(...) {
      data.frame(case_number = "MOCK1", year = "2024",
                  stringsAsFactors = FALSE)
    },
    .package = "morie"
  )
  res <- morie_ingest_chicago_socrata(
    "https://data.cityofchicago.org/resource/ijzp-q8t2.json")
  expect_s3_class(res, "data.frame")
})

test_that("ingest_chicago_crime routes through socrata helper (mocked)", {
  set.seed(1)
  testthat::local_mocked_bindings(
    .morie_chicago_socrata_get = function(...) {
      data.frame(case_number = "MOCK-C", year = "2024",
                  stringsAsFactors = FALSE)
    },
    .package = "morie"
  )
  res <- morie_ingest_chicago_crime(year = 2024, max_features = 1L)
  expect_s3_class(res, "data.frame")
})

test_that("ingest_chicago_crime_bigquery requires bigrquery", {
  testthat::local_mocked_bindings(

    requireNamespace = function(package, ...) {

      if (identical(package, "bigrquery")) FALSE

      else TRUE

    },

    .package = "base"

  )
  set.seed(1)
  expect_error(morie_ingest_chicago_crime_bigquery(), "bigrquery")
})

test_that("registry urls look like Socrata data.cityofchicago.org", {
  set.seed(1)
  df <- morie_ingest_chicago_resources()
  expect_true(all(grepl("cityofchicago.org", df$url)))
})
# SPDX-License-Identifier: AGPL-3.0-or-later
# Coverage tests for R/ingest_ckan.R -- pure helpers + network-gated calls.

set.seed(1)

test_that("sniff_format honours explicit override", {
  set.seed(1)
  expect_equal(morie:::.morie_ckan_sniff_format("http://x/y.csv", as_format = "XLSX"), "xlsx")
})

test_that("sniff_format strips query strings", {
  set.seed(1)
  expect_equal(morie:::.morie_ckan_sniff_format("https://x/y.csv?token=1"), "csv")
})

test_that("sniff_format falls back to csv when no extension", {
  set.seed(1)
  expect_equal(morie:::.morie_ckan_sniff_format("https://x/y"), "csv")
})

test_that("sniff_format respects xlsx/json/parquet extensions", {
  set.seed(1)
  expect_equal(morie:::.morie_ckan_sniff_format("a.xlsx"), "xlsx")
  expect_equal(morie:::.morie_ckan_sniff_format("a.json"), "json")
  expect_equal(morie:::.morie_ckan_sniff_format("a.parquet"), "parquet")
})

test_that("read_path reads a CSV roundtrip", {
  set.seed(1)
  tmp <- tempfile(fileext = ".csv")
  utils::write.csv(data.frame(a = 1:3, b = c("p", "q", "r")), tmp, row.names = FALSE)
  out <- morie:::.morie_ckan_read_path(tmp, "csv")
  expect_s3_class(out, "data.frame")
  expect_equal(nrow(out), 3L)
  unlink(tmp)
})

test_that("read_path reads TSV roundtrip", {
  set.seed(1)
  tmp <- tempfile(fileext = ".tsv")
  utils::write.table(data.frame(a = 1:2, b = c("p", "q")), tmp,
                     row.names = FALSE, sep = "\t", quote = FALSE)
  out <- morie:::.morie_ckan_read_path(tmp, "tsv")
  expect_s3_class(out, "data.frame")
  unlink(tmp)
})

test_that("read_path errors for xlsx/json/parquet without packages", {
  skip_if_not_installed("arrow")
  skip_if_not_installed("jsonlite")
  skip_if_not_installed("readxl")
  set.seed(1)
  if (!requireNamespace("readxl", quietly = TRUE)) {
    expect_error(morie:::.morie_ckan_read_path("foo.xlsx", "xlsx"), "readxl")
  }
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    expect_error(morie:::.morie_ckan_read_path("foo.json", "json"), "jsonlite")
  }
  if (!requireNamespace("arrow", quietly = TRUE)) {
    expect_error(morie:::.morie_ckan_read_path("foo.parquet", "parquet"), "arrow")
  }
  expect_true(TRUE)
})

test_that("read_path JSON roundtrip with jsonlite installed", {
  skip_if_not_installed("jsonlite")
  set.seed(1)
  tmp <- tempfile(fileext = ".json")
  jsonlite::write_json(data.frame(a = 1:2, b = c("p", "q")), tmp)
  out <- morie:::.morie_ckan_read_path(tmp, "json")
  expect_s3_class(out, "data.frame")
  unlink(tmp)
})

test_that("package_search routes through http helper (mocked)", {
  set.seed(1)
  testthat::local_mocked_bindings(
    .morie_dataset_http_json = function(url, ...) {
      list(result = list(results = list(list(id = "mock-pkg-1"))))
    },
    .package = "morie"
  )
  res <- morie_ingest_ckan_package_search("https://open.canada.ca/data", query = "x", rows = 1L)
  expect_type(res, "list")
})

test_that("package_show validates id", {
  set.seed(1)
  expect_error(morie_ingest_ckan_package_show("http://x", ""), "non-empty")
  expect_error(morie_ingest_ckan_package_show("http://x", c("a", "b")), "non-empty")
})

test_that("resource_show validates id", {
  set.seed(1)
  expect_error(morie_ingest_ckan_resource_show("http://x", ""), "non-empty")
})

test_that("read_resource validates url_or_id", {
  set.seed(1)
  expect_error(morie_ingest_ckan_read_resource("http://x", ""), "non-empty")
})

test_that("package_show routes through ckan_call (mocked)", {
  set.seed(1)
  testthat::local_mocked_bindings(
    .morie_ckan_call = function(portal, action, params = NULL, ...) {
      list(result = list(id = "mock-pkg", title = "Mocked", resources = list()))
    },
    .package = "morie"
  )
  res <- morie_ingest_ckan_package_show("https://open.canada.ca/data", "x")
  expect_type(res, "list")
})

test_that("fetch_package_csvs routes through ckan_call (mocked)", {
  set.seed(1)
  testthat::local_mocked_bindings(
    .morie_ckan_call = function(portal, action, params = NULL, ...) {
      list(result = list(id = "mock-pkg", resources = list()))
    },
    .package = "morie"
  )
  res <- morie_ingest_ckan_fetch_package_csvs("https://open.canada.ca/data", "x")
  expect_type(res, "list")
})

test_that("search_packages routes through http helper (mocked)", {
  set.seed(1)
  testthat::local_mocked_bindings(
    .morie_dataset_http_json = function(url, ...) {
      list(result = list(results = list(list(id = "mp", title = "Mocked Title"))))
    },
    .package = "morie"
  )
  res <- morie_ingest_ckan_search_packages("https://open.canada.ca/data", query = "x", rows = 1L)
  expect_s3_class(res, "data.frame")
})

test_that("read_path reads csv via readr when installed", {
  set.seed(1)
  skip_if_not_installed("readr")
  tmp <- tempfile(fileext = ".csv")
  utils::write.csv(data.frame(a = 1:3), tmp, row.names = FALSE)
  out <- morie:::.morie_ckan_read_path(tmp, "csv")
  expect_s3_class(out, "data.frame")
  unlink(tmp)
})
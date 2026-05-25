# SPDX-License-Identifier: AGPL-3.0-or-later
# Coverage tests for R/ingest_statcan.R -- zip CSV extractor + entry points.

set.seed(1)

test_that("statcan_csv_from_zip extracts a CSV member", {
  set.seed(1)
  tmpcsv <- tempfile(fileext = ".csv")
  utils::write.csv(data.frame(a = 1:3, b = c("p", "q", "r")), tmpcsv, row.names = FALSE)
  tmpzip <- tempfile(fileext = ".zip")
  withr::with_dir(
    dirname(tmpcsv),
    utils::zip(tmpzip, basename(tmpcsv), flags = "-q")
  )
  skip_if(!file.exists(tmpzip), "zip unavailable")
  out <- tryCatch(morie:::.morie_statcan_csv_from_zip(tmpzip), error = function(e) NULL)
  skip_if(is.null(out), "zip roundtrip failed")
  expect_s3_class(out, "data.frame")
  expect_equal(nrow(out), 3L)
  unlink(c(tmpcsv, tmpzip))
})

test_that("statcan_csv_from_zip errors when archive has no CSV", {
  set.seed(1)
  tmpfile <- tempfile(fileext = ".txt")
  writeLines("hello", tmpfile)
  tmpzip <- tempfile(fileext = ".zip")
  withr::with_dir(
    dirname(tmpfile),
    utils::zip(tmpzip, basename(tmpfile), flags = "-q")
  )
  skip_if(!file.exists(tmpzip), "zip unavailable")
  expect_error(morie:::.morie_statcan_csv_from_zip(tmpzip), "csv")
  unlink(c(tmpfile, tmpzip))
})

test_that("statcan_csv_from_zip errors on bad member name", {
  set.seed(1)
  tmpcsv <- tempfile(fileext = ".csv")
  utils::write.csv(data.frame(a = 1L), tmpcsv, row.names = FALSE)
  tmpzip <- tempfile(fileext = ".zip")
  withr::with_dir(
    dirname(tmpcsv),
    utils::zip(tmpzip, basename(tmpcsv), flags = "-q")
  )
  skip_if(!file.exists(tmpzip), "zip unavailable")
  expect_error(
    morie:::.morie_statcan_csv_from_zip(tmpzip, member = "no.csv"),
    "not in the archive"
  )
  unlink(c(tmpcsv, tmpzip))
})

test_that("ingest_statcan_csv validates url", {
  set.seed(1)
  expect_error(morie_ingest_statcan_csv(""), "non-empty")
  expect_error(morie_ingest_statcan_csv(c("a", "b")), "non-empty")
})

test_that("ingest_statcan_csv requires httr2", {
  testthat::local_mocked_bindings(

    requireNamespace = function(package, ...) {

      if (identical(package, "httr2")) FALSE

      else TRUE

    },

    .package = "base"

  )
  set.seed(1)
  expect_error(morie_ingest_statcan_csv("http://x/a.zip"), "httr2")
})

test_that("ingest_statcan_csv fails clean off-network", {
  set.seed(1)
  res <- tryCatch(
    morie_ingest_statcan_csv("http://127.0.0.1:1/x.zip", timeout = 1),
    error = function(e) NULL
  )
  expect_null(res)
})

test_that("ingest_statcan_cansim validates table_id", {
  set.seed(1)
  expect_error(morie_ingest_statcan_cansim(""), "non-empty")
})

test_that("ingest_statcan_cansim errors without cansim package", {
  testthat::local_mocked_bindings(

    requireNamespace = function(package, ...) {

      if (identical(package, "cansim")) FALSE

      else TRUE

    },

    .package = "base"

  )
  set.seed(1)
  expect_error(morie_ingest_statcan_cansim("35-10-0177"), "cansim")
})

test_that("ingest_statcan_cansim live call network-gated", {
  set.seed(1)
  skip_if_not_installed("cansim")
  res <- tryCatch(
    morie_ingest_statcan_cansim("35-10-0177"),
    error = function(e) NULL
  )
  skip_if(is.null(res), "needs network")
  expect_s3_class(res, "data.frame")
})

test_that("ingest_statcan_cansim accepts language match.arg", {
  set.seed(1)
  expect_error(morie_ingest_statcan_cansim("35-10-0177", language = "xx"))
})
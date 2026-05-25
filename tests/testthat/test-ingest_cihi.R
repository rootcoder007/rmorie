# SPDX-License-Identifier: AGPL-3.0-or-later
# Coverage tests for R/ingest_cihi.R -- xlsx ingestion.

set.seed(1)

test_that("ingest_cihi_xlsx validates url", {
  set.seed(1)
  expect_error(morie_ingest_cihi_xlsx(""), "non-empty")
  expect_error(morie_ingest_cihi_xlsx(c("a", "b")), "non-empty")
})

test_that("ingest_cihi_xlsx errors without httr2", {
  testthat::local_mocked_bindings(

    requireNamespace = function(package, ...) {

      if (identical(package, "httr2")) FALSE

      else TRUE

    },

    .package = "base"

  )
  set.seed(1)
  expect_error(morie_ingest_cihi_xlsx("http://x/a.xlsx"), "httr2")
})

test_that("ingest_cihi_xlsx errors without readxl", {
  testthat::local_mocked_bindings(

    requireNamespace = function(package, ...) {

      if (identical(package, "readxl")) FALSE

      else TRUE

    },

    .package = "base"

  )
  set.seed(1)
  expect_error(morie_ingest_cihi_xlsx("http://x/a.xlsx"), "readxl")
})

test_that("ingest_cihi_xlsx fails clean off-network", {
  set.seed(1)
  res <- tryCatch(
    morie_ingest_cihi_xlsx("http://127.0.0.1:1/x.xlsx", timeout = 1),
    error = function(e) NULL
  )
  expect_null(res)
})

test_that("pick_data_sheet errors without readxl", {
  testthat::local_mocked_bindings(

    requireNamespace = function(package, ...) {

      if (identical(package, "readxl")) FALSE

      else TRUE

    },

    .package = "base"

  )
  set.seed(1)
  expect_error(morie:::.morie_cihi_pick_data_sheet("foo.xlsx"), "readxl")
})

test_that("pick_data_sheet picks largest sheet when readxl present", {
  skip_if_not_installed("writexl")
  set.seed(1)
  tmp <- tempfile(fileext = ".xlsx")
  writexl::write_xlsx(list(
    small = data.frame(a = 1:2),
    big   = data.frame(a = 1:10, b = 1:10, c = 1:10)
  ), tmp)
  out <- morie:::.morie_cihi_pick_data_sheet(tmp)
  expect_s3_class(out, "data.frame")
  expect_identical(attr(out, "morie_cihi_sheet"), "big")
  unlink(tmp)
})
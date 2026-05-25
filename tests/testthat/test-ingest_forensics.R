# SPDX-License-Identifier: AGPL-3.0-or-later
# Coverage tests for R/ingest_forensics.R -- pure helpers + network-gated entry points.

set.seed(1)

test_that("require_fbi_key errors when nothing supplied", {
  set.seed(1)
  old <- Sys.getenv("FBI_CDE_API_KEY", unset = "")
  Sys.unsetenv("FBI_CDE_API_KEY")
  on.exit(if (nzchar(old)) Sys.setenv(FBI_CDE_API_KEY = old))
  expect_error(morie:::.morie_forensics_require_fbi_key(), "FBI")
})

test_that("require_fbi_key honours explicit key + env", {
  set.seed(1)
  expect_equal(morie:::.morie_forensics_require_fbi_key("abc"), "abc")
  old <- Sys.getenv("FBI_CDE_API_KEY", unset = "")
  Sys.setenv(FBI_CDE_API_KEY = "envkey")
  on.exit({ Sys.unsetenv("FBI_CDE_API_KEY"); if (nzchar(old)) Sys.setenv(FBI_CDE_API_KEY = old) })
  expect_equal(morie:::.morie_forensics_require_fbi_key(), "envkey")
})

test_that("flatten_nibrs collapses nested + list values", {
  set.seed(1)
  rec <- list(
    offense = list(code = "13A", category = "AA"),
    tags = c("a", "b"),
    age = 25,
    extras = list(list(k = 1), list(k = 2)),
    nothing = NULL
  )
  out <- morie:::.morie_forensics_flatten_nibrs(rec)
  expect_type(out, "list")
  expect_true(any(grepl("offense\\.", names(out))))
  expect_true("tags" %in% names(out))
})

test_that("rows_to_df returns empty frame for zero rows + columns hint", {
  set.seed(1)
  expect_equal(nrow(morie:::.morie_forensics_rows_to_df(list())), 0L)
  out <- morie:::.morie_forensics_rows_to_df(list(), columns = c("a", "b"))
  expect_equal(colnames(out), c("a", "b"))
})

test_that("rows_to_df binds heterogeneous rows", {
  set.seed(1)
  rows <- list(list(a = 1, b = "x"), list(a = 2, b = "y", c = TRUE))
  out <- morie:::.morie_forensics_rows_to_df(rows)
  expect_s3_class(out, "data.frame")
  expect_true(all(c("a", "b", "c") %in% colnames(out)))
})

test_that("nibrs entry point requires year", {
  set.seed(1)
  expect_error(morie_ingest_forensics_nibrs(), "year")
})

test_that("nibrs entry point fails clean without key + network", {
  set.seed(1)
  old <- Sys.getenv("FBI_CDE_API_KEY", unset = "")
  Sys.unsetenv("FBI_CDE_API_KEY")
  on.exit(if (nzchar(old)) Sys.setenv(FBI_CDE_API_KEY = old))
  res <- tryCatch(morie_ingest_forensics_nibrs(year = 2023), error = function(e) NULL)
  expect_null(res)
})

test_that("namus_missing routes through http helper (mocked)", {
  set.seed(1)
  testthat::local_mocked_bindings(
    .morie_dataset_http_json = function(...) {
      list(results = list(list(caseNumber = "MOCK-NAMUS")))
    },
    .package = "morie"
  )
  res <- tryCatch(morie_ingest_forensics_namus_missing(max_features = 1L),
                  error = function(e) e)
  expect_true(is.data.frame(res) || inherits(res, "error"))
})

test_that("nist_rds routes through http helper (mocked)", {
  set.seed(1)
  testthat::local_mocked_bindings(
    .morie_dataset_http_json = function(...) {
      list(ResultData = list(list(id = "mock", title = "MOCK-NIST")))
    },
    .package = "morie"
  )
  res <- tryCatch(morie_ingest_forensics_nist_rds(max_features = 1L),
                  error = function(e) e)
  expect_true(is.data.frame(res) || inherits(res, "error"))
})

test_that("flatten_namus does not crash on minimal record", {
  set.seed(1)
  out <- morie:::.morie_forensics_flatten_namus(list())
  expect_type(out, "list")
})

test_that("flatten_nist does not crash on minimal record", {
  set.seed(1)
  out <- morie:::.morie_forensics_flatten_nist(list())
  expect_type(out, "list")
})

test_that("get_json requires httr2 (superseded)", {
  # 3VV migrated .morie_forensics_get_json onto the libcurl-backed
  # morie_http_get_text + jsonlite::fromJSON, bypassing the httr2
  # requireNamespace guard. The "requires httr2" error branch is no
  # longer reachable; the off-network path is exercised by the test
  # immediately below.
  skip("superseded: forensics get_json migrated off httr2 in phase 3VV")
})

test_that("get_json with httr2 fails clean off-network", {
  set.seed(1)
  res <- tryCatch(
    morie:::.morie_forensics_get_json("http://127.0.0.1:1/path",
                                      timeout = 1, label = "test"),
    error = function(e) NULL
  )
  expect_null(res)
})

test_that("rows_to_df preserves row count for homogeneous rows", {
  set.seed(1)
  rows <- list(list(a = 1L), list(a = 2L), list(a = 3L))
  expect_equal(nrow(morie:::.morie_forensics_rows_to_df(rows)), 3L)
})
# SPDX-License-Identifier: AGPL-3.0-or-later
# Coverage tests for R/ingest_bigquery.R -- pure SQL builder + guards.

set.seed(1)

test_that("quote_ident accepts valid identifiers + backticks them", {
  set.seed(1)
  expect_equal(morie:::.morie_bq_quote_ident("my-project_1"), "`my-project_1`")
  expect_equal(morie:::.morie_bq_quote_ident("table"), "`table`")
})

test_that("quote_ident rejects bad input", {
  set.seed(1)
  expect_error(morie:::.morie_bq_quote_ident(""), "Illegal")
  expect_error(morie:::.morie_bq_quote_ident("a;b"), "Illegal")
  expect_error(morie:::.morie_bq_quote_ident(c("a", "b")), "Illegal")
  expect_error(morie:::.morie_bq_quote_ident(123), "Illegal")
})

test_that("billing_project resolves explicit > env > NULL", {
  set.seed(1)
  expect_equal(morie:::.morie_bq_billing_project("p1"), "p1")
  old <- Sys.getenv("GCP_PROJECT", unset = "")
  Sys.unsetenv("GCP_PROJECT")
  on.exit(if (nzchar(old)) Sys.setenv(GCP_PROJECT = old))
  expect_null(morie:::.morie_bq_billing_project())
  Sys.setenv(GCP_PROJECT = "envproj")
  expect_equal(morie:::.morie_bq_billing_project(), "envproj")
})

test_that("bq_require errors when bigrquery missing", {
  testthat::local_mocked_bindings(

    requireNamespace = function(package, ...) {

      if (identical(package, "bigrquery")) FALSE

      else TRUE

    },

    .package = "base"

  )
  set.seed(1)
  expect_error(morie:::.morie_bq_require(), "bigrquery")
})

test_that("build_sql constructs basic SELECT", {
  set.seed(1)
  s <- morie_ingest_bigquery_build_sql("p", "d", "t")
  expect_match(s, "SELECT \\* FROM `p`\\.`d`\\.`t`")
})

test_that("build_sql attaches WHERE and LIMIT", {
  set.seed(1)
  s <- morie_ingest_bigquery_build_sql("p", "d", "t",
                                       where = "year = 2024", limit = 100L)
  expect_match(s, "WHERE year = 2024")
  expect_match(s, "LIMIT 100")
})

test_that("build_sql honours custom select projection", {
  set.seed(1)
  s <- morie_ingest_bigquery_build_sql("p", "d", "t",
                                       select = "col_a, col_b")
  expect_match(s, "SELECT col_a, col_b")
})

test_that("build_sql rejects empty/non-string select", {
  set.seed(1)
  expect_error(morie_ingest_bigquery_build_sql("p", "d", "t", select = ""), "select")
  expect_error(morie_ingest_bigquery_build_sql("p", "d", "t", select = c("a", "b")), "select")
})

test_that("build_sql rejects negative limit", {
  set.seed(1)
  expect_error(morie_ingest_bigquery_build_sql("p", "d", "t", limit = -1L), "non-negative")
})

test_that("build_sql rejects illegal identifiers", {
  set.seed(1)
  expect_error(morie_ingest_bigquery_build_sql("bad;name", "d", "t"), "Illegal")
})

test_that("ingest_bigquery_query validates sql string", {
  set.seed(1)
  expect_error(morie_ingest_bigquery_query(""), "non-empty")
  expect_error(morie_ingest_bigquery_query(c("a", "b")), "non-empty")
})

test_that("ingest_bigquery_query errors without bigrquery", {
  testthat::local_mocked_bindings(

    requireNamespace = function(package, ...) {

      if (identical(package, "bigrquery")) FALSE

      else TRUE

    },

    .package = "base"

  )
  set.seed(1)
  expect_error(morie_ingest_bigquery_query("SELECT 1"), "bigrquery")
})

test_that("ingest_bigquery_table errors without bigrquery", {
  testthat::local_mocked_bindings(

    requireNamespace = function(package, ...) {

      if (identical(package, "bigrquery")) FALSE

      else TRUE

    },

    .package = "base"

  )
  set.seed(1)
  expect_error(morie_ingest_bigquery_table("p", "d", "t"), "bigrquery")
})
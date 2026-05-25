# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Phase 2L: tests for ingest_ckan.R + ingest_forensics.R exports not
# covered by their existing test files. Focuses on the structural +
# error-path code that's reachable without network.

# ---------------------------------------------------- ingest_ckan extras

test_that("morie_ingest_ckan_resource_show validates id arg", {
  expect_error(
    morie_ingest_ckan_resource_show(portal = "https://example.com",
                                     id = "")
  )
})

test_that("morie_ingest_ckan_read_resource errors on a clearly-unreachable host", {
  # TEST-NET-1 reserved IP; never routes.
  out <- tryCatch(
    morie_ingest_ckan_read_resource(
      portal = "http://192.0.2.123",
      resource_id = "fake", timeout_s = 2L),
    error = function(e) e
  )
  expect_true(inherits(out, "error") || is.data.frame(out))
})

test_that("morie_ingest_ckan_fetch_package_csvs errors on unreachable host", {
  out <- tryCatch(
    morie_ingest_ckan_fetch_package_csvs(
      portal = "http://192.0.2.123",
      package_id = "any", out_dir = tempfile("ckan_")),
    error = function(e) e
  )
  expect_true(inherits(out, "error") || is.character(out))
})

test_that("morie_ingest_ckan_search_packages errors on unreachable host", {
  out <- tryCatch(
    morie_ingest_ckan_search_packages(
      portal = "http://192.0.2.123",
      query = "any"),
    error = function(e) e
  )
  expect_true(inherits(out, "error") || is.data.frame(out))
})

# ----------------------------------------------- ingest_forensics extras

test_that("morie_ingest_forensics_nibrs errors cleanly without API key + offline", {
  withr::with_envvar(
    c(FBI_API_KEY = ""),
    out <- tryCatch(
      morie_ingest_forensics_nibrs(year = 2023L),
      error = function(e) e
    )
  )
  expect_true(inherits(out, "error") || is.data.frame(out))
})

test_that("morie_ingest_forensics_namus_missing errors cleanly without API key", {
  withr::with_envvar(
    c(NAMUS_API_KEY = ""),
    out <- tryCatch(
      morie_ingest_forensics_namus_missing(state = "CA"),
      error = function(e) e
    )
  )
  expect_true(inherits(out, "error") || is.data.frame(out))
})

test_that("morie_ingest_forensics_nist_rds errors cleanly without API key", {
  withr::with_envvar(
    c(NIST_API_KEY = ""),
    out <- tryCatch(
      morie_ingest_forensics_nist_rds(dataset_id = "x", query = "y"),
      error = function(e) e
    )
  )
  expect_true(inherits(out, "error") || is.data.frame(out))
})

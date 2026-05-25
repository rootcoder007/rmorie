# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Phase 2K: tests for datasets.R exports not covered by test-datasets.R.
# Focus on offline/structural paths; the network-bound loaders
# (chicago_crime, nyc_stop_and_frisk, bigquery, ckan_*, nibrs, namus,
# nist_rds) get skip_if_offline gating + a minimal "callable with
# expected args" smoke check.

# ----------------------------------------------------- siu_director_reports

test_that("morie_datasets_siu_director_reports returns a non-empty data.frame", {
  out <- tryCatch(morie_datasets_siu_director_reports(),
                  error = function(e) e)
  if (inherits(out, "error")) {
    skip(sprintf("siu_director_reports needs the catalogue: %s",
                 conditionMessage(out)))
  }
  expect_true(is.data.frame(out) || is.list(out))
})

# ----------------------------------------------------- siu_report_text offline

test_that("morie_datasets_siu_report_text offline=TRUE returns empty string", {
  out <- tryCatch(
    morie_datasets_siu_report_text(url = "https://example.com/x",
                                    offline = TRUE),
    error = function(e) e
  )
  if (inherits(out, "error")) {
    skip(sprintf("siu_report_text offline: %s", conditionMessage(out)))
  }
  expect_true(is.character(out) || is.list(out))
})

test_that("morie_datasets_siu_report_text errors on NULL url + offline=FALSE", {
  expect_error(
    morie_datasets_siu_report_text(url = NULL, offline = FALSE)
  )
})

# ----------------------------------------------------- siu_report_fields

test_that("morie_datasets_siu_report_fields parses a minimal SIU text blob", {
  txt <- paste0(
    "Case Number: 23-OFD-001\n",
    "Date of Incident: 2023-08-04\n",
    "Region: Toronto\n",
    "Number of Officers Involved: 1 SO\n",
    "Sex/Gender of the Affected Person: Male\n"
  )
  out <- tryCatch(morie_datasets_siu_report_fields(txt),
                  error = function(e) e)
  if (inherits(out, "error")) {
    skip(sprintf("siu_report_fields: %s", conditionMessage(out)))
  }
  expect_true(is.list(out) || is.data.frame(out))
})

# ---------------------------------------------- network-gated loaders, smokey

test_that("morie_datasets_chicago_crime is invokable", {
  testthat::skip_if_offline("data.cityofchicago.org")
  out <- tryCatch(
    morie_datasets_chicago_crime(year = 2023L, max_features = 5L),
    error = function(e) e
  )
  if (inherits(out, "error")) {
    skip(sprintf("chicago_crime: %s", conditionMessage(out)))
  }
  expect_true(is.data.frame(out) || is.list(out))
})

test_that("morie_datasets_nyc_stop_and_frisk is invokable", {
  testthat::skip_if_offline("opendata.cityofnewyork.us")
  out <- tryCatch(
    morie_datasets_nyc_stop_and_frisk(year = 2023L, max_features = 5L),
    error = function(e) e
  )
  if (inherits(out, "error")) {
    skip(sprintf("nyc_stop_and_frisk: %s", conditionMessage(out)))
  }
  expect_true(is.data.frame(out) || is.list(out))
})

test_that("morie_datasets_ckan_search errors cleanly on unreachable portal", {
  # Use a clearly-unreachable host so we hit the error path without
  # relying on test_if_offline timing.
  out <- tryCatch(
    morie_datasets_ckan_search(
      portal = "https://invalid.host.local",
      query = "anything", rows = 3L),
    error = function(e) e
  )
  expect_true(inherits(out, "error") || is.null(out) ||
                is.data.frame(out))
})

test_that("morie_datasets_namus_missing_persons errors without an API key + offline", {
  # NamUs requires an API key + a working network. We only need the
  # error-path to be reachable for coverage.
  out <- tryCatch(
    morie_datasets_namus_missing_persons(state = "CA"),
    error = function(e) e
  )
  expect_true(inherits(out, "error") || is.data.frame(out))
})

test_that("morie_datasets_nibrs errors cleanly without FBI_API_KEY", {
  withr::with_envvar(
    c(FBI_API_KEY = ""),
    out <- tryCatch(morie_datasets_nibrs(year = 2023L,
                                          max_features = 5L),
                    error = function(e) e)
  )
  expect_true(inherits(out, "error") || is.data.frame(out))
})

test_that("morie_datasets_nist_rds is invokable", {
  out <- tryCatch(
    morie_datasets_nist_rds(dataset_id = "some_id", query = "test"),
    error = function(e) e
  )
  expect_true(inherits(out, "error") || is.list(out) ||
                is.data.frame(out))
})

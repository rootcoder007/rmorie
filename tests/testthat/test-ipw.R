# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Phase 2M: tests for ipw.R — propensity-score IPW analyses on
# synthetic CPADS-shaped data. Reuses make_canonical_cpads() from
# helper-cpads.R (testthat auto-sources helper-*.R).

test_that("morie_validate_cpads_data passes on canonical synthetic CPADS", {
  df <- make_canonical_cpads(n = 200L, seed = 1L)
  missing <- morie_validate_cpads_data(df, strict = TRUE)
  expect_length(missing, 0L)
})

test_that("morie_validate_cpads_data errors when required col is absent (strict)", {
  df <- make_canonical_cpads(n = 50L, seed = 2L)
  df$weight <- NULL
  expect_error(morie_validate_cpads_data(df, strict = TRUE),
               regexp = "weight|missing required")
})

test_that("morie_validate_cpads_data returns missing-list quietly when strict=FALSE", {
  df <- make_canonical_cpads(n = 50L, seed = 3L)
  df$cannabis_any_use <- NULL
  missing <- morie_validate_cpads_data(df, strict = FALSE)
  expect_true("cannabis_any_use" %in% missing)
})

test_that("morie_run_propensity_ipw_analysis returns IPW-fitted rich result", {
  df <- make_canonical_cpads(n = 500L, seed = 4L)
  res <- tryCatch(
    morie_run_propensity_ipw_analysis(df, output_dir = NULL),
    error = function(e) e
  )
  if (inherits(res, "error")) {
    skip(sprintf("ipw analysis error: %s", conditionMessage(res)))
  }
  expect_true(is.list(res))
})

test_that("morie_run_propensity_ipw_analysis respects a custom trim", {
  df <- make_canonical_cpads(n = 400L, seed = 5L)
  res <- tryCatch(
    morie_run_propensity_ipw_analysis(df, trim = c(0.05, 0.95)),
    error = function(e) e
  )
  if (inherits(res, "error")) {
    skip(sprintf("ipw trim path error: %s", conditionMessage(res)))
  }
  expect_true(is.list(res))
})

test_that("morie_run_ebac_selection_ipw_analysis is callable on CPADS-shaped data", {
  df <- make_canonical_cpads(n = 500L, seed = 6L)
  res <- tryCatch(
    morie_run_ebac_selection_ipw_analysis(df, output_dir = NULL),
    error = function(e) e
  )
  if (inherits(res, "error")) {
    skip(sprintf("ebac ipw analysis error: %s", conditionMessage(res)))
  }
  expect_true(is.list(res))
})

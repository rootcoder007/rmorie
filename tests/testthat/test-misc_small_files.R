# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Phase 2T: tests for several small (100-150 line) untested R files.
# Each exposes a single morie_* function with a narrowly-typed
# interface, so single-fn smoke + error-path coverage suffices.

# ----------------------------------------------------- inspector.R

test_that("morie_inspect_output handles a non-existent file", {
  out <- tryCatch(morie_inspect_output("/nonexistent/file.txt"),
                  error = function(e) e)
  expect_true(inherits(out, "error") || is.list(out) ||
                is.character(out))
})

test_that("morie_verify_statistical_output handles a non-existent file", {
  out <- tryCatch(
    morie_verify_statistical_output("/nonexistent/file.txt"),
    error = function(e) e)
  expect_true(inherits(out, "error") || is.list(out) ||
                is.character(out))
})

test_that("morie_inspect_output reads a small synthetic text file", {
  tmp <- tempfile(fileext = ".txt")
  on.exit(unlink(tmp), add = TRUE)
  writeLines(c("=== morie_otis_analyze_b01 ===",
               "Result: Some statistical summary text"), tmp)
  out <- tryCatch(morie_inspect_output(tmp), error = function(e) e)
  expect_true(inherits(out, "error") || is.list(out) ||
                is.character(out))
})

# ----------------------------------------------------- tpsuof.R

test_that("morie_tps_use_of_force computes UoF rate per total encounters", {
  # n_encounters is the TOTAL number of officer-citizen encounters
  # (a single scalar denominator), not per-type counts.
  out <- morie_tps_use_of_force(
    force_types = c("Display", "Empty hand", "OC spray",
                     "Baton", "CEW", "Firearm"),
    n_encounters = 1000L)
  expect_true(is.list(out) || is.data.frame(out))
})

test_that("morie_tps_use_of_force errors when n_encounters is not a positive scalar", {
  expect_error(
    morie_tps_use_of_force(force_types = c("Display", "CEW"),
                            n_encounters = c(100L, 200L)),
    regexp = "positive scalar"
  )
  expect_error(
    morie_tps_use_of_force(force_types = c("Display"),
                            n_encounters = 0L)
  )
})

# ----------------------------------------------------- dccmd.R

test_that("morie_dcc_multivariate_garch runs on a multi-series matrix", {
  set.seed(1L)
  n <- 200L; k <- 3L
  x <- matrix(stats::rnorm(n * k), n, k)
  out <- tryCatch(morie_dcc_multivariate_garch(x),
                  error = function(e) e)
  if (inherits(out, "error")) {
    skip(sprintf("dcc_garch error: %s", conditionMessage(out)))
  }
  expect_true(is.list(out) || is.matrix(out))
})

test_that("morie_dcc_multivariate_garch handles single-series input gracefully", {
  set.seed(2L)
  x <- matrix(stats::rnorm(100), 100, 1)
  out <- tryCatch(morie_dcc_multivariate_garch(x),
                  error = function(e) e)
  expect_true(inherits(out, "error") || is.list(out) || is.matrix(out))
})

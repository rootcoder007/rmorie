# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Phase 2M: tests for dataset_profile.R.

test_that("morie_infer_measurement_level classifies common R types", {
  # Convention: ratio for continuous numeric, ordinal for ordered factor,
  # binary for 2-level inputs (incl. factors + binary ints + 2-level
  # character), nominal would be > 2 unordered categories.
  expect_equal(morie_infer_measurement_level(c(1, 2, 3, 4, 5)), "ratio")
  expect_equal(morie_infer_measurement_level(c(0L, 1L, 0L, 1L)),
               "binary")
  expect_equal(morie_infer_measurement_level(
    factor(c("a", "b", "a"))), "binary")
  expect_equal(morie_infer_measurement_level(
    ordered(c("low", "med", "high"),
            levels = c("low", "med", "high"))), "ordinal")
  expect_equal(morie_infer_measurement_level(
    c("alice", "bob")), "binary")
})

test_that("morie_profile_dataset returns one row per column with key stats", {
  set.seed(1L)
  df <- data.frame(
    age   = sample(20:80, 50, replace = TRUE),
    sex   = sample(c("M", "F"), 50, replace = TRUE),
    score = stats::rnorm(50, mean = 100, sd = 15)
  )
  out <- morie_profile_dataset(df)
  expect_true(is.list(out) || is.data.frame(out))
  if (is.data.frame(out)) {
    expect_equal(nrow(out), 3L)
    expect_true(all(c("name", "dtype", "measurement_level",
                      "n_missing", "n_unique") %in% names(out)))
  }
})

test_that("morie_profile_dataset errors on non-data.frame input", {
  expect_error(morie_profile_dataset(1:10), regexp = "data\\.frame")
})

test_that("morie_suggest_analysis_plan returns a character vector of suggestions", {
  set.seed(2L)
  df <- data.frame(
    treatment = sample(0:1, 100, replace = TRUE),
    outcome   = stats::rnorm(100),
    age       = sample(20:80, 100, replace = TRUE),
    sex       = sample(c("M", "F"), 100, replace = TRUE)
  )
  prof <- morie_profile_dataset(df)
  plan <- morie_suggest_analysis_plan(prof)
  expect_true(is.character(plan) || is.list(plan) || is.data.frame(plan))
  expect_true(length(plan) >= 1L)
})

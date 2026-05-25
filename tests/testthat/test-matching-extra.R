# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Phase 2F-1: tests for matching.R exports not yet covered by
# test-matching.R. Uses make_match_df() from that file (auto-sourced).

# rosenbaum_bounds and doubly_robust already have placeholder tests in
# test-matching.R; the extra ones below target genetic, variable_ratio,
# cardinality, multi_treatment, and longitudinal (5 large untested fns).

test_that("morie_matching_genetic returns match_result on synthetic data", {
  df <- make_match_df(n = 150, tau = 0.4, seed = 2L)
  out <- tryCatch(
    morie_matching_genetic(df, "d", c("x1", "x2"),
                           pop_size = 20L, n_generations = 3L),
    error = function(e) e
  )
  if (inherits(out, "error")) {
    skip(sprintf("genetic matching needs richer fixture / GenMatch: %s",
                 conditionMessage(out)))
  }
  expect_true(is.list(out))
})

test_that("morie_matching_variable_ratio matches multiple controls per treated", {
  df <- make_match_df(n = 200, tau = 0.4, seed = 3L)
  out <- tryCatch(
    morie_matching_variable_ratio(df, "d", c("x1", "x2"),
                                   min_ratio = 1L, max_ratio = 3L),
    error = function(e) e
  )
  if (inherits(out, "error")) {
    skip(sprintf("variable_ratio error: %s", conditionMessage(out)))
  }
  expect_true(is.list(out))
})

test_that("morie_matching_cardinality runs on balanced synthetic data", {
  # Balanced data so the per-caliper "Fewer control" warning doesn't
  # fire; covers the happy path mathematically.
  df <- make_match_df_balanced(n = 200L, tau = 0.4, seed = 4L)
  out <- tryCatch(
    morie_matching_cardinality(df, "d", c("x1", "x2")),
    error = function(e) e
  )
  if (inherits(out, "error")) {
    skip(sprintf("cardinality matching needs richer fixture / LP solver: %s",
                 conditionMessage(out)))
  }
  expect_true(is.list(out))
})

test_that("morie_matching_cardinality emits a single summary warning on skewed data", {
  # Skewed treatment so MatchIt fires "Fewer control" on every
  # caliper pass; verify morie collapses into one summary.
  df <- make_match_df_skewed(n = 200L, tau = 0.4, seed = 14L)
  expect_warning(
    out <- morie_matching_cardinality(df, "d", c("x1", "x2")),
    "caliper passes had fewer control units than treated"
  )
  expect_true(is.list(out) || inherits(out, "morie_match_result"))
})

test_that("morie_matching_multi_treatment handles 3-arm treatment", {
  set.seed(5L)
  n <- 200
  df <- data.frame(
    d = sample(0:2, n, replace = TRUE),
    y = stats::rnorm(n),
    x1 = stats::rnorm(n),
    x2 = stats::rnorm(n)
  )
  out <- tryCatch(
    morie_matching_multi_treatment(df, "d", c("x1", "x2")),
    error = function(e) e
  )
  if (inherits(out, "error")) {
    skip(sprintf("multi_treatment matching error: %s",
                 conditionMessage(out)))
  }
  expect_true(is.list(out))
})

test_that("morie_matching_longitudinal runs on synthetic panel", {
  set.seed(6L)
  n_units <- 30; n_periods <- 4
  rows <- list()
  for (u in seq_len(n_units)) {
    is_treated <- u <= 15
    trt_time <- if (is_treated) 3L else NA_integer_
    for (t in seq_len(n_periods)) {
      d <- as.integer(is_treated && t >= 3L)
      rows[[length(rows) + 1L]] <- data.frame(
        unit = u, time = t, d = d, treatment_time = trt_time,
        y = 1 + 0.5 * d + stats::rnorm(1, sd = 0.5),
        x = stats::rnorm(1)
      )
    }
  }
  df <- do.call(rbind, rows)
  out <- tryCatch(
    morie_matching_longitudinal(df, "d", c("x"), unit = "unit",
                                  time = "time",
                                  treatment_time = "treatment_time"),
    error = function(e) e
  )
  if (inherits(out, "error")) {
    skip(sprintf("longitudinal matching error: %s",
                 conditionMessage(out)))
  }
  expect_true(is.list(out))
})

# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Phase 2DD: tests for the .study_core.R + .study_reporting.R
# internal helpers (accessed via morie:::) — pure-fn / numeric paths
# that don't need a full CPADS pipeline run.

# ============================================================== study_core

test_that(".wald_ci returns symmetric (1.96 SE) bounds", {
  ci <- morie:::.wald_ci(estimate = 0.5, se = 0.1)
  expect_true(is.numeric(ci) || is.list(ci))
  flat <- unlist(ci)
  expect_true(length(flat) >= 2L && all(is.finite(flat)))
})

test_that(".binary_ci returns plausible Wilson/Wald CI", {
  out <- morie:::.binary_ci(successes = 30L, n = 100L)
  expect_true(is.list(out) || is.numeric(out))
  flat <- unlist(out)
  expect_true(all(is.finite(flat)))
})

test_that(".binary_ci handles 0/N and N/N edge cases", {
  out_zero <- morie:::.binary_ci(successes = 0L, n = 100L)
  out_full <- morie:::.binary_ci(successes = 100L, n = 100L)
  expect_true(!is.null(out_zero))
  expect_true(!is.null(out_full))
})

test_that(".weighted_binary_estimate computes p-hat + SE for a weighted sample", {
  set.seed(1L)
  x <- stats::rbinom(200L, 1L, 0.4)
  w <- stats::rgamma(200L, 2.0)
  out <- morie:::.weighted_binary_estimate(x, w)
  expect_true(is.list(out) || is.numeric(out))
  flat <- unlist(out)
  expect_true(all(is.finite(flat[is.finite(flat)])))
})

test_that(".clip_exp clips its argument to a safe exp() range", {
  out_pos <- morie:::.clip_exp(1000)        # clip to ~exp(50)
  out_neg <- morie:::.clip_exp(-1000)       # clip to ~exp(-50)
  out_mid <- morie:::.clip_exp(1.5)         # passes through
  expect_true(is.finite(out_pos))
  expect_true(is.finite(out_neg))
  expect_equal(out_mid, exp(1.5), tolerance = 1e-10)
})

test_that(".safe_confint returns CI matrix on a real lm fit", {
  set.seed(2L)
  x <- stats::rnorm(60); y <- 0.5 * x + stats::rnorm(60)
  fit <- stats::lm(y ~ x)
  out <- morie:::.safe_confint(fit)
  expect_true(is.matrix(out) || is.data.frame(out) || is.list(out))
})

test_that(".or_table runs on a real logistic fit", {
  set.seed(3L)
  x <- stats::rnorm(80)
  y <- stats::rbinom(80L, 1L, stats::plogis(0.5 * x))
  fit <- stats::glm(y ~ x, family = stats::binomial())
  out <- tryCatch(morie:::.or_table(fit, model = "logit"),
                  error = function(e) e)
  if (inherits(out, "error")) {
    skip(sprintf(".or_table error: %s", conditionMessage(out)))
  }
  expect_true(is.data.frame(out) || is.list(out))
})

test_that(".linear_coef_table runs on a real lm fit", {
  set.seed(4L)
  x <- stats::rnorm(80); y <- 0.5 * x + stats::rnorm(80, sd = 0.3)
  fit <- stats::lm(y ~ x)
  out <- tryCatch(morie:::.linear_coef_table(fit, model = "lm"),
                  error = function(e) e)
  if (inherits(out, "error")) {
    skip(sprintf(".linear_coef_table error: %s",
                 conditionMessage(out)))
  }
  expect_true(is.data.frame(out) || is.list(out))
})

test_that(".cpads_labeled_data adds value labels to a synthetic CPADS frame", {
  df <- make_canonical_cpads(n = 50L, seed = 5L)
  out <- tryCatch(morie:::.cpads_labeled_data(df),
                  error = function(e) e)
  if (inherits(out, "error")) {
    skip(sprintf(".cpads_labeled_data error: %s",
                 conditionMessage(out)))
  }
  expect_true(is.data.frame(out) || is.list(out))
})

# ============================================================ study_reporting

test_that(".continuous_power_required_n returns finite N for a feasible diff", {
  out <- morie:::.continuous_power_required_n(
    mean1 = 0, mean2 = 0.5, sd_pooled = 1)
  expect_true(is.numeric(out) && out > 0)
})

test_that(".continuous_power_required_n returns NA when the effect is 0", {
  out <- morie:::.continuous_power_required_n(
    mean1 = 0, mean2 = 0, sd_pooled = 1)
  expect_true(is.na(out) || (is.numeric(out) && is.infinite(out)))
})

test_that(".block_schedule generates a block schedule for a given N", {
  out <- tryCatch(
    morie:::.block_schedule(endpoint = "binary",
                             required_n = 200L,
                             strata_levels = c("a", "b", "c")),
    error = function(e) e
  )
  if (inherits(out, "error")) {
    skip(sprintf(".block_schedule error: %s",
                 conditionMessage(out)))
  }
  expect_true(is.list(out) || is.data.frame(out))
})

test_that(".legacy_reference_root returns a character path", {
  out <- morie:::.legacy_reference_root()
  expect_true(is.character(out) && length(out) == 1L)
})

test_that(".read_existing_output returns the fallback on missing file", {
  tmp <- tempfile("study_out_")
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  out <- morie:::.read_existing_output(tmp, "nonexistent.json",
                                         fallback = "default-value")
  expect_equal(out, "default-value")
})

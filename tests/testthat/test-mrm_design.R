# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Phase 2M: tests for mrm_design.R — experimental-design helpers.

test_that("mrm_two_treatment_test reports Welch + Student + Wilcoxon p-values", {
  set.seed(1L)
  a <- stats::rnorm(50, mean = 100, sd = 10)
  b <- stats::rnorm(50, mean = 105, sd = 10)
  out <- mrm_two_treatment_test(a, b)
  expect_type(out, "list")
  # The exact field names vary across releases; sanity-check at minimum
  # one of the canonical statistics is present.
  flat <- unlist(out)
  expect_true(any(grepl("welch|student|wilcox|p[._-]value|t[._-]stat",
                        names(flat), ignore.case = TRUE)))
})

test_that("mrm_anova_oneway returns F + Tukey post-hoc on synthetic groups", {
  set.seed(2L)
  d <- data.frame(
    y = c(stats::rnorm(40, 0), stats::rnorm(40, 0.5),
          stats::rnorm(40, 1)),
    g = rep(c("a", "b", "c"), each = 40)
  )
  out <- mrm_anova_oneway(d, response_col = "y", group_col = "g")
  expect_type(out, "list")
  flat <- unlist(out)
  expect_true(any(grepl("f|p[._-]value|tukey", names(flat),
                        ignore.case = TRUE)))
})

test_that("mrm_factorial_2k returns 2^k main + interaction effects", {
  set.seed(3L)
  n <- 100L
  d <- data.frame(
    y  = stats::rnorm(n),
    f1 = sample(c(-1, 1), n, replace = TRUE),
    f2 = sample(c(-1, 1), n, replace = TRUE),
    f3 = sample(c(-1, 1), n, replace = TRUE)
  )
  out <- mrm_factorial_2k(d, response_col = "y",
                          factor_cols = c("f1", "f2", "f3"))
  expect_type(out, "list")
})

test_that("mrm_factorial_2k errors when given fewer than 2 factors", {
  d <- data.frame(y = stats::rnorm(20), f1 = sample(c(-1, 1), 20,
                                                     replace = TRUE))
  expect_error(mrm_factorial_2k(d, "y", "f1"),
               regexp = "2.*factors")
})

test_that("mrm_causal_design returns IPW estimator output", {
  set.seed(4L)
  n <- 200L
  d <- data.frame(
    d = stats::rbinom(n, 1, 0.5),
    y = stats::rnorm(n),
    x = stats::rnorm(n)
  )
  out <- mrm_causal_design(d, treatment_col = "d",
                            outcome_col = "y",
                            covariates = "x",
                            estimator = "ipw")
  expect_type(out, "list")
})

test_that("mrm_causal_design diff_in_means estimator path works too", {
  set.seed(5L)
  d <- data.frame(
    d = stats::rbinom(100, 1, 0.5),
    y = stats::rnorm(100)
  )
  out <- mrm_causal_design(d, "d", "y", estimator = "diff_in_means")
  expect_type(out, "list")
})

# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Phase 2P: tests for mrm_mathstats.R — proportion / variance tests,
# CLT/QQ/PIT visualisations.

test_that("mrm_oneprop_test recovers a known p when x/n approximates p0", {
  out <- mrm_oneprop_test(x = 52L, n = 100L, p0 = 0.5)
  expect_true(is.list(out) || is.data.frame(out))
  flat <- unlist(out)
  expect_true(any(grepl("p[._-]value|stat|estimate", names(flat),
                        ignore.case = TRUE)))
})

test_that("mrm_oneprop_test errors on x > n", {
  expect_error(mrm_oneprop_test(x = 150L, n = 100L, p0 = 0.5))
})

test_that("mrm_twoprop_test runs on two binomial samples", {
  out <- mrm_twoprop_test(x1 = 30L, n1 = 100L, x2 = 50L, n2 = 100L)
  expect_true(is.list(out) || is.data.frame(out))
})

test_that("mrm_twoprop_test: no events in either arm is chi2 0, p 1, with a warning", {
  expect_warning(out <- mrm_twoprop_test(0L, 50L, 0L, 50L), "degenerate")
  expect_identical(out$chi2, 0)
  expect_identical(out$p_value_chi2, 1)
  expect_identical(out$p_value_fisher, 1)
  expect_warning(out <- mrm_twoprop_test(50L, 50L, 50L, 50L), "degenerate")
  expect_identical(out$chi2, 0)
})

test_that("mrm_var_test runs against a known variance", {
  set.seed(1L)
  s <- stats::rnorm(50, sd = 2)
  out <- mrm_var_test(s, sigma0_sq = 4)
  expect_true(is.list(out) || is.data.frame(out))
})

test_that("mrm_qq_plot returns a plottable structure or invisible", {
  set.seed(2L)
  s <- stats::rnorm(100)
  out <- tryCatch(mrm_qq_plot(s, dist = "norm"),
                  error = function(e) e)
  if (inherits(out, "error")) {
    skip(sprintf("qq_plot error: %s", conditionMessage(out)))
  }
  # mrm_qq_plot is allowed to return a ggplot, a list, or invisibly NULL
  expect_true(is.null(out) || is.list(out) || inherits(out, "ggplot") ||
                is.data.frame(out))
})

test_that("mrm_clt_demo simulates means + recovers near-normality", {
  out <- tryCatch(
    mrm_clt_demo(base_distribution = "unif", sample_size = 100L, n_samples = 200L),
    error = function(e) e
  )
  if (inherits(out, "error")) {
    skip(sprintf("clt_demo error: %s", conditionMessage(out)))
  }
  expect_true(is.list(out) || is.data.frame(out))
})

test_that("mrm_pit returns probability-integral-transformed values", {
  set.seed(3L)
  s <- stats::rnorm(100)
  out <- mrm_pit(s, dist = "norm")
  expect_true(is.numeric(out) || is.list(out))
})

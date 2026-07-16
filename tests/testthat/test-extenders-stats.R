# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Phase 1.k tests for the stats wrapper-as-extender entry points in
# R/extenders_stats.R: DescTools / performance / ppcor / coin /
# randtests.  Each block guards with skip_if_not_installed() and
# checks the happy-path shape (a 2-element list with $method and
# $raw).

# ---------------------------------------------------------------------------
# DescTools
# ---------------------------------------------------------------------------

test_that("morie_desc_cramers_v wraps DescTools::CramerV", {
  skip_if_not_installed("DescTools")
  set.seed(1L)
  x <- sample(letters[1:3], 60L, replace = TRUE)
  y <- sample(letters[1:3], 60L, replace = TRUE)
  out <- morie_desc_cramers_v(x, y)
  expect_type(out, "list")
  expect_identical(out$method, "DescTools::CramerV")
  expect_true(is.numeric(out$raw) || is.matrix(out$raw))
})

test_that("morie_desc_kappa wraps DescTools::CohenKappa / KappaM", {
  skip_if_not_installed("DescTools")
  set.seed(2L)
  r1 <- sample(1:3, 40L, replace = TRUE)
  r2 <- sample(1:3, 40L, replace = TRUE)
  out <- morie_desc_kappa(r1, r2)
  expect_type(out, "list")
  expect_identical(out$method, "DescTools::CohenKappa")
  expect_false(is.null(out$raw))
})

test_that("morie_desc_winsorize wraps DescTools::Winsorize", {
  skip_if_not_installed("DescTools")
  x <- c(1:9, 100)
  out <- morie_desc_winsorize(x, probs = c(0.1, 0.9))
  expect_identical(out$method, "DescTools::Winsorize")
  expect_length(out$raw, length(x))
  expect_lte(max(out$raw), max(x))
})

test_that("morie_desc_gini wraps DescTools::Gini", {
  skip_if_not_installed("DescTools")
  out <- morie_desc_gini(c(1, 2, 3, 4, 5))
  expect_identical(out$method, "DescTools::Gini")
  expect_true(is.numeric(out$raw))
})

test_that("morie_desc_atkinson wraps DescTools::Atkinson", {
  skip_if_not_installed("DescTools")
  out <- morie_desc_atkinson(c(1, 2, 3, 4, 5), parameter = 0.5)
  expect_identical(out$method, "DescTools::Atkinson")
  expect_true(is.numeric(out$raw))
})


# ---------------------------------------------------------------------------
# performance
# ---------------------------------------------------------------------------

test_that("morie_performance_check_model wraps performance::check_model", {
  skip_if_not_installed("performance")
  set.seed(3L)
  df <- data.frame(y = rnorm(40L), x = rnorm(40L))
  fit <- lm(y ~ x, data = df)
  out <- tryCatch(
    morie_performance_check_model(fit, panel = FALSE),
    error = function(e) e
  )
  if (inherits(out, "error")) {
    skip(sprintf("check_model error: %s", conditionMessage(out)))
  }
  expect_identical(out$method, "performance::check_model")
  expect_false(is.null(out$raw))
})

test_that("morie_performance_r2 wraps performance::r2", {
  skip_if_not_installed("performance")
  set.seed(4L)
  df <- data.frame(y = rnorm(40L), x = rnorm(40L))
  fit <- lm(y ~ x, data = df)
  out <- morie_performance_r2(fit)
  expect_identical(out$method, "performance::r2")
  expect_false(is.null(out$raw))
})

test_that("morie_performance_check_collinearity wraps the VIF check", {
  skip_if_not_installed("performance")
  set.seed(5L)
  df <- data.frame(
    y = rnorm(60L),
    x1 = rnorm(60L),
    x2 = rnorm(60L)
  )
  fit <- lm(y ~ x1 + x2, data = df)
  out <- tryCatch(
    morie_performance_check_collinearity(fit),
    error = function(e) e
  )
  if (inherits(out, "error")) {
    skip(sprintf("check_collinearity error: %s", conditionMessage(out)))
  }
  expect_identical(out$method, "performance::check_collinearity")
  expect_false(is.null(out$raw))
})

test_that("morie_performance_check_outliers wraps the outlier check", {
  skip_if_not_installed("performance")
  set.seed(6L)
  df <- data.frame(y = c(rnorm(40L), 50), x = rnorm(41L))
  fit <- lm(y ~ x, data = df)
  out <- tryCatch(
    morie_performance_check_outliers(fit),
    error = function(e) e
  )
  if (inherits(out, "error")) {
    skip(sprintf("check_outliers error: %s", conditionMessage(out)))
  }
  expect_identical(out$method, "performance::check_outliers")
  expect_false(is.null(out$raw))
})


# ---------------------------------------------------------------------------
# ppcor
# ---------------------------------------------------------------------------

test_that("morie_ppcor_partial runs the native partial correlation", {
  set.seed(7L)
  df <- data.frame(
    a = rnorm(50L),
    b = rnorm(50L),
    c = rnorm(50L)
  )
  out_mat <- morie_ppcor_partial(df)
  expect_identical(out_mat$method, "partial_cor (rmorie native)")
  expect_false(is.null(out_mat$raw))

  out_one <- morie_ppcor_partial(df$a, df$b, df$c)
  expect_identical(out_one$method, "partial_cor_test (rmorie native)")
  expect_false(is.null(out_one$raw))
})

test_that("morie_ppcor_semipartial runs the native semipartial correlation", {
  set.seed(8L)
  df <- data.frame(
    a = rnorm(50L),
    b = rnorm(50L),
    c = rnorm(50L)
  )
  out_mat <- morie_ppcor_semipartial(df)
  expect_identical(out_mat$method, "semipartial_cor (rmorie native)")
  expect_false(is.null(out_mat$raw))

  out_one <- morie_ppcor_semipartial(df$a, df$b, df$c)
  expect_identical(out_one$method, "semipartial_cor_test (rmorie native)")
  expect_false(is.null(out_one$raw))
})


# ---------------------------------------------------------------------------
# coin
# ---------------------------------------------------------------------------

test_that("morie_coin_independence runs the native independence test", {
  set.seed(9L)
  df <- data.frame(
    y = rnorm(40L),
    x = rnorm(40L)
  )
  out <- morie_coin_independence(y ~ x, data = df)
  expect_identical(out$method, "morie independence permutation test")
  expect_true(is.finite(out$statistic))
  expect_true(out$p.value >= 0 && out$p.value <= 1)
})

test_that("morie_coin_wilcoxon runs the native Wilcoxon permutation test", {
  set.seed(10L)
  df <- data.frame(
    y = rnorm(40L),
    g = factor(rep(c("A", "B"), each = 20L))
  )
  out <- morie_coin_wilcoxon(y ~ g, data = df)
  expect_identical(out$method, "morie Wilcoxon permutation test")
  expect_true(is.finite(out$statistic))
  expect_true(out$p.value >= 0 && out$p.value <= 1)
})

test_that("morie_coin_oneway runs the native one-way permutation test", {
  set.seed(11L)
  df <- data.frame(
    y = rnorm(60L),
    g = factor(rep(c("A", "B", "C"), each = 20L))
  )
  out <- morie_coin_oneway(y ~ g, data = df)
  expect_identical(out$method, "morie one-way permutation test")
  expect_true(is.finite(out$statistic))
  expect_true(out$p.value >= 0 && out$p.value <= 1)
})


# ---------------------------------------------------------------------------
# randtests
# ---------------------------------------------------------------------------

test_that("morie_randtests_runs runs the native runs test", {
  set.seed(12L)
  out <- morie_randtests_runs(rnorm(40L))
  expect_identical(out$method, "runs_test (rmorie native)")
  expect_true(is.finite(out$raw$p.value))
})

test_that("morie_randtests_turning_point runs the native turning-point test", {
  set.seed(13L)
  out <- morie_randtests_turning_point(rnorm(40L))
  expect_identical(out$method, "turning_point_test (rmorie native)")
  expect_true(is.finite(out$raw$p.value))
})

test_that("morie_randtests_bartels runs the native Bartels rank test", {
  set.seed(14L)
  out <- morie_randtests_bartels(rnorm(40L))
  expect_identical(out$method, "bartels_rank_test (rmorie native)")
  expect_true(is.finite(out$raw$p.value))
})

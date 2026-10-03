# SPDX-License-Identifier: AGPL-3.0-or-later
# Research P16 (continued): the disposed-cases mean as a bound must agree with research/lean/P16Censoring.lean.

test_that("true_mean_ge, lower_bound_sub, bias_lower and disposed_understates on random dockets", {
  set.seed(16)
  for (rep in 1:60) {
    n <- sample(1:12, 1); m <- sample(1:12, 1)
    t <- runif(n, 0, 200); a <- runif(m, 0, 400)
    b <- morie_backlog_censoring(t, a)
    expect_equal(b$n, n); expect_equal(b$m, m)
    expect_equal(b$disposed_mean, mean(t), tolerance = 1e-12)
    expect_equal(b$pending_age, mean(a), tolerance = 1e-12)
    L <- (sum(t) + sum(a)) / (n + m)
    expect_equal(b$lower_bound, L, tolerance = 1e-12)
    expect_equal(b$lower_bound - b$disposed_mean, (m / (n + m)) * (mean(a) - mean(t)), tolerance = 1e-10)   # lower_bound_sub
    expect_equal(b$bias_lower, (m / (n + m)) * (mean(a) - mean(t)), tolerance = 1e-12)
    expect_equal(b$understates, mean(t) <= mean(a))
    expect_equal(b$upper_bound, Inf)
    for (draw in 1:5) {
      u <- a + runif(m, 0, 500)
      truth <- mean(c(t, u))
      expect_gte(truth, b$lower_bound - 1e-10)                        # true_mean_ge
      expect_gte(truth - b$disposed_mean, b$bias_lower - 1e-10)       # bias_lower
      if (b$understates) expect_gte(truth, b$disposed_mean - 1e-10)   # disposed_understates
    }
  }
})

test_that("no_upper_bound: pending durations can push the true mean past any level", {
  t <- c(30, 45, 60); a <- c(100, 150)
  b <- morie_backlog_censoring(t, a)
  for (B in c(100, 1e3, 1e6)) {
    K <- max((B * 5 - sum(t) - sum(a)) / 2 + 1, 0)
    u <- a + K
    expect_gt(mean(c(t, u)), B)
    expect_true(all(u >= a))
  }
  expect_equal(b$lower_bound, 385 / 5)
})

test_that("input checks", {
  expect_error(morie_backlog_censoring("a", 1), "numeric")
  expect_error(morie_backlog_censoring(c(1, NA), 1), "missing")
  expect_error(morie_backlog_censoring(c(1, -1), 1), "non-negative")
  expect_error(morie_backlog_censoring(numeric(0), 1), "at least one")
  expect_error(morie_backlog_censoring(1, numeric(0)), "at least one")
})

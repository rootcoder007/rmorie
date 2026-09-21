# SPDX-License-Identifier: AGPL-3.0-or-later
# Research P7: the concentration functions must agree with research/lean/P7Concentration.lean.

set.seed(1)

test_that("the O(n log n) Gini equals the mean-absolute-difference definition", {
  set.seed(1)
  for (i in 1:20) {
    x <- rpois(50, 0.7)
    if (sum(x) == 0) next
    n <- length(x)
    mad_form <- sum(abs(outer(x, x, "-"))) / (2 * n^2 * mean(x))
    expect_equal(morie_concentration_gini(x), mad_form, tolerance = 1e-12)
  }
})

test_that("Gini splits exactly into zero share and positive-place Gini (gini_zero_decomposition)", {
  set.seed(1)
  for (i in 1:50) {
    x <- rpois(200, stats::runif(1, 0.05, 3))
    if (sum(x > 0) < 2) next
    d <- morie_concentration_decompose(x)
    expect_equal(d$identity_check, 0, tolerance = 1e-12)
    expect_equal(d$gini_all, d$zero_share + (1 - d$zero_share) * d$gini_positive, tolerance = 1e-12)
  }
})

test_that("a uniform Poisson process produces the null zero share (poisson_zero_prob)", {
  set.seed(1)
  x <- rpois(200000, 0.4)
  d <- morie_concentration_decompose(x)
  expect_equal(d$zero_share, exp(-0.4), tolerance = 5e-3)
  expect_lt(abs(d$excess_zero_share), 5e-3)
  # and its overall Gini is large even though every place has the same rate
  expect_gt(d$gini_all, 0.5)
})

test_that("heterogeneous rates show up as excess zero share and positive-place concentration", {
  set.seed(1)
  rates <- rgamma(200000, shape = 0.3, rate = 0.3 / 0.4)   # same mean 0.4, very unequal
  y <- rpois(200000, rates)
  d <- morie_concentration_decompose(y)
  expect_gt(d$excess_zero_share, 0.1)
  u <- morie_concentration_decompose(rpois(200000, 0.4))
  expect_gt(d$gini_positive, u$gini_positive)
})

test_that("argument checks", {
  set.seed(1)
  expect_error(morie_concentration_gini(c(0, 0)), "positive total")
  expect_error(morie_concentration_decompose(c(-1, 2)), "non-negative")
})

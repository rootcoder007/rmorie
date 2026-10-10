# SPDX-License-Identifier: AGPL-3.0-or-later

test_that("james_stein validates input", {
  expect_error(james_stein(c(1, 2)), ">= 3 means")
  expect_error(james_stein(1:4, sigma2 = 0), "sigma2 must be positive")
})

test_that("james_stein shrinks toward a supplied target", {
  x <- c(2, -2, 2, -2)
  r <- james_stein(x, target = 0, sigma2 = 1)
  expect_identical(r$k, 2)
  expect_equal(r$shrinkage_factor, 1 - 2 / 16)
  expect_equal(r$js_estimates, (1 - 2 / 16) * x)
})

test_that("james_stein defaults to the grand mean", {
  r <- james_stein(c(1, 2, 3, 4, 5))
  expect_equal(r$target, 3)
  expect_identical(r$k, 2)
  expect_equal(r$shrinkage_factor, 0.8)
})

# SPDX-License-Identifier: AGPL-3.0-or-later

test_that("morie_garch_fit rejects short series", {
  expect_error(morie_garch_fit(rnorm(5)), "Need >=10 obs")
})

test_that(".garch_negll guards the parameter domain", {
  expect_equal(.garch_negll(c(-1, 0.1, 0.8), rnorm(20), 20L), 1e10)
  expect_equal(.garch_negll(c(0.1, 0.5, 0.6), rnorm(20), 20L), 1e10)
})

test_that("morie_garch_fit estimates a stationary GARCH(1,1)", {
  set.seed(42)
  x <- rnorm(200)
  r <- morie_garch_fit(x)
  expect_identical(r$n, 200L)
  expect_lt(r$persistence, 1)
  expect_length(r$conditional_variance, 200L)
  expect_true(is.finite(r$loglik))
  expect_true(is.finite(.garch_negll(c(0.1, 0.1, 0.8), x - mean(x), 200L)))
})

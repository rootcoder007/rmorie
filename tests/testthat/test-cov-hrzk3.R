# SPDX-License-Identifier: AGPL-3.0-or-later

test_that("hrzk3 returns NA for too little data", {
  r <- hrzk3(1:2, 1:2)
  expect_true(is.na(r$estimate))
  expect_equal(r$n, 2L)
  expect_match(r$method, "insufficient")
  expect_match(hrzk3(1:3, 1:2)$method, "insufficient")
})

test_that("hrzk3 replaces a non-positive bandwidth and flags far grid points", {
  set.seed(1)
  x <- rnorm(30)
  y <- 2 * x + rnorm(30, sd = 0.1)
  r <- hrzk3(x, y, bandwidth = -1, grid = c(0, 1e6))
  expect_equal(r$bandwidth, .hrz_silverman(x))
  expect_equal(r$estimate[1], 0, tolerance = 0.2)
  expect_true(is.na(r$estimate[2]))
  expect_true(is.na(r$se[2]))
})

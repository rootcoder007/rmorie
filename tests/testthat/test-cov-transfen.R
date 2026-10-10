# SPDX-License-Identifier: AGPL-3.0-or-later

test_that("Transent returns NaN when the series is too short", {
  r <- Transent(c(1, 0), c(0, 1), lag = 1)
  expect_true(is.nan(r$te_xy))
  expect_true(is.nan(r$te_yx))
  expect_identical(r$n, 2L)
})

test_that("Transent detects a lagged copy", {
  set.seed(1)
  x <- sample(0:1, 200, replace = TRUE)
  y <- c(0, x[-200])
  r <- Transent(x, y, lag = 1)
  expect_gt(r$te_xy, r$te_yx)
  expect_equal(r$bits, r$te_xy / log(2))
  r2 <- Transent(x, y, lag = 2)
  expect_identical(r2$lag, 2L)
  expect_true(is.finite(r2$te_xy))
})

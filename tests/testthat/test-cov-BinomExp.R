# SPDX-License-Identifier: AGPL-3.0-or-later

test_that("BinomExp validates arguments", {
  expect_error(BinomExp(c(1, 2), 2, 1), "x must be a single value")
  expect_error(BinomExp(NA, 2, 1), "x must be a single value")
  expect_error(BinomExp(1, 2, c(1, 2)), "delta must be a single value")
  expect_error(BinomExp(1, 1.5, 1), "n must be a single integer")
  expect_error(BinomExp(1, -1, 1), "n must be a single integer")
  r <- BinomExp(1, 3, 2)
  expect_equal(r$sum, 27)
  expect_equal(r$abs_error, 0)
})

# SPDX-License-Identifier: AGPL-3.0-or-later

test_that("ExpMean returns tau and rejects bad tau", {
  expect_equal(ExpMean(2.5)$mean, 2.5)
  expect_error(ExpMean(0), "tau must be a single value > 0")
  expect_error(ExpMean(c(1, 2)), "tau must be a single value > 0")
  expect_error(ExpMean(NA), "tau must be a single value > 0")
})

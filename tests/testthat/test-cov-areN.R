# SPDX-License-Identifier: AGPL-3.0-or-later

test_that("Areratio computes the variance ratio and validates input", {
  r <- Areratio(2, 1, n1 = 2, n2 = 1)
  expect_equal(r$are, 1)
  expect_equal(r$logare, 0)
  expect_error(Areratio(0, 1), "variances must be strictly positive")
  expect_error(Areratio(1, 1, n1 = 0), "sample sizes must be strictly positive")
})

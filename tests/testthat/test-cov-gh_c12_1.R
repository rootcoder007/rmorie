# SPDX-License-Identifier: AGPL-3.0-or-later

test_that("Ghosalinfdimbvm validates n and theta0", {
  expect_error(Ghosalinfdimbvm(n = 0), "n must be positive")
  expect_error(Ghosalinfdimbvm(theta0 = 1), "strictly between 0 and 1")
  expect_error(Ghosalinfdimbvm(theta0 = 0), "strictly between 0 and 1")
})

test_that("Ghosalinfdimbvm reports a small total-variation gap", {
  r <- Ghosalinfdimbvm(theta0 = 0.4, n = 2000, seed = 42)
  expect_s3_class(r, "morie_rich_result")
  expect_gte(r$estimate, 0)
  expect_lt(r$estimate, 0.05)
  expect_true(r$bvm_holds)
})

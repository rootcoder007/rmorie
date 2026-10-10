# SPDX-License-Identifier: AGPL-3.0-or-later

test_that("morie_qmDS handles singleton calibration samples", {
  r <- morie_qmDS(c(1, 5), obs = 3, mod = 2)
  expect_equal(r$probs, c(0.5, 0.5))
  expect_equal(r$estimate, c(3, 3))
})

test_that("morie_qmDS rejects empty calibration samples", {
  expect_error(morie_qmDS(1, obs = numeric(0), mod = 1:3), "must be non-empty")
})

test_that("morie_qmDS recovers a pure shift", {
  mod <- c(1, 2, 3, 4)
  r <- morie_qmDS(c(0, 2.5, 9), obs = mod + 10, mod = mod)
  expect_equal(r$estimate, c(11, 12.5, 14))
})

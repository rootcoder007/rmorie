# SPDX-License-Identifier: AGPL-3.0-or-later

test_that("Stickpost treats distinct non-negative integers as counts", {
  r <- Stickpost(c(3, 1, 2), alpha = 1)
  expect_equal(r$counts, c(3, 1, 2))
  # V_1 = (1 + 3) / (1 + 3 + 1 + 3)
  expect_equal(r$V[1], 4 / 8)
  expect_equal(r$estimate, r$pi[1])
  expect_equal(sum(r$pi) + r$remainder, 1)
})

test_that("Stickpost tabulates label vectors", {
  r <- Stickpost(c(1, 1, 2), alpha = 1)
  expect_equal(r$counts, c(2, 1))
  expect_length(r$V, 2L)
})

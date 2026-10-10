# SPDX-License-Identifier: AGPL-3.0-or-later

test_that("Raklng validates its inputs", {
  m <- list(list(labels = c("a", "b"), targets = c(a = 1, b = 1)))
  expect_error(Raklng(numeric(0), numeric(0), m), "y is empty")
  expect_error(Raklng(1:2, 1, m), "one entry per observation")
  expect_error(Raklng(1:2, c(1, 0), m), "weights must be positive")
  expect_error(Raklng(1:2, c(1, 1), list()), "at least one margin")
  expect_error(Raklng(1:2, c(1, 1), list(list("a", c(a = 1)))),
               "margin labels must have one entry")
  expect_error(Raklng(1:2, c(1, 1), list(list(c("a", "z"), c(a = 1, b = 1)))),
               "no target for a level")
  expect_error(Raklng(1:2, c(1, 1), list(list(c("a", "b"), c(a = -1, b = 3)))),
               "non-negative")
  expect_error(Raklng(1:2, c(1, 1), list(
    list(c("a", "b"), c(a = 1, b = 1)),
    list(c("x", "x"), c(x = 5))
  )), "inconsistent totals")
})

test_that("Raklng handles empty levels", {
  # level c has no sampled unit but a positive target
  expect_error(Raklng(1:2, c(1, 1), list(list(c("a", "b"), c(a = 1, b = 1, c = 1)))),
               "level c has no sampled unit")
  # zero target on an empty level is skipped
  r <- Raklng(c(2, 4), c(1, 1), list(list(c("a", "b"), c(a = 1, b = 3, c = 0))))
  expect_equal(r$weights, c(1, 3))
  expect_equal(r$estimate, (2 + 12) / 4)
})

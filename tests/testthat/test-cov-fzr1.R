# SPDX-License-Identifier: AGPL-3.0-or-later

test_that("Kdfr1 integrates a callable kernel", {
  r <- Kdfr1(dnorm)
  expect_equal(r$kernel, "callable")
  expect_equal(r$estimate, 1 / (2 * sqrt(pi)), tolerance = 1e-6)
  epan <- function(y) ifelse(abs(y) <= 1, 0.75 * (1 - y^2), 0)
  expect_gt(Kdfr1(epan, lo = -1, hi = 1)$estimate, 0)
  expect_error(Kdfr1("epan"), "kernel must be")
  g <- Kdfr1()
  expect_equal(g$kernel, "gaussian")
  expect_equal(g$estimate, 0.2820947917738781, tolerance = 1e-15)
})

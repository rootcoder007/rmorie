# SPDX-License-Identifier: AGPL-3.0-or-later

test_that("Rootnoise deterministic eta uses both gamma CDF branches", {
  # alpha = 3 forces x >= a + 1 (continued-fraction branch) during bracketing
  r <- Rootnoise(c(0.5, 0.3, 0.2), alpha = 3, eps = 0.25)
  expect_equal(sum(r$eta), 1)
  expect_equal(sum(r$p_noisy), 1)
  expect_equal(r$estimate, r$p_noisy[1])
  # van der Corput points 0.5, 0.25, 0.75 mapped through the gamma quantile
  q <- qgamma(c(0.5, 0.25, 0.75), 3)
  expect_equal(r$eta, q / sum(q), tolerance = 1e-6)
})

test_that("Rootnoise single prior falls back to uniform eta", {
  r <- Rootnoise(1, alpha = 0.3, eps = 0.5)
  expect_equal(r$eta, 1)
  expect_equal(r$p_noisy, 1)
  expect_equal(r$entropy, 0)
})

test_that("Rootnoise supplied eta is normalised", {
  r <- Rootnoise(c(0.5, 0.5), eps = 0.5, eta = c(1, 3))
  expect_equal(r$eta, c(0.25, 0.75))
  expect_equal(r$p_noisy, c(0.375, 0.625))
  expect_equal(r$entropy, -sum(r$p_noisy * log(r$p_noisy)))
  z <- Rootnoise(c(0.5, 0.5), eps = 0.5, eta = c(0, 0))
  expect_equal(z$eta, c(0, 0))
  expect_equal(z$p_noisy, c(0.25, 0.25))
})

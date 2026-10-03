# SPDX-License-Identifier: AGPL-3.0-or-later
# Research P16: Little's law on a docket must agree with research/lean/P16Backlog.lean.

test_that("the occupancy integral is the sum of durations and L = lambda W exactly", {
  set.seed(17)
  for (k in 1:40) {
    n <- sample(5:60, 1); horizon <- 100
    a <- runif(n, 0, 80); d <- a + runif(n, 0, 20)
    r <- morie_court_backlog(a, d, horizon = horizon, target_backlog = 2.5)
    # the integral of the pending count, computed on a fine grid, matches sum(d - a) (occupancy_integral)
    grid <- seq(0, horizon, by = 0.001)
    pending <- vapply(grid, function(t) sum(a <= t & t < d), numeric(1))
    expect_equal(sum(pending) * 0.001, r$occupancy_integral, tolerance = 2e-3)
    expect_equal(r$occupancy_integral, sum(d - a), tolerance = 1e-12)
    expect_equal(r$average_backlog, r$filing_rate * r$mean_disposition_time, tolerance = 1e-12)   # little
    expect_equal(r$little_identity_check, 0, tolerance = 1e-12)
    expect_equal(r$required_mean_time, 2.5 / r$filing_rate, tolerance = 1e-12)                   # little_target
    expect_equal(r$occupancy_integral, horizon * (r$filing_rate * r$mean_disposition_time), tolerance = 1e-12)  # little_backlog
  }
  r <- morie_court_backlog(c(0, 1, 2, 4, 5, 7), c(3, 2.5, 6, 5, 9, 10), horizon = 10)
  expect_equal(r$average_backlog, 1.65, tolerance = 1e-12)
  rc <- morie_court_backlog(c(0, 1, 2), c(3, NA, 12), horizon = 10)
  expect_equal(rc$n_censored, 2); expect_equal(rc$occupancy_integral, 3 + 9 + 8); expect_true(is.na(rc$little_identity_check))
  expect_error(morie_court_backlog(c(0, 1), c(1)), "same positive length")
  expect_error(morie_court_backlog(c(-1, 1), c(1, 2)), "non-negative")
  expect_error(morie_court_backlog(c(0, 1), c(1, 2), horizon = 0), "positive")
  expect_error(morie_court_backlog(c(0, 11), c(1, 12), horizon = 10), "inside the horizon")
  expect_error(morie_court_backlog(c(2, 1), c(1, 2)), "precede")
  expect_error(morie_court_backlog(c(0, 1), c(1, 2), target_backlog = -1), "non-negative")
})

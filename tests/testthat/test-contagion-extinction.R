# SPDX-License-Identifier: AGPL-3.0-or-later
# Research P10: extinction probability must agree with research/lean/P10Extinction.lean.

test_that("the iterates are monotone, converge to a fixed point, and the mean decides the regime", {
  set.seed(8)
  for (k in 1:60) {
    K <- sample(2:6, 1)
    p <- rgamma(K + 1, 1); p <- p / sum(p)
    r <- morie_contagion_extinction(p)
    expect_true(all(diff(r$iterates) >= -1e-15))                           # iter_mono
    expect_lt(abs(r$fixed_point_check), 1e-12)                            # extinction_fixed
    expect_gte(r$extinction, 0); expect_lte(r$extinction, 1)
    expect_equal(r$mean_offspring, sum((0:K) * p), tolerance = 1e-12)
    if (r$mean_offspring < 1) expect_equal(r$extinction, 1, tolerance = 1e-9)    # subcritical_extinction_one
    if (r$mean_offspring > 1) expect_lt(r$extinction, 1 - 1e-9)                  # supercritical_extinction_lt_one
    # the smallest fixed point: no fixed point of f lies below it (extinction_le_fixed)
    f <- function(s) sum(p * s^(0:K))
    grid <- seq(0, r$extinction - 1e-6, length.out = 200)
    if (r$extinction > 1e-6) expect_true(all(sapply(grid, f) > grid))
  }
  r <- morie_contagion_extinction(c(0.3, 0.3, 0.4))
  # f(s) = 0.3 + 0.3 s + 0.4 s^2 = s  =>  0.4 s^2 - 0.7 s + 0.3 = 0  =>  s = 0.75
  expect_equal(r$extinction, 0.75, tolerance = 1e-9)
  expect_equal(r$regime, "supercritical")
  expect_equal(morie_contagion_extinction(c(0.5, 0.3, 0.2))$extinction, 1, tolerance = 1e-9)
  expect_error(morie_contagion_extinction(c(0.5, 0.6)), "summing to one")
})

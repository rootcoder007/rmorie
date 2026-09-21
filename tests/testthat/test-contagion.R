# SPDX-License-Identifier: AGPL-3.0-or-later
# Research P10: the branching arithmetic must agree with research/lean/P10Contagion.lean.

test_that("subcritical branching gives 1/(1-n) cluster size, mu/(1-n) rate and endogeneity n", {
  for (n in c(0, 0.1, 0.5, 0.9, 0.99)) {
    b <- morie_contagion_branching(n, mu = 3)
    expect_true(b$subcritical)
    expect_equal(b$expected_cluster_size, sum(n^(0:5000)), tolerance = 1e-8)
    expect_equal(b$stationary_rate, 3 * b$expected_cluster_size)
    expect_equal((b$stationary_rate - 3) / b$stationary_rate, n, tolerance = 1e-12)
    expect_equal(b$generation_means, n^(0:9))
  }
  # a simulated Galton-Watson tree with Poisson(n) offspring has the proved mean cluster size
  set.seed(1)
  n <- 0.6; sizes <- replicate(4000, { z <- 1; tot <- 1; while (z > 0 && tot < 1e4) { z <- sum(stats::rpois(z, n)); tot <- tot + z }; tot })
  expect_equal(mean(sizes), 1 / (1 - n), tolerance = 0.06)
})

test_that("critical and supercritical ratios have no finite cluster size", {
  for (n in c(1, 1.1, 2)) {
    b <- morie_contagion_branching(n)
    expect_false(b$subcritical); expect_equal(b$expected_cluster_size, Inf); expect_true(is.na(b$endogeneity_share))
    expect_true(all(b$generation_means >= 1))
  }
  expect_error(morie_contagion_branching(-0.1), "non-negative")
})

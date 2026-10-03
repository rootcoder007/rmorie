# SPDX-License-Identifier: AGPL-3.0-or-later
# Research P19: regression to the mean must agree with research/lean/P19Regression.lean.

test_that("the symmetrised selected change is never positive and the exact exchangeable identities hold", {
  set.seed(20)
  for (k in 1:60) {
    n <- 200; mu <- rgamma(n, 2, 0.5)
    x1 <- rpois(n, mu); x2 <- rpois(n, mu); w <- runif(n, 0.5, 2)
    c0 <- quantile(x1, runif(1, 0.5, 0.95))
    r <- morie_regression_to_mean(x1, x2, c0, w)
    expect_lte(r$symmetrised_change, 1e-12)                                        # selected_change_nonpos (symmetrised pop)
    if (!is.na(r$low_symmetrised_change)) expect_gte(r$low_symmetrised_change, -1e-12)   # low_selected_change_nonneg
    expect_equal(r$excess_over_symmetry, r$selected_change - r$symmetrised_change, tolerance = 1e-12)
    # an exactly exchangeable population: the data and its swapped copy; selected mass and cross term reindex
    X1 <- c(x1, x2); X2 <- c(x2, x1); W <- c(w, w)
    s1 <- X1 > c0; s2 <- X2 > c0
    expect_equal(sum(W[s1]), sum(W[s2]), tolerance = 1e-12)                        # exchange_mass
    expect_equal(sum(W * X2 * s1), sum(W * X1 * s2), tolerance = 1e-12)            # exchange_cross
    expect_lte(sum(W[s1] * (X2 - X1)[s1]), 1e-12)                                  # selected_change_nonpos
    expect_true(all(X1 * (s1 - s2) >= c0 * (s1 - s2) - 1e-12))                     # indicator_bound
    rr <- morie_regression_to_mean(X1, X2, c0, W)
    expect_equal(rr$selected_change, rr$symmetrised_change, tolerance = 1e-12)
  }
  expect_error(morie_regression_to_mean(1:3, 1:2, 1), "equal length")
  expect_error(morie_regression_to_mean(c(1, NA), c(1, 2), 1), "NA")
  expect_error(morie_regression_to_mean(1:3, 1:3, c(1, 2)), "single number")
  expect_error(morie_regression_to_mean(1:3, 1:3, 1, weights = c(-1, 1, 1)), "non-negative")
  expect_error(morie_regression_to_mean(1:3, 1:3, 10), "exceeds the threshold")
})

# SPDX-License-Identifier: AGPL-3.0-or-later
# Research P13: pooling must agree with research/lean/P13Meta.lean.

test_that("DerSimonian-Laird: Q, c and tau2 from first principles; RE variance >= FE, equality iff tau2 = 0", {
  set.seed(1)
  for (k in 1:40) {
    n <- sample(2:8, 1)
    v <- runif(n, 0.005, 0.05)
    est <- rnorm(n, -0.2, sqrt(v) * sample(c(0.5, 1, 3), 1))
    m <- morie_meta_random_effects(est, v)
    w <- 1 / v
    theta <- sum(w * est) / sum(w)
    q <- sum(w * (est - theta)^2)
    cc <- sum(w) - sum(w^2) / sum(w)
    expect_equal(m$Q, q, tolerance = 1e-12)
    expect_equal(m$c, cc, tolerance = 1e-12)
    expect_equal(m$tau2, max(0, (q - (n - 1)) / cc), tolerance = 1e-12)   # tauDL definition
    expect_gte(m$tau2, 0)                                                  # tauDL_nonneg
    expect_equal(m$truncated, q <= n - 1)                                  # tauDL_eq_zero_iff
    expect_equal(m$tau2 == 0, q <= n - 1)
    expect_gte(m$random$variance, m$fixed$variance - 1e-15)                # re_var_ge
    expect_equal(m$random$variance == m$fixed$variance, m$tau2 == 0)       # re_var_eq_iff
    expect_gte(m$variance_ratio, 1 - 1e-15)
    expect_equal(m$random$variance, 1 / sum(1 / (v + m$tau2)), tolerance = 1e-12)
    expect_equal(m$fixed$estimate, theta, tolerance = 1e-12)
  }
  expect_error(morie_meta_random_effects(1, 1), "at least two")
  expect_error(morie_meta_random_effects(c(1, 2), c(1, -1)), "positive")
})

test_that("re_var_ge over any tau2 >= 0, with equality only at zero", {
  v <- c(0.01, 0.02, 0.05)
  fe <- 1 / sum(1 / v)
  for (t in c(0, 1e-6, 0.01, 0.5, 3)) {
    re <- 1 / sum(1 / (v + t))
    expect_gte(re, fe)
    expect_equal(re == fe, t == 0)
  }
})

test_that("under homogeneity the truncated estimator averages above zero (truncation_bias, pos_part_pos)", {
  v <- c(0.010, 0.020, 0.015, 0.030, 0.012)
  b <- morie_meta_dl_bias(v, n_draws = 3000, seed = 11)
  expect_gt(b$mean_tau2, 0)                       # dl_biased_under_homogeneity: E[max(X,0)] > E[X] = 0
  expect_gt(b$share_positive, 0)
  expect_lt(b$share_positive, 1)
  expect_gt(b$mean_variance_ratio, 1)
  expect_equal(b$mean_Q, length(v) - 1, tolerance = 0.1)  # E[Q] = k - 1 under homogeneity
  # the same seed reproduces, a different seed does not
  b2 <- morie_meta_dl_bias(v, n_draws = 3000, seed = 11)
  expect_identical(b$mean_tau2, b2$mean_tau2)
  expect_false(identical(b$mean_tau2, morie_meta_dl_bias(v, n_draws = 3000, seed = 12)$mean_tau2))
  # the truncation identity on the simulated draws: mean(max(X,0)) - mean(X) = mean(max(-X,0)) >= 0
  set.seed(3)
  x <- rnorm(5000)
  expect_equal(mean(pmax(x, 0)), mean(x) + mean(pmax(-x, 0)), tolerance = 1e-12)
  expect_gte(mean(pmax(x, 0)), max(mean(x), 0))  # pos_part_ge
})

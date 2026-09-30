# SPDX-License-Identifier: AGPL-3.0-or-later
# Research P7: the concentration functions must agree with research/lean/P7Concentration.lean.

set.seed(1)

test_that("the O(n log n) Gini equals the mean-absolute-difference definition", {
  set.seed(1)
  for (i in 1:20) {
    x <- rpois(50, 0.7)
    if (sum(x) == 0) next
    n <- length(x)
    mad_form <- sum(abs(outer(x, x, "-"))) / (2 * n^2 * mean(x))
    expect_equal(morie_concentration_gini(x), mad_form, tolerance = 1e-12)
  }
})

test_that("Gini splits exactly into zero share and positive-place Gini (gini_zero_decomposition)", {
  set.seed(1)
  for (i in 1:50) {
    x <- rpois(200, stats::runif(1, 0.05, 3))
    if (sum(x > 0) < 2) next
    d <- morie_concentration_decompose(x)
    expect_equal(d$identity_check, 0, tolerance = 1e-12)
    expect_equal(d$gini_all, d$zero_share + (1 - d$zero_share) * d$gini_positive, tolerance = 1e-12)
  }
})

test_that("a uniform Poisson process produces the null zero share (poisson_zero_prob)", {
  set.seed(1)
  x <- rpois(200000, 0.4)
  d <- morie_concentration_decompose(x)
  expect_equal(d$zero_share, exp(-0.4), tolerance = 5e-3)
  expect_lt(abs(d$excess_zero_share), 5e-3)
  # and its overall Gini is large even though every place has the same rate
  expect_gt(d$gini_all, 0.5)
})

test_that("heterogeneous rates show up as excess zero share and positive-place concentration", {
  set.seed(1)
  rates <- rgamma(200000, shape = 0.3, rate = 0.3 / 0.4)   # same mean 0.4, very unequal
  y <- rpois(200000, rates)
  d <- morie_concentration_decompose(y)
  expect_gt(d$excess_zero_share, 0.1)
  u <- morie_concentration_decompose(rpois(200000, 0.4))
  expect_gt(d$gini_positive, u$gini_positive)
})

test_that("argument checks", {
  set.seed(1)
  expect_error(morie_concentration_gini(c(0, 0)), "positive total")
  expect_error(morie_concentration_decompose(c(-1, 2)), "non-negative")
})

test_that("Poisson mixtures obey the proved inequalities (mixture_zero_ge_exp_neg_mean, mixture_var_ge_mean, variance_eq)", {
  set.seed(1)
  # exact finite mixtures: weights w, intensities lam
  for (k in 1:20) {
    m <- sample(2:6, 1)
    w <- runif(m); w <- w / sum(w)
    lam <- runif(m, 0, 4)
    mu <- sum(w * lam)
    zero <- sum(w * exp(-lam))
    expect_gte(zero, exp(-mu) - 1e-12)
    second <- sum(w * (lam + lam^2))
    v <- second - mu^2
    expect_gte(v, mu - 1e-12)
    expect_equal(v, mu + sum(w * (lam - mu)^2), tolerance = 1e-12)   # variance_eq
  }
  # equality iff constant intensity
  expect_equal(sum(c(0.3, 0.7) * (2 + 2^2)) - 2^2, 2)
  # a heterogeneous sample shows dispersion above one and excess zeros; a Poisson sample does not, on average
  x_het <- rpois(20000, rgamma(20000, shape = 2, rate = 2))
  d_het <- morie_concentration_dispersion(x_het)
  expect_gt(d_het$dispersion_index, 1.2)
  expect_gt(d_het$zero_share_gap, 0.02)
  expect_equal(d_het$implied_intensity_variance, d_het$variance - d_het$mean_count)
  x_poi <- rpois(20000, 1)
  d_poi <- morie_concentration_dispersion(x_poi)
  expect_lt(abs(d_poi$dispersion_index - 1), 0.05)
  expect_lt(abs(d_poi$zero_share_gap), 0.02)
  expect_error(morie_concentration_dispersion(c(0, 0)), "positive total")
  expect_true("Research.P7.Mixture.mixture_zero_ge_exp_neg_mean" %in% morie_concentration_decompose(x_het)$theorems)
})

test_that("distinct-place growth: the Polya expectation sits inside the proved envelope, and simulated Polya series stay near it (expectedDistinct_bounds)", {
  set.seed(4)
  for (M in c(0.5, 2, 10)) for (n in c(10, 100, 1000)) {
    S <- sum(1 / (M + 0:(n - 1)))
    expect_gte(M * S, M * log((M + n) / M) - 1e-12)
    expect_lte(M * S, 1 + M * log((M + n - 1) / M) + 1e-12)
  }
  sim_polya <- function(M, n) { place <- integer(n); k <- 0
    for (i in seq_len(n)) { if (stats::runif(1) < M / (M + i - 1)) { k <- k + 1; place[i] <- k } else place[i] <- place[sample.int(i - 1, 1)] }
    place }
  n <- 2000; M <- 4
  Ks <- replicate(200, max(morie_concentration_distinct_growth(sim_polya(M, n))$distinct))
  expect_gte(mean(Ks), M * log((M + n) / M) - 1); expect_lte(mean(Ks), 1 + M * log((M + n - 1) / M) + 1)
  g <- morie_concentration_distinct_growth(sim_polya(M, n))
  expect_true(all(g$lower <= g$expected + 1e-9) && all(g$expected <= g$upper + 1e-9))
  expect_lt(g$loglog_slope, 0.5)
  # a power-law series (every third event is a new place) has slope near 1
  pl <- rep(seq_len(700), each = 3)
  expect_gt(morie_concentration_distinct_growth(pl)$loglog_slope, 0.9)
  expect_error(morie_concentration_distinct_growth(1), "at least two")
})

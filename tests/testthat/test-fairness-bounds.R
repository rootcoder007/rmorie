# SPDX-License-Identifier: AGPL-3.0-or-later
# Research P5: the fairness functions must agree with research/lean/P5Fairness.lean.

set.seed(1)

test_that("Chouldechova's identity holds on any confusion table", {
  set.seed(1)
  for (i in 1:50) {
    cells <- stats::runif(4, 1, 500)
    r <- morie_fairness_rates(cells[1], cells[2], cells[3], cells[4])
    expect_equal(morie_fairness_implied_fpr(r$p, r$ppv, r$fnr), r$fpr, tolerance = 1e-12)
  }
})

test_that("equal ppv and fnr with different base rates force different fpr (impossibility)", {
  set.seed(1)
  f1 <- morie_fairness_implied_fpr(0.30, 0.6, 0.25)
  f2 <- morie_fairness_implied_fpr(0.50, 0.6, 0.25)
  expect_false(isTRUE(all.equal(f1, f2)))
  expect_gt(f2, f1)   # higher base rate, higher implied fpr, other things equal
})

test_that("base-rate bounds are sharp: the corner noise rates attain them", {
  set.seed(1)
  p <- 0.37; a <- 0.08; b <- 0.15
  # lower end: alpha = a, beta = 0
  p_obs_low <- p * (1 - 0) + (1 - p) * a
  bl <- morie_fairness_base_rate_bounds(p_obs_low, a, b)
  expect_equal(bl$lower, p, tolerance = 1e-12)
  # upper end: alpha = 0, beta = b
  p_obs_up <- p * (1 - b) + (1 - p) * 0
  bu <- morie_fairness_base_rate_bounds(p_obs_up, a, b)
  expect_equal(bu$upper, p, tolerance = 1e-12)
  # any admissible noise pair keeps the truth inside the interval
  for (i in 1:200) {
    al <- stats::runif(1, 0, a); be <- stats::runif(1, 0, b)
    p_obs <- p * (1 - be) + (1 - p) * al
    bb <- morie_fairness_base_rate_bounds(p_obs, a, b)
    expect_lte(bb$lower, p + 1e-12)
    expect_gte(bb$upper, p - 1e-12)
  }
})

test_that("known noise rates invert exactly", {
  set.seed(1)
  p <- c(0.1, 0.4, 0.8)
  p_obs <- p * (1 - 0.2) + (1 - p) * 0.1
  expect_equal(morie_fairness_true_rate(p_obs, 0.1, 0.2), p, tolerance = 1e-12)
})

test_that("argument checks", {
  set.seed(1)
  expect_error(morie_fairness_rates(0, 1, 1, 1), "positive")
  expect_error(morie_fairness_implied_fpr(1, 0.5, 0.1), "in \\(0, 1\\)")
  expect_error(morie_fairness_base_rate_bounds(0.5, 0.6, 0.5), "below 1")
  expect_error(morie_fairness_true_rate(0.5, 0.6, 0.5), "alpha \\+ beta")
})

test_that("group comparison is decided exactly when the intervals are disjoint (compare_decided / compare_undecided)", {
  set.seed(1)
  d <- morie_fairness_compare_groups(0.35, 0.55, alpha_max = 0.05, beta_max = 0.20)
  expect_true(d$decided); expect_equal(d$order, "a < b")
  # every admissible noise pair keeps the order
  for (i in 1:200) {
    aA <- stats::runif(1, 0, 0.05); bA <- stats::runif(1, 0, 0.20)
    aB <- stats::runif(1, 0, 0.05); bB <- stats::runif(1, 0, 0.20)
    pA <- morie_fairness_true_rate(0.35, aA, bA); pB <- morie_fairness_true_rate(0.55, aB, bB)
    expect_lt(pA, pB)
  }
  u <- morie_fairness_compare_groups(0.35, 0.55, alpha_max = 0.05, beta_max = 0.40)
  expect_false(u$decided)
  # and the witnesses from compare_undecided reverse the order
  pA_hi <- 0.35 / (1 - 0.40); pB_lo <- (0.55 - 0.05) / (1 - 0.05)
  expect_gt(pA_hi, pB_lo)
  # breakdown beta restores decidability just below it
  expect_false(is.na(u$breakdown_beta_max))
  r <- morie_fairness_compare_groups(0.35, 0.55, alpha_max = 0.05, beta_max = u$breakdown_beta_max - 1e-6)
  expect_true(r$decided)
})

test_that("dropping an orthogonal covariate rescales a logit coefficient by sqrt(s/(s+v)) (reduced_coefficient, ratio_is_rescaling)", {
  r <- morie_logit_rescale(0.8, omitted_var = 1)
  expect_equal(r$rescale, sqrt((pi^2 / 3) / (pi^2 / 3 + 1)))
  expect_lt(r$rescale, 1); expect_equal(morie_logit_rescale(0.8, 0)$rescale, 1)
  expect_equal(r$apparent_change, r$odds_ratio_reduced / r$odds_ratio_full, tolerance = 1e-12)
  # simulation: x and u independent, latent y* = b x + g u + logistic error
  set.seed(11)
  n <- 200000; x <- rnorm(n); u <- rnorm(n); b <- 0.8; g <- 1.2
  ystar <- b * x + g * u + rlogis(n); y <- as.integer(ystar > 0)
  full <- stats::glm(y ~ x + u, family = stats::binomial)
  red <- stats::glm(y ~ x, family = stats::binomial)
  ratio <- unname(stats::coef(red)["x"] / stats::coef(full)["x"])
  expect_equal(ratio, morie_logit_rescale(b, omitted_var = g^2)$rescale, tolerance = 0.03)
  expect_error(morie_logit_rescale(1, -1), "non-negative")
})

test_that("ranking resolution: reversal below 2 delta, stability above (rank_reversal_exists, rank_stable_of_gap)", {
  set.seed(6)
  for (k in 1:200) {
    t1 <- runif(1); t2 <- t1 + runif(1, 0, 0.5); d <- runif(1, 0, 0.3)
    if (t2 - t1 < 2 * d) {
      expect_lt(t2 - d, t1 + d)                       # admissible noise reverses the order
    } else if (t2 - t1 > 2 * d) {
      e1 <- runif(1, -d, d); e2 <- runif(1, -d, d)
      expect_lt(t1 + e1, t2 + e2)
    }
  }
  r <- morie_ranking_resolution(c(0.2, 0.35, 0.8), half_width = c(0.1, 0.1, 0.05))
  expect_false(r$identified_pairs[1, 2]); expect_true(r$identified_pairs[1, 3]); expect_true(r$identified_pairs[2, 3])
  expect_equal(r$share_unidentified, 1 / 3)
  expect_equal(r$resolution, 0.2)
  # Baldus-type situation: intervals covering most of [0, 1] identify no pair
  b <- morie_ranking_resolution(runif(20), half_width = 0.45)
  expect_equal(b$share_unidentified, 1)
  expect_error(morie_ranking_resolution(0.5, 0.1), "at least two")
})

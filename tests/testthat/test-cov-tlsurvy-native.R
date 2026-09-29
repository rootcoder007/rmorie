# SPDX-License-Identifier: AGPL-3.0-or-later
# Coverage -- R/tlsurvy_native.R (TMLE on adaptive surveys, van der
# Laan & Rose 2018, Ch. 29). Inclusion probabilities are proportional
# to the design base and sum to n within [floor, 1]; Poisson sampling
# keeps u < pi on the package stream; the Horvitz-Thompson mean and
# its Poisson-sampling variance are recomputed.

.ts_V <- c(1, 4, 2, 8, 0.5, 3, 6, 1.5)

test_that("inclusion probabilities are proportional, capped, floored and sum to n", {
  u <- morie_tlsurvy_inclusion_probabilities(.ts_V, 4, "uniform")
  expect_equal(u$pi, rep(0.5, 8))
  p <- morie_tlsurvy_inclusion_probabilities(.ts_V, 3, "proportional")
  base <- abs(.ts_V) + 1e-12
  expect_equal(p$pi, 3 * base / sum(base), tolerance = 1e-9)
  expect_equal(p$n_expected, 3, tolerance = 1e-9)
  # adaptive: the 8 would exceed 1 and is capped; the rest share the slack
  inf <- c(1, 1, 1, 30, 1, 1, 1, 0.001)
  a <- morie_tlsurvy_inclusion_probabilities(.ts_V, 4, "adaptive", influence = inf, floor = 0.05)
  expect_equal(a$pi[4], 1)
  expect_equal(a$pi[8], 0.05)
  expect_equal(sum(a$pi), 4, tolerance = 1e-9)
  expect_equal(a$pi[1], a$pi[2], tolerance = 1e-12)
  expect_true(all(a$pi >= 0.05 & a$pi <= 1))
  expect_error(morie_tlsurvy_inclusion_probabilities(.ts_V, 4, "cluster"), "design must be one of")
  expect_error(morie_tlsurvy_inclusion_probabilities(.ts_V, 9), "n must lie in 1..8")
  expect_error(morie_tlsurvy_inclusion_probabilities(.ts_V, 4, "adaptive"), "needs the expected influence")
  expect_error(morie_tlsurvy_inclusion_probabilities(.ts_V, 4, "adaptive", influence = 1:3), "3 influence values")
})

test_that("draw_sample selects u < pi on the package stream", {
  pi <- seq(0.1, 0.8, length.out = 8)
  e <- .ghc_rng(5)
  u <- .ghc_unif(e, 8L)
  s <- morie_tlsurvy_draw_sample(pi, 5)
  expect_identical(s$selected, which(u < pi))
  expect_equal(s$fraction, length(which(u < pi)) / 8)
  expect_error(morie_tlsurvy_draw_sample(rep(0, 4)), "selected nothing")
})

test_that("horvitz_thompson weights by 1/pi", {
  pi <- c(0.5, 0.25, 1, 0.2)
  y <- c(2, 4, 3, 1)
  r <- morie_tlsurvy_horvitz_thompson(y, pi, c(1, 2, 4))
  expect_equal(r$estimate, (4 + 16 + 5) / 4)
  expect_equal(r$se, sqrt((0.5 * 16 + 0.75 * 256 + 0.8 * 25) / 16), tolerance = 1e-12)
  expect_equal(morie_tlsurvy_horvitz_thompson(y, pi, 1:4, N = 10)$estimate, (4 + 16 + 3 + 5) / 10)
  expect_error(morie_tlsurvy_horvitz_thompson(y, c(0, pi[-1]), 1), "zero inclusion")
})

test_that("design_efficiency compares the uniform and adaptive HT errors", {
  inf <- abs(.ts_V - 2)
  r <- morie_tlsurvy_design_efficiency(.ts_V, inf, 4, seed = 2)
  ht <- function(d) {
    pr <- morie_tlsurvy_inclusion_probabilities(.ts_V, 4, d, if (d == "adaptive") inf)
    s <- morie_tlsurvy_draw_sample(pr$pi, 2)
    morie_tlsurvy_horvitz_thompson(.ts_V, pr$pi, s$selected)$se
  }
  expect_equal(r$uniform_se, ht("uniform"), tolerance = 1e-12)
  expect_equal(r$adaptive_se, ht("adaptive"), tolerance = 1e-12)
  expect_equal(r$ratio, r$adaptive_se / r$uniform_se)
})

test_that("adaptive_survey_tmle runs the estimator on the sample with 1/pi weights", {
  inf <- abs(.ts_V - 2) + 0.5
  est <- function(idx, w) list(estimate = sum(w * .ts_V[idx]) / 8, se = 0.1)
  for (fn in list(morie_tlsurvy_adaptive_survey_tmle, morie_tlsurvy_adaptivesurveytmle)) {
    r <- fn(.ts_V, inf, est, 4, seed = 7)
    pr <- morie_tlsurvy_inclusion_probabilities(.ts_V, 4, "adaptive", inf)
    s <- morie_tlsurvy_draw_sample(pr$pi, 7)
    expect_equal(r$estimate, sum(.ts_V[s$selected] / pr$pi[s$selected]) / 8, tolerance = 1e-12)
    expect_equal(r$se_estimator, 0.1)
    expect_equal(r$n_used, s$n)
  }
  r2 <- morie_tlsurvy_adaptive_survey_tmle(.ts_V, inf, function(i, w) list(estimate = 1), 4)
  expect_true(is.nan(r2$se_estimator))
})

test_that("morie_tlsurvy_cheatsheet states the design rule", {
  expect_match(morie_tlsurvy_cheatsheet(), "proportional to the expected INFLUENCE", fixed = TRUE)
})

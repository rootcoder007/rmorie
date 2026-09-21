# SPDX-License-Identifier: AGPL-3.0-or-later
# Research P4: the feedback-loop functions must agree numerically with the
# Lean theorems in research/lean/P4Feedback.lean and P4Limit.lean.

set.seed(1)

test_that("one-step drift matches Research.P4.naive_step_drift exactly", {
  set.seed(1)
  lam_a <- 0.3; lam_b <- 0.2; c_a <- 7; c_b <- 3
  mf <- morie_feedback_loop_meanfield(lam_a, lam_b, c_a, c_b, n_steps = 1)
  x <- c_a / (c_a + c_b)
  drift_formula <- x * (1 - x) * (lam_a - lam_b) /
    (c_a + c_b + lam_a * x + lam_b * (1 - x))
  expect_equal(mf$share_a[2] - mf$share_a[1], drift_formula, tolerance = 1e-12)
})

test_that("naive share is strictly increasing when lam_a > lam_b (naive_share_increasing)", {
  set.seed(1)
  mf <- morie_feedback_loop_meanfield(0.21, 0.20, 1, 99, n_steps = 500)
  expect_true(all(diff(mf$share_a) > 0))
  # and strictly decreasing with the rates swapped
  mf2 <- morie_feedback_loop_meanfield(0.20, 0.21, 99, 1, n_steps = 500)
  expect_true(all(diff(mf2$share_a) < 0))
})

test_that("naive share heads to 1 whatever the ratio (naiveShare_tendsto_one)", {
  set.seed(1)
  mf <- morie_feedback_loop_meanfield(0.3, 0.2, 10, 10, n_steps = 200000)
  expect_gt(tail(mf$share_a, 1), 0.9)
  expect_equal(attr(mf, "limit")$share_a, 1)
  expect_equal(attr(mf, "limit")$theorem, "Research.P4.naiveShare_tendsto_one")
})

test_that("the runaway is logarithmically slow: a 5 percent gap from a 1/99 start barely moves", {
  set.seed(1)
  # The limit is still 1 (theorem), but the drift is x(1-x)(lam_a-lam_b)/c_n with
  # c_n growing linearly, so the share moves like a harmonic sum: after two
  # million steps it has gone from 0.010 to about 0.015. Any claim that a
  # feedback loop "will" concentrate patrols must state the horizon.
  mf <- morie_feedback_loop_meanfield(0.21, 0.20, 1, 99, n_steps = 2e6)
  expect_true(all(diff(mf$share_a) > 0))
  expect_gt(tail(mf$share_a, 1), 0.012)
  expect_lt(tail(mf$share_a, 1), 0.03)
  expect_equal(attr(mf, "limit")$share_a, 1)
})

test_that("equal rates leave the share where it started (zero drift)", {
  set.seed(1)
  mf <- morie_feedback_loop_meanfield(0.25, 0.25, 30, 70, n_steps = 100)
  expect_equal(mf$share_a, rep(0.3, 101), tolerance = 1e-12)
  expect_equal(attr(mf, "limit")$share_a, 0.3)
})

test_that("corrected update converges to lam_a / (lam_a + lam_b) (corrected_share_tendsto)", {
  set.seed(1)
  mf <- morie_feedback_loop_meanfield(0.3, 0.2, 1, 99, n_steps = 20000, update = "corrected")
  expect_equal(tail(mf$share_a, 1), 0.6, tolerance = 1e-2)
  # closed form: x_n = (c_a0 + n lam_a) / (c0 + n (lam_a + lam_b))
  n <- 20000
  expect_equal(tail(mf$share_a, 1), (1 + n * 0.3) / (100 + n * 0.5), tolerance = 1e-12)
})

test_that("the urn simulator follows the proved mean-field limits", {
  set.seed(1)
  s <- morie_feedback_loop_sim(0.3, 0.2, 10, 10, n_steps = 20000, n_sims = 30, seed = 7)
  expect_equal(dim(s$share_a), c(30, 20001))
  expect_gt(stats::median(s$final), 0.8)
  expect_equal(s$limit$share_a, 1)
  cs <- morie_feedback_loop_sim(0.3, 0.2, 10, 10, n_steps = 20000, n_sims = 30,
                                update = "corrected", seed = 7)
  expect_lt(abs(stats::median(cs$final) - 0.6), 0.05)
})

test_that("public reports hold the naive loop back from the corner", {
  set.seed(1)
  s0 <- morie_feedback_loop_sim(0.3, 0.2, 10, 10, n_steps = 4000, n_sims = 30, seed = 3)
  s5 <- morie_feedback_loop_sim(0.3, 0.2, 10, 10, n_steps = 4000, n_sims = 30, seed = 3, rho = 0.5)
  expect_lt(stats::median(s5$final), stats::median(s0$final))
  expect_true(is.na(s5$limit$share_a))
})

test_that("argument checks refuse what the theorems do not cover", {
  set.seed(1)
  expect_error(morie_feedback_loop_meanfield(0, 0.2, 1, 1), "positive")
  expect_error(morie_feedback_loop_meanfield(0.3, 0.2, 0, 1), "positive")
  expect_error(morie_feedback_loop_meanfield(0.3, 0.2, 1, 1, rho = 2), "rho")
  expect_error(morie_feedback_loop_sim(1.5, 0.2, 1, 1), "<= 1")
  expect_error(morie_feedback_loop_limit("a", 0.2, 1, 1), "single non-missing number")
})

test_that("the proved rate bound holds along the recursion (naiveShare_rate_bound)", {
  set.seed(1)
  for (n in c(10L, 1000L, 100000L)) {
    mf <- morie_feedback_loop_meanfield(0.3, 0.2, 10, 10, n_steps = n)
    b <- morie_feedback_loop_bound(0.3, 0.2, 10, 10, n_steps = n)
    expect_lte(tail(mf$share_a, 1) - mf$share_a[1], b$bound)
    expect_lte(tail(mf$share_a, 1), b$share_a_max)
  }
  # the 1/99 case: the bound explains why two million steps cannot move the share far
  b <- morie_feedback_loop_bound(0.21, 0.20, 1, 99, n_steps = 2e6)
  expect_lt(b$bound, 0.15)
  expect_equal(b$theorem, "Research.P4.naiveShare_rate_bound")
  expect_error(morie_feedback_loop_bound(0.2, 0.3, 1, 1), "swap")
})

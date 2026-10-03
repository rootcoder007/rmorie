# SPDX-License-Identifier: AGPL-3.0-or-later
# Research P14 (continued): the slope test must agree with research/lean/P14Slope.lean.

test_that("nested judges on a population: propensity_mono, outcome_diff, slope_bound, no violations", {
  set.seed(14)
  for (rep in 1:40) {
    n <- sample(10:60, 1); J <- sample(2:5, 1)
    w <- runif(n, 0.5, 2)
    y0 <- runif(n); y1 <- runif(n)
    # nested detention sets: judge j detains everyone with severity above a falling cutoff
    sev <- runif(n); cut <- sort(runif(J), decreasing = TRUE)
    d <- sapply(cut, function(cc) as.numeric(sev > cc))                     # n x J, nested in j
    judge <- rep(seq_len(J), each = n)
    dd <- as.vector(d); yy <- as.vector(ifelse(d == 1, y1, y0)); ww <- rep(w, J)
    s <- morie_judge_slope_test(judge, dd, yy, weights = ww, lo = 0, hi = 1)
    P <- colSums(w * d) / sum(w); Y <- colSums(w * ifelse(d == 1, y1, y0)) / sum(w)
    expect_equal(s$judges$propensity, sort(P), tolerance = 1e-12)
    expect_true(all(diff(s$judges$propensity) >= -1e-15))                     # propensity_mono
    expect_equal(s$violations, 0L)
    expect_true(s$monotone_consistent)
    for (r in seq_len(nrow(s$pairs))) {
      j <- which(seq_len(J) == as.integer(s$pairs$j[r])); k <- which(seq_len(J) == as.integer(s$pairs$k[r]))
      marginal <- (d[, k] - d[, j]) * (y1 - y0)                                 # outcome_diff
      expect_equal(s$pairs$dY[r], sum(w * marginal) / sum(w), tolerance = 1e-10)
      expect_lte(abs(s$pairs$dY[r]), (1 - 0) * s$pairs$dP[r] + 1e-12)         # slope_bound
      if (s$pairs$dP[r] > 0) expect_equal(s$pairs$late[r], s$pairs$dY[r] / s$pairs$dP[r], tolerance = 1e-12)
    }
  }
})

test_that("violation_refutes_monotonicity: a pair steeper than the outcome range is flagged", {
  judge <- rep(c("A", "B", "C"), each = 6)
  d <- c(0, 0, 0, 1, 1, 0,  0, 1, 1, 1, 0, 1,  1, 1, 1, 1, 1, 0)
  y <- c(1, 0, 0, 1, 0, 0,  0, 1, 1, 0, 0, 1,  1, 1, 1, 1, 0, 1)
  s <- morie_judge_slope_test(judge, d, y)
  expect_equal(s$judges$judge, c("A", "B", "C"))
  expect_equal(s$judges$propensity, c(2, 4, 5) / 6, tolerance = 1e-12)
  expect_equal(s$judges$outcome, c(2, 3, 5) / 6, tolerance = 1e-12)
  expect_equal(s$pairs$violation, c(FALSE, FALSE, TRUE))
  expect_equal(s$violations, 1L)
  expect_false(s$monotone_consistent)
  expect_equal(s$pairs$late, c(0.5, 1, 2), tolerance = 1e-12)
  one <- morie_judge_slope_test(rep("A", 3), c(0, 1, 1), c(1, 0, 1))
  expect_null(one$pairs); expect_equal(one$violations, 0L)
})

test_that("input checks", {
  expect_error(morie_judge_slope_test(1:3, c(0, 1), c(1, 0, 1)), "equal length")
  expect_error(morie_judge_slope_test(c(1, NA, 1), c(0, 1, 1), c(1, 0, 1)), "missing")
  expect_error(morie_judge_slope_test(1:3, c(0, 2, 1), c(1, 0, 1)), "0/1")
  expect_error(morie_judge_slope_test(1:3, c(0, 1, 1), c(1, 0, 1), lo = 0.5), "lo, hi")
  expect_error(morie_judge_slope_test(1:3, c(0, 1, 1), c(1, 0, 1), weights = c(1, -1, 1)), "weights")
  expect_error(morie_judge_slope_test(c(1, 1, 2), c(0, 1, 1), c(1, 0, 1), weights = c(1, 1, 0)), "positive total")
})

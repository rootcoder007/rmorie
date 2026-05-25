# SPDX-License-Identifier: AGPL-3.0-or-later
library(testthat)

set.seed(1)

# ---------------------------------------------------------------------------
# noisy_or_detection
# ---------------------------------------------------------------------------

test_that("morie_fairness_noisy_or_detection basic", {
  set.seed(1)
  crimes <- matrix(runif(20), ncol = 2L)
  officers <- matrix(runif(10), ncol = 2L)
  r <- morie_fairness_noisy_or_detection(crimes, officers,
                                         radius = 0.5, p_detect = 0.7)
  expect_s3_class(r, "morie_fairness_result")
  expect_length(r$probabilities, 10L)
  expect_length(r$officers_in_range, 10L)
  expect_true(all(r$probabilities >= 0 & r$probabilities <= 1))
})

test_that("morie_fairness_noisy_or_detection samples when seed given", {
  set.seed(2)
  crimes <- matrix(runif(20), ncol = 2L)
  officers <- matrix(runif(10), ncol = 2L)
  r <- morie_fairness_noisy_or_detection(crimes, officers, radius = 0.5,
                                         seed = 42L)
  expect_length(r$detected, 10L)
  expect_true(all(r$detected %in% c(0L, 1L)))
})

test_that("morie_fairness_noisy_or_detection handles zero officers", {
  crimes <- matrix(runif(8), ncol = 2L)
  officers <- matrix(numeric(0), ncol = 2L)
  r <- morie_fairness_noisy_or_detection(crimes, officers, radius = 0.5)
  expect_equal(r$officers_in_range, integer(4L))
  expect_equal(r$probabilities, rep(0, 4L))
})

test_that("morie_fairness_noisy_or_detection rejects bad shape", {
  expect_error(
    morie_fairness_noisy_or_detection(matrix(1:9, ncol = 3L),
                                      matrix(1:4, ncol = 2L),
                                      radius = 1),
    "n, 2"
  )
  expect_error(
    morie_fairness_noisy_or_detection(matrix(1:4, ncol = 2L),
                                      matrix(1:9, ncol = 3L),
                                      radius = 1),
    "m, 2"
  )
})

test_that("morie_fairness_noisy_or_detection rejects bad p_detect/radius", {
  c1 <- matrix(runif(4), ncol = 2L)
  o1 <- matrix(runif(4), ncol = 2L)
  expect_error(morie_fairness_noisy_or_detection(c1, o1, radius = 1,
                                                 p_detect = 0),
               "in .0, 1.")
  expect_error(morie_fairness_noisy_or_detection(c1, o1, radius = 0),
               "positive")
})

# ---------------------------------------------------------------------------
# simulate_biased_crime_data
# ---------------------------------------------------------------------------

test_that("morie_fairness_simulate_biased_crime_data returns frame", {
  d <- morie_fairness_simulate_biased_crime_data(n = 100L, seed = 1L)
  expect_s3_class(d, "data.frame")
  expect_equal(nrow(d), 100L)
  expect_true(all(c("area","group","true_outcome",
                    "detected","risk_score") %in% names(d)))
  expect_true(all(d$risk_score >= 0 & d$risk_score <= 500))
})

test_that("morie_fairness_simulate_biased_crime_data injects disparity", {
  d <- morie_fairness_simulate_biased_crime_data(n = 800L, bias = 0.5,
                                                 seed = 2L)
  m_A <- mean(d$risk_score[d$group == "A"])
  m_B <- mean(d$risk_score[d$group == "B"])
  expect_gt(m_B, m_A)  # bias shifts non-reference upward
})

test_that("morie_fairness_simulate_biased_crime_data errors on bad inputs", {
  expect_error(morie_fairness_simulate_biased_crime_data(groups = "A"),
               "two groups")
  expect_error(morie_fairness_simulate_biased_crime_data(base_rate = 1.5),
               "base_rate")
  expect_error(morie_fairness_simulate_biased_crime_data(bias = 2),
               "bias")
  expect_error(morie_fairness_simulate_biased_crime_data(n_areas = 1L),
               "n_areas")
})

test_that("morie_fairness_simulate_biased_crime_data accepts group_props", {
  d <- morie_fairness_simulate_biased_crime_data(
    n = 300L, group_props = c(0.7, 0.3), seed = 3L
  )
  expect_true(mean(d$group == "A") > 0.5)
})

test_that("morie_fairness_simulate_biased_crime_data wrong props errors", {
  expect_error(
    morie_fairness_simulate_biased_crime_data(group_props = c(0.5, 0.3, 0.2)),
    "one entry per group"
  )
})
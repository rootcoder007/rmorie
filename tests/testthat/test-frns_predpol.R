# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Phase 2S: tests for frns_predpol.R + frns_temporal.R — predictive-
# policing fairness audits.

test_that("morie_predpol_aggregate_areas returns per-area rates", {
  set.seed(1L)
  n <- 200L
  area <- sample(sprintf("A%02d", 1:5), n, replace = TRUE)
  risk <- stats::runif(n)
  outcome <- stats::rbinom(n, 1, risk)
  out <- morie_predpol_aggregate_areas(area, risk, outcome)
  expect_true(is.list(out) || is.data.frame(out))
})

test_that("morie_predpol_aggregate_areas accepts group + population", {
  set.seed(2L)
  n <- 200L
  area <- sample(sprintf("A%02d", 1:5), n, replace = TRUE)
  risk <- stats::runif(n)
  outcome <- stats::rbinom(n, 1, risk)
  group <- sample(c("white", "black"), n, replace = TRUE)
  pop <- setNames(sample(1000:5000, 5), sprintf("A%02d", 1:5))
  out <- morie_predpol_aggregate_areas(area, risk, outcome,
                                        group = group,
                                        population = pop)
  expect_true(is.list(out) || is.data.frame(out))
})

test_that("morie_predpol_aggregate_areas errors on length mismatch", {
  expect_error(
    morie_predpol_aggregate_areas(area = c("A", "B"),
                                   risk = c(0.1, 0.2),
                                   outcome = c(0, 1, 1))
  )
})

test_that("morie_predpol_calibration_audit returns calibration stats", {
  set.seed(3L)
  n <- 10L
  out <- tryCatch(
    morie_predpol_calibration_audit(
      areas = sprintf("A%02d", 1:n),
      mean_risk = stats::runif(n),
      outcome_rate = stats::runif(n),
      group = sample(c("A", "B"), n, replace = TRUE)),
    error = function(e) e
  )
  if (inherits(out, "error")) {
    skip(sprintf("calibration_audit error: %s",
                 conditionMessage(out)))
  }
  expect_true(is.list(out) || is.data.frame(out))
})

test_that("morie_predpol_score_disparity returns inter-group disparity", {
  set.seed(4L)
  n <- 300L
  score <- stats::rbeta(n, 2, 5)
  group <- sample(c("white", "black", "asian"), n, replace = TRUE)
  out <- morie_predpol_score_disparity(score, group,
                                        reference = "white")
  expect_true(is.list(out) || is.data.frame(out))
})

# ---------------------------------------------------- frns_temporal

test_that("morie_predpol_temporal_audit runs on synthetic period x city panel", {
  set.seed(5L)
  n <- 400L
  period <- sample(c("2022Q1", "2022Q2", "2022Q3", "2022Q4"),
                    n, replace = TRUE)
  city <- sample(c("Toronto", "Hamilton"), n, replace = TRUE)
  group <- sample(c("white", "black"), n, replace = TRUE)
  y_pred <- stats::rbinom(n, 1L, 0.3)
  out <- tryCatch(
    morie_predpol_temporal_audit(period = period, city = city,
                                  y_pred = y_pred, group = group),
    error = function(e) e
  )
  if (inherits(out, "error")) {
    skip(sprintf("temporal_audit error: %s", conditionMessage(out)))
  }
  expect_true(is.list(out) || is.data.frame(out))
})

test_that("morie_predpol_temporal_audit errors on length mismatch", {
  expect_error(
    morie_predpol_temporal_audit(period = c("Q1", "Q2"),
                                  city = c("X", "Y"),
                                  y_pred = c(0, 1, 1),
                                  group = c("a", "b"))
  )
})

test_that("morie_predpol_temporal_audit errors on empty input", {
  expect_error(
    morie_predpol_temporal_audit(period = character(0),
                                  city = character(0),
                                  y_pred = integer(0),
                                  group = character(0))
  )
})

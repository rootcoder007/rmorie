# SPDX-License-Identifier: AGPL-3.0-or-later
library(testthat)

set.seed(1)

make_predpol_records <- function(n = 200, seed = 1) {
  set.seed(seed)
  area <- sample(sprintf("a%02d", 1:5), n, replace = TRUE)
  risk <- runif(n, 0, 1)
  outcome <- rbinom(n, 1, plogis(risk - 0.5))
  group <- sample(c("X", "Y"), n, replace = TRUE)
  data.frame(area = area, risk = risk, outcome = outcome,
             group = group, stringsAsFactors = FALSE)
}

# ---------------------------------------------------------------------------
# aggregate_areas
# ---------------------------------------------------------------------------

test_that("morie_fairness_predpol_aggregate_areas basic roll-up", {
  set.seed(1); d <- make_predpol_records()
  r <- morie_fairness_predpol_aggregate_areas(d$area, d$risk, d$outcome,
                                              group = d$group)
  expect_true(all(c("areas","mean_risk","outcome_rate",
                    "group","n_records") %in% names(r)))
  expect_equal(length(r$areas), 5L)
  expect_equal(length(r$mean_risk), 5L)
  expect_equal(sum(r$n_records), nrow(d))
})

test_that("morie_fairness_predpol_aggregate_areas without group", {
  set.seed(2); d <- make_predpol_records()
  r <- morie_fairness_predpol_aggregate_areas(d$area, d$risk, d$outcome)
  expect_null(r$group)
})

test_that("morie_fairness_predpol_aggregate_areas with population named vector", {
  set.seed(3); d <- make_predpol_records()
  pop <- setNames(c(1000, 2000, 1500, 800, 1200), sprintf("a%02d", 1:5))
  r <- morie_fairness_predpol_aggregate_areas(d$area, d$risk, d$outcome,
                                              population = pop)
  expect_true(all(is.finite(r$outcome_rate) | is.na(r$outcome_rate)))
})

test_that("morie_fairness_predpol_aggregate_areas per-record population", {
  set.seed(4); d <- make_predpol_records()
  pop_per <- ifelse(d$area == "a01", 1000, 2000)
  r <- morie_fairness_predpol_aggregate_areas(d$area, d$risk, d$outcome,
                                              population = pop_per)
  expect_true(all(is.finite(r$outcome_rate)))
})

test_that("morie_fairness_predpol_aggregate_areas errors on length mismatch", {
  expect_error(
    morie_fairness_predpol_aggregate_areas(c("a","b"), c(1), c(0,1)),
    "same length"
  )
})

test_that("morie_fairness_predpol_aggregate_areas errors on bad group len", {
  set.seed(5); d <- make_predpol_records()
  expect_error(
    morie_fairness_predpol_aggregate_areas(d$area, d$risk, d$outcome,
                                           group = c("X","Y")),
    "same length as area"
  )
})

# ---------------------------------------------------------------------------
# calibration_audit
# ---------------------------------------------------------------------------

test_that("morie_fairness_predpol_calibration_audit basic", {
  set.seed(6)
  areas <- paste0("a", 1:8)
  mean_risk <- seq(0.1, 0.9, length.out = 8L)
  outcome_rate <- mean_risk + rnorm(8L, 0, 0.05)
  group <- rep(c("X","Y"), each = 4L)
  r <- morie_fairness_predpol_calibration_audit(areas, mean_risk,
                                                outcome_rate, group)
  expect_s3_class(r, "morie_fairness_result")
  expect_true(is.finite(r$value))
  expect_true(is.finite(r$spearman))
  expect_true(r$spearman > 0)
})

test_that("morie_fairness_predpol_calibration_audit errors on alignment", {
  expect_error(
    morie_fairness_predpol_calibration_audit(c("a"), c(1), c(1), c("X")),
    "at least two areas"
  )
})

test_that("morie_fairness_predpol_calibration_audit warns on non-finite", {
  areas <- paste0("a", 1:6)
  mean_risk <- c(0.1, NA_real_, 0.3, 0.4, 0.5, 0.6)
  outcome_rate <- c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6)
  group <- rep(c("X","Y"), 3L)
  r <- morie_fairness_predpol_calibration_audit(areas, mean_risk,
                                                outcome_rate, group)
  expect_true(length(r$warnings) >= 1L)
})

test_that("morie_fairness_predpol_calibration_audit handles constant risk", {
  areas <- paste0("a", 1:4)
  r <- morie_fairness_predpol_calibration_audit(
    areas, rep(0.5, 4), c(0.1, 0.2, 0.3, 0.4),
    rep(c("X","Y"), each = 2L)
  )
  expect_true(length(r$warnings) >= 1L)
})

# ---------------------------------------------------------------------------
# score_disparity
# ---------------------------------------------------------------------------

test_that("morie_fairness_predpol_score_disparity returns spread", {
  set.seed(7)
  n <- 80L
  group <- rep(c("X","Y"), each = n / 2L)
  score <- ifelse(group == "X", rnorm(n, 0, 1), rnorm(n, 2, 1))
  r <- morie_fairness_predpol_score_disparity(score, group)
  expect_s3_class(r, "morie_fairness_result")
  expect_gt(r$value, 0)
  expect_true(is.finite(r$anova_f))
  expect_true(is.finite(r$anova_pvalue))
})

test_that("morie_fairness_predpol_score_disparity accepts reference", {
  set.seed(8)
  score <- c(rnorm(20, 0), rnorm(20, 2))
  grp <- rep(c("X","Y"), each = 20L)
  r <- morie_fairness_predpol_score_disparity(score, grp, reference = "X")
  expect_equal(r$reference, "X")
})

test_that("morie_fairness_predpol_score_disparity errors on bad reference", {
  set.seed(9)
  score <- c(rnorm(20, 0), rnorm(20, 2))
  grp <- rep(c("X","Y"), each = 20L)
  expect_error(
    morie_fairness_predpol_score_disparity(score, grp, reference = "Z"),
    "not found"
  )
})

test_that("morie_fairness_predpol_score_disparity errors single group", {
  expect_error(
    morie_fairness_predpol_score_disparity(c(1,2,3), c("A","A","A")),
    "at least two groups"
  )
})

test_that("morie_fairness_predpol_score_disparity warns on non-finite scores", {
  set.seed(10)
  score <- c(rnorm(15, 0), NaN, rnorm(14, 2))
  grp <- rep(c("X","Y"), each = 15L)
  r <- morie_fairness_predpol_score_disparity(score, grp)
  expect_true(length(r$warnings) >= 1L)
})
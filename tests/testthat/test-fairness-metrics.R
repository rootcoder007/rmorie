# SPDX-License-Identifier: AGPL-3.0-or-later
library(testthat)

set.seed(1)

make_fair_df <- function(n = 200, seed = 1) {
  set.seed(seed)
  group <- sample(c("A", "B"), n, replace = TRUE, prob = c(0.5, 0.5))
  # Inject disparity: A has higher favourable rate
  base_p <- ifelse(group == "A", 0.7, 0.4)
  y_true <- rbinom(n, 1, base_p)
  noise <- rbinom(n, 1, 0.15)
  y_pred <- ifelse(noise == 1, 1 - y_true, y_true)
  score <- pmin(pmax(rnorm(n, mean = ifelse(group == "A", 0.6, 0.4), sd = 0.15), 0), 1)
  data.frame(y_true = y_true, y_pred = y_pred,
             group = group, score = score, stringsAsFactors = FALSE)
}

# ---------------------------------------------------------------------------
# disparate_impact
# ---------------------------------------------------------------------------

test_that("morie_fairness_disparate_impact returns rich result", {
  set.seed(1); d <- make_fair_df()
  r <- morie_fairness_disparate_impact(d$y_pred, d$group, privileged = "A")
  expect_true(is.list(r))
  expect_true("value" %in% names(r))
  expect_true(is.finite(r$value))
  expect_lte(r$value, 1.0 + 1e-9)
  expect_equal(r$privileged, "A")
  expect_true(is.logical(r$adverse_impact))
})

test_that("morie_fairness_disparate_impact infers privileged with warning", {
  set.seed(1); d <- make_fair_df()
  r <- morie_fairness_disparate_impact(d$y_pred, d$group)
  expect_true(length(r$warnings) >= 1L)
  expect_true(nzchar(r$privileged))
})

test_that("morie_fairness_disparate_impact errors on single group", {
  expect_error(morie_fairness_disparate_impact(c(1, 0, 1), c("A","A","A")),
               "at least two groups")
})

test_that("morie_fairness_disparate_impact errors on bad privileged", {
  set.seed(1); d <- make_fair_df()
  expect_error(morie_fairness_disparate_impact(d$y_pred, d$group, privileged = "Z"),
               "not found")
})

test_that("morie_fairness_disparate_impact handles zero base rate", {
  yp <- c(0,0,0,0, 1,1,0,0)
  grp <- c("A","A","A","A", "B","B","B","B")
  r <- morie_fairness_disparate_impact(yp, grp, privileged = "A")
  expect_true(is.nan(r$value) || !is.finite(r$value))
  expect_true(length(r$warnings) >= 1L)
})

test_that("morie_fairness_disparate_impact errors on length mismatch", {
  expect_error(morie_fairness_disparate_impact(c(1,0,1), c("A","B")),
               "length mismatch")
})

test_that("morie_fairness_disparate_impact errors on empty", {
  expect_error(morie_fairness_disparate_impact(integer(0), character(0)),
               "empty")
})

# ---------------------------------------------------------------------------
# demographic_parity
# ---------------------------------------------------------------------------

test_that("morie_fairness_demographic_parity returns gap structure", {
  set.seed(2); d <- make_fair_df()
  r <- morie_fairness_demographic_parity(d$y_pred, d$group, privileged = "A")
  expect_true(is.list(r))
  expect_true(is.finite(r$value))
  expect_equal(r$privileged, "A")
  expect_true("gaps" %in% names(r))
})

test_that("morie_fairness_demographic_parity zero gap when identical", {
  yp <- c(1,1,0,0, 1,1,0,0)
  grp <- c("A","A","A","A", "B","B","B","B")
  r <- morie_fairness_demographic_parity(yp, grp, privileged = "A")
  expect_equal(unname(abs(r$value)), 0, tolerance = 1e-9)
})

test_that("morie_fairness_demographic_parity inferred ref emits warning", {
  set.seed(3); d <- make_fair_df()
  r <- morie_fairness_demographic_parity(d$y_pred, d$group)
  expect_true(length(r$warnings) >= 1L)
})

# ---------------------------------------------------------------------------
# equalized_odds
# ---------------------------------------------------------------------------

test_that("morie_fairness_equalized_odds returns TPR/FPR gaps", {
  set.seed(4); d <- make_fair_df()
  r <- morie_fairness_equalized_odds(d$y_true, d$y_pred, d$group,
                                     privileged = "A")
  expect_true(is.list(r))
  expect_true(is.finite(r$value))
  expect_true(is.logical(r$violation))
  expect_true(all(c("tpr_gaps","fpr_gaps","rates") %in% names(r)))
})

test_that("morie_fairness_equalized_odds errors on length mismatch", {
  expect_error(morie_fairness_equalized_odds(c(1,0), c(1,1,0), c("A","B","A")),
               "length mismatch")
})

test_that("morie_fairness_equalized_odds warns on missing-positive group", {
  # Group B has no positives -> TPR NA
  yt <- c(1,1,0,0, 0,0,0,0)
  yp <- c(1,0,0,0, 0,1,0,0)
  grp <- c("A","A","A","A", "B","B","B","B")
  r <- morie_fairness_equalized_odds(yt, yp, grp, privileged = "A")
  expect_true(length(r$warnings) >= 1L)
})

# ---------------------------------------------------------------------------
# average_odds_difference
# ---------------------------------------------------------------------------

test_that("morie_fairness_average_odds_difference returns AOD", {
  set.seed(5); d <- make_fair_df()
  r <- morie_fairness_average_odds_difference(
    d$y_true, d$y_pred, d$group, privileged = "A"
  )
  expect_true(is.list(r))
  expect_true(is.finite(r$value))
  expect_true("average_odds_difference" %in% names(r))
})

test_that("morie_fairness_average_odds_difference single-group errors", {
  expect_error(morie_fairness_average_odds_difference(c(1,0,1), c(1,0,1),
                                                     c("A","A","A")),
               "at least two groups")
})

# ---------------------------------------------------------------------------
# gini
# ---------------------------------------------------------------------------

test_that("morie_fairness_gini equality has Gini 0", {
  r <- morie_fairness_gini(c(5, 5, 5, 5))
  expect_equal(r$value, 0, tolerance = 1e-9)
})

test_that("morie_fairness_gini high concentration", {
  r <- morie_fairness_gini(c(0, 0, 0, 100))
  expect_gt(r$value, 0.5)
})

test_that("morie_fairness_gini per-group breakdown when group supplied", {
  vals <- c(1,1,1, 0,0,10)
  grp  <- c("A","A","A", "B","B","B")
  r <- morie_fairness_gini(vals, grp)
  expect_true(length(r$per_group) == 2L)
  expect_true(all(c("A","B") %in% names(r$per_group)))
})

test_that("morie_fairness_gini warns on negatives", {
  r <- morie_fairness_gini(c(-1, 1, 2))
  expect_true(length(r$warnings) >= 1L)
})

test_that("morie_fairness_gini handles all-zero", {
  r <- morie_fairness_gini(c(0, 0, 0))
  expect_equal(r$value, 0)
})

# ---------------------------------------------------------------------------
# bias_amplification
# ---------------------------------------------------------------------------

test_that("morie_fairness_bias_amplification returns BAS", {
  set.seed(6); d <- make_fair_df()
  r <- morie_fairness_bias_amplification(d$y_pred, d$group, privileged = "A")
  expect_true(is.list(r))
  expect_true(is.finite(r$value))
  expect_true(all(c("demographic_parity_gap","gini") %in% names(r)))
})

test_that("morie_fairness_bias_amplification zero on parity", {
  yp <- c(1,0,1,0, 1,0,1,0)
  grp <- c("A","A","A","A", "B","B","B","B")
  r <- morie_fairness_bias_amplification(yp, grp, privileged = "A")
  expect_equal(unname(abs(r$value)), 0, tolerance = 1e-9)
})

# ---------------------------------------------------------------------------
# print method (only if S3 class exists)
# ---------------------------------------------------------------------------

test_that("printing a fairness result runs", {
  set.seed(7); d <- make_fair_df()
  r <- morie_fairness_demographic_parity(d$y_pred, d$group, privileged = "A")
  # plain list -> use default print without error
  expect_silent({ invisible(capture.output(print(r))) })
})

# SPDX-License-Identifier: AGPL-3.0-or-later
# Module 1 structural tests: native nearest-neighbour matching.
# No MatchIt anywhere in here — this is the zero-Suggests path.

.dgp_confounded <- function(n = 400L, seed = 42L) {
  set.seed(seed)
  x1 <- rnorm(n); x2 <- rnorm(n); x3 <- rbinom(n, 1, 0.4)
  # minority-treated design: matching must SELECT comparable
  # controls from a large pool for balance to be testable
  ps <- plogis(-1.7 + 0.4 * x1 - 0.35 * x2 + 0.4 * x3)
  d <- rbinom(n, 1, ps)
  y <- 1.0 * d + 0.8 * x1 + 0.5 * x2 + rnorm(n)
  data.frame(y = y, d = d, x1 = x1, x2 = x2, x3 = x3)
}

.smd <- function(df, treat, var) {
  a <- df[[var]][df[[treat]] == 1]; b <- df[[var]][df[[treat]] == 0]
  (mean(a) - mean(b)) / sqrt((stats::var(a) + stats::var(b)) / 2)
}

test_that("native nearest matching returns the morie_match_result shape", {
  d <- .dgp_confounded()
  r <- morie_matching_nearest_neighbor(d, "d", c("x1", "x2", "x3"))
  expect_s3_class(r, "morie_match_result")
  expect_true(all(c("matched_data", "n_treated", "n_matched_control",
                    "match_pairs", "method", "details") %in% names(r)))
  expect_identical(r$method, "nearest_neighbor (morie native)")
  expect_true(all(c("treated_idx", "control_idx", "distance") %in%
                    names(r$match_pairs)))
  expect_true(all(r$match_pairs$distance >= 0))
})

test_that("without replacement no control is reused", {
  d <- .dgp_confounded()
  r <- morie_matching_nearest_neighbor(d, "d", c("x1", "x2", "x3"))
  expect_false(any(duplicated(r$match_pairs$control_idx)))
})

test_that("1:2 matching yields up to two controls per treated", {
  d <- .dgp_confounded(n = 600L)
  r <- morie_matching_nearest_neighbor(d, "d", c("x1", "x2", "x3"),
                                       n_neighbors = 2L)
  per_treated <- table(r$match_pairs$treated_idx)
  expect_true(all(per_treated <= 2L))
  expect_gt(sum(per_treated == 2L), 0L)
})

test_that("matching improves covariate balance on a confounded DGP", {
  d <- .dgp_confounded(n = 4000L, seed = 7L)
  r <- morie_matching_nearest_neighbor(d, "d", c("x1", "x2", "x3"))
  md <- r$matched_data
  for (v in c("x1", "x2", "x3")) {
    expect_lt(abs(.smd(md, "d", v)), abs(.smd(d, "d", v)) + 1e-9)
    expect_lt(abs(.smd(md, "d", v)), 0.1)
  }
})

test_that("caliper drops distant treated units", {
  d <- .dgp_confounded(n = 500L, seed = 3L)
  r_all <- morie_matching_nearest_neighbor(d, "d", c("x1", "x2", "x3"))
  r_cal <- morie_matching_nearest_neighbor(d, "d", c("x1", "x2", "x3"),
                                           caliper = 0.05)
  expect_lte(nrow(r_cal$match_pairs), nrow(r_all$match_pairs))
  expect_true(all(r_cal$match_pairs$distance <=
                    0.05 * r_cal$details$propensity_logit_sd + 1e-12))
})

test_that("with replacement controls may repeat and every treated matches", {
  d <- .dgp_confounded(n = 300L, seed = 9L)
  r <- morie_matching_nearest_neighbor(d, "d", c("x1", "x2", "x3"),
                                       replace = TRUE)
  expect_equal(r$n_treated, sum(stats::complete.cases(
    d[, c("d", "x1", "x2", "x3")]) & d$d == 1))
})

# --- Module 2: native Mahalanobis matching ---

test_that("native mahalanobis matching returns the result shape", {
  d <- .dgp_confounded(n = 500L, seed = 21L)
  r <- morie_matching_mahalanobis(d, "d", c("x1", "x2", "x3"))
  expect_s3_class(r, "morie_match_result")
  expect_identical(r$method, "mahalanobis (morie native)")
  expect_false(any(duplicated(r$match_pairs$control_idx)))
  expect_true(all(r$match_pairs$distance >= 0))
})

test_that("mahalanobis matching improves balance on weak confounding", {
  d <- .dgp_confounded(n = 4000L, seed = 22L)
  r <- morie_matching_mahalanobis(d, "d", c("x1", "x2", "x3"))
  md <- r$matched_data
  for (v in c("x1", "x2", "x3")) {
    expect_lt(abs(.smd(md, "d", v)), 0.1)
  }
})

test_that("mahalanobis exact strata never cross", {
  d <- .dgp_confounded(n = 800L, seed = 23L)
  d$g <- sample(c("a", "b"), nrow(d), replace = TRUE)
  r <- morie_matching_mahalanobis(d, "d", c("x1", "x2"), exact = "g")
  p <- r$match_pairs
  expect_true(all(d[p$treated_idx, "g"] == d[p$control_idx, "g"]))
})

test_that("mahalanobis caliper bounds pair distances", {
  d <- .dgp_confounded(n = 800L, seed = 24L)
  r <- morie_matching_mahalanobis(d, "d", c("x1", "x2", "x3"),
                                  caliper = 0.3)
  expect_true(all(r$match_pairs$distance <= 0.3 + 1e-12))
})

# SPDX-License-Identifier: AGPL-3.0-or-later
# Structural tests for the native matching engines (modules 1-4).
# No MatchIt anywhere in here — this is the zero-Suggests path.

#' @srrstats {G5.4} Correctness of every native matching engine is
#'   additionally tested against MatchIt as the reference
#'   implementation in tests/cross/test-morie_vs_matchit.R (identical
#'   matched-set sizes; ATT/ATE agreement within stated tolerances;
#'   pair-for-pair identity on tie-free data for module 1).
#' @srrstats {G5.5} Correctness tests here run with fixed seeds via
#'   the .dgp_confounded()/.dgp_discrete() generators.
#' @srrstats {G5.6} Implementation recovers the known simulated
#'   treatment effect (cross tests assert proximity to the true
#'   effect) and known balance properties (weighted SMD bounds below).
#' @srrstats {G5.9b} Property invariants are re-run across multiple
#'   random seeds in tests/property/test-properties-matching.R.
#' @noRd
NULL

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
  expect_identical(r$method, "nearest_neighbor (rmorie native)")
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
  expect_identical(r$method, "mahalanobis (rmorie native)")
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

# ---- module 3: native exact matching ------------------------------------

.dgp_discrete <- function(n = 600L, seed = 7L) {
  set.seed(seed)
  region <- sample(c("N", "S", "E", "W"), n, replace = TRUE)
  year <- sample(2019:2021, n, replace = TRUE)
  ps <- plogis(-1 + 0.5 * (region == "N") + 0.3 * (year == 2020))
  d <- rbinom(n, 1, ps)
  y <- 1.5 * d + (region == "N") + 0.5 * (year == 2020) + rnorm(n)
  data.frame(region = region, year = year, d = d, y = y,
             stringsAsFactors = FALSE)
}

test_that("native exact matching keeps only two-arm strata", {
  df <- .dgp_discrete()
  res <- morie_matching_exact(df, "d", c("region", "year"))
  expect_s3_class(res$matched_data, "data.frame")
  expect_identical(res$method, "exact (rmorie native)")
  md <- res$matched_data
  key <- paste(md$region, md$year)
  for (k in unique(key)) {
    expect_setequal(unique(md$d[key == k]), c(0, 1))
  }
})

test_that("native exact matching weights follow the CEM convention", {
  df <- .dgp_discrete()
  res <- morie_matching_exact(df, "d", c("region", "year"))
  md <- res$matched_data
  expect_true(all(md$weights[md$d == 1] == 1))
  expect_equal(sum(md$weights[md$d == 0]), sum(md$d == 0), tolerance = 1e-8)
  expect_true(all(md$weights > 0))
  expect_s3_class(md$subclass, "factor")
})

test_that("native exact matching balances discrete covariates exactly (weighted)", {
  df <- .dgp_discrete(n = 2000L)
  res <- morie_matching_exact(df, "d", c("region", "year"))
  md <- res$matched_data
  ct <- md[md$d == 1, ]
  cc <- md[md$d == 0, ]
  for (lev in unique(md$region)) {
    pt <- mean(ct$region == lev)
    pc <- sum((cc$region == lev) * cc$weights) / sum(cc$weights)
    expect_equal(pt, pc, tolerance = 1e-8)
  }
})

# ---- module 4: native CEM ------------------------------------------------

test_that("native CEM returns the contract shape with L1 diagnostic", {
  df <- .dgp_confounded(n = 800L)
  res <- morie_matching_cem(df, "d", c("x1", "x2", "x3"), n_bins = 4L)
  expect_identical(res$method, "cem (rmorie native)")
  expect_true(res$n_treated > 0)
  expect_true(res$n_matched_control > 0)
  expect_true(is.numeric(res$details$l1_before))
  expect_gte(res$details$l1_before, 0)
  expect_lte(res$details$l1_before, 1)
  expect_true(all(c("weights", "subclass") %in% names(res$matched_data)))
})

test_that("native CEM reduces covariate imbalance (weighted SMD < 0.1)", {
  df <- .dgp_confounded(n = 4000L)
  res <- morie_matching_cem(df, "d", c("x1", "x2", "x3"), n_bins = 5L)
  md <- res$matched_data
  wmean <- function(x, w) sum(x * w) / sum(w)
  for (v in c("x1", "x2", "x3")) {
    smd_pre <- abs(.smd(df, "d", v))
    t_mean <- mean(md[[v]][md$d == 1])
    c_mean <- wmean(md[[v]][md$d == 0], md$weights[md$d == 0])
    s <- stats::sd(df[[v]][df$d == 1])
    smd_post <- abs((t_mean - c_mean) / s)
    expect_lt(smd_post, smd_pre + 1e-12)
    expect_lt(smd_post, 0.1)
  }
})

test_that("native CEM honours per-variable n_bins list with Sturges fallback", {
  df <- .dgp_confounded(n = 600L)
  res <- morie_matching_cem(df, "d", c("x1", "x2"), n_bins = list(x1 = 3L))
  expect_identical(res$method, "cem (rmorie native)")
  expect_true(res$n_treated > 0)
})

test_that("native CEM with coarser bins retains more units", {
  df <- .dgp_confounded(n = 1000L)
  fine <- morie_matching_cem(df, "d", c("x1", "x2", "x3"), n_bins = 8L)
  coarse <- morie_matching_cem(df, "d", c("x1", "x2", "x3"), n_bins = 2L)
  expect_gte(nrow(coarse$matched_data), nrow(fine$matched_data))
})

# ---- module 5: native optimal pair matching ------------------------------

test_that("native optimal matching returns the result shape (propensity)", {
  d <- .dgp_confounded(n = 500L, seed = 31L)
  r <- morie_matching_optimal_pair(d, "d", c("x1", "x2", "x3"))
  expect_s3_class(r, "morie_match_result")
  expect_identical(r$method, "optimal_pair (rmorie native)")
  expect_equal(r$n_treated, sum(stats::complete.cases(
    d[, c("d", "x1", "x2", "x3")]) & d$d == 1))
  expect_false(any(duplicated(r$match_pairs$control_idx)))
  expect_true(all(r$match_pairs$distance >= 0))
  expect_true(is.numeric(r$details$total_distance))
})

test_that("optimal total distance <= greedy total distance", {
  d <- .dgp_confounded(n = 800L, seed = 32L)
  opt <- morie_matching_optimal_pair(d, "d", c("x1", "x2", "x3"))
  grd <- morie_matching_nearest_neighbor(d, "d", c("x1", "x2", "x3"))
  # same estimand only when greedy matched every treated unit
  skip_if(nrow(grd$match_pairs) != nrow(opt$match_pairs))
  expect_lte(sum(opt$match_pairs$distance),
             sum(grd$match_pairs$distance) + 1e-9)
})

test_that("native optimal mahalanobis mode matches every treated unit", {
  d <- .dgp_confounded(n = 400L, seed = 33L)
  r <- morie_matching_optimal_pair(d, "d", c("x1", "x2", "x3"),
                                   distance = "mahalanobis")
  expect_identical(r$details$engine, "native-optimal-assignment")
  expect_equal(r$n_treated, nrow(r$match_pairs))
  expect_false(any(duplicated(r$match_pairs$control_idx)))
})

test_that("native optimal improves balance on a confounded DGP", {
  d <- .dgp_confounded(n = 3000L, seed = 34L)
  r <- morie_matching_optimal_pair(d, "d", c("x1", "x2", "x3"))
  md <- r$matched_data
  for (v in c("x1", "x2", "x3")) {
    expect_lt(abs(.smd(md, "d", v)), 0.1)
  }
})

# ---- module 6: native genetic matching -----------------------------------

test_that("native genetic matching returns the result shape", {
  d <- .dgp_confounded(n = 400L, seed = 51L)
  r <- morie_matching_genetic(d, "d", c("x1", "x2", "x3"),
                              pop_size = 12L, n_generations = 4L)
  expect_s3_class(r, "morie_match_result")
  expect_identical(r$method, "genetic (rmorie native)")
  expect_named(r$details$best_weights, c("x1", "x2", "x3"))
  expect_true(all(r$details$best_weights > 0))
  expect_false(any(duplicated(r$match_pairs$control_idx)))
})

test_that("native genetic matching is deterministic given a seed", {
  d <- .dgp_confounded(n = 300L, seed = 52L)
  r1 <- morie_matching_genetic(d, "d", c("x1", "x2"),
                               pop_size = 10L, n_generations = 3L, seed = 7L)
  r2 <- morie_matching_genetic(d, "d", c("x1", "x2"),
                               pop_size = 10L, n_generations = 3L, seed = 7L)
  expect_identical(r1$match_pairs, r2$match_pairs)
  expect_identical(r1$details$best_weights, r2$details$best_weights)
})

test_that("genetic balance is at least as good as plain mahalanobis", {
  d <- .dgp_confounded(n = 1500L, seed = 53L)
  gen <- morie_matching_genetic(d, "d", c("x1", "x2", "x3"),
                                pop_size = 16L, n_generations = 6L)
  mah <- morie_matching_mahalanobis(d, "d", c("x1", "x2", "x3"))
  worst <- function(md) max(vapply(c("x1", "x2", "x3"), function(v)
    abs(.smd(md, "d", v)), numeric(1)))
  # equal-weight candidate is seeded into the GA population, so the
  # selected weights can only improve the fitness; allow tiny slack
  # because fitness is min-p, not max-SMD
  expect_lt(worst(gen$matched_data), worst(mah$matched_data) + 0.05)
  expect_lt(worst(gen$matched_data), 0.1)
})

test_that("native genetic matching honours n_neighbors", {
  d <- .dgp_confounded(n = 600L, seed = 54L)
  r <- morie_matching_genetic(d, "d", c("x1", "x2"), n_neighbors = 2L,
                              pop_size = 8L, n_generations = 2L)
  per_treated <- table(r$match_pairs$treated_idx)
  expect_true(all(per_treated <= 2L))
  expect_gt(sum(per_treated == 2L), 0L)
})

# ---- module 7: cardinality matching (native caliper sweep) ---------------

test_that("cardinality matching achieves the balance threshold when feasible", {
  d <- .dgp_confounded(n = 3000L, seed = 71L)
  r <- morie_matching_cardinality(d, "d", c("x1", "x2", "x3"),
                                  balance_threshold = 0.1)
  expect_s3_class(r, "morie_match_result")
  expect_identical(r$method, "cardinality")
  expect_null(r$details$warning)
  bal <- morie_matching_balance(r$matched_data, "d", c("x1", "x2", "x3"))
  expect_lte(bal$max_smd, 0.1)
})

test_that("cardinality matching flags an unachievable threshold honestly", {
  set.seed(72)
  n <- 200L
  x1 <- rnorm(n)
  d <- rbinom(n, 1, plogis(-0.5 + 3 * x1))  # extreme separation
  df <- data.frame(x1 = x1, d = d)
  r <- morie_matching_cardinality(df, "d", "x1",
                                  balance_threshold = 0.001)
  expect_identical(r$details$warning, "Balance threshold not achieved.")
})

test_that("cardinality keeps the largest passing sample across calipers", {
  d <- .dgp_confounded(n = 2000L, seed = 73L)
  r10 <- morie_matching_cardinality(d, "d", c("x1", "x2"),
                                    balance_threshold = 0.1)
  r25 <- morie_matching_cardinality(d, "d", c("x1", "x2"),
                                    balance_threshold = 0.25)
  # looser threshold can only admit an equal-or-larger sample
  expect_gte(nrow(r25$matched_data), nrow(r10$matched_data))
})

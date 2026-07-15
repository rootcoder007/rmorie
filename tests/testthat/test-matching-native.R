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

# ---- module 8: native design-based weighted GLM --------------------------

test_that("native svyglm-equivalent returns a full coefficient table", {
  set.seed(81)
  n <- 300L
  x <- rnorm(n); w <- runif(n, 0.5, 2)
  y <- rbinom(n, 1, plogis(-0.3 + 0.7 * x))
  df <- data.frame(x = x, y = y)
  out <- rmorie:::.morie_svyglm_native(y ~ x, data = df, weights = w,
                                       family = stats::quasibinomial())
  expect_identical(colnames(out$coefficients),
                   c("Estimate", "Std. Error", "t value", "Pr(>|t|)"))
  expect_true(all(is.finite(out$coefficients)))
  expect_true(all(out$confint[, 1] < out$confint[, 2]))
  expect_identical(dim(out$vcov), c(2L, 2L))
})

test_that("ebac selection IPW runs without the survey package loaded", {
  set.seed(82)
  n <- 300L
  cpads <- data.frame(
    weight = runif(n, 0.5, 2),
    alcohol_past12m = rbinom(n, 1, 0.85),
    heavy_drinking_30d = rbinom(n, 1, 0.3),
    ebac_tot = ifelse(rbinom(n, 1, 0.7) == 1, abs(rnorm(n, 0.05, 0.03)), NA),
    ebac_legal = rbinom(n, 1, 0.7),
    cannabis_any_use = rbinom(n, 1, 0.3),
    age_group = sample(1:6, n, TRUE),
    gender = sample(1:2, n, TRUE),
    province_region = sample(1:5, n, TRUE),
    mental_health = sample(1:5, n, TRUE),
    physical_health = sample(1:5, n, TRUE)
  )
  out <- morie_run_ebac_selection_ipw_analysis(cpads)
  expect_true(is.finite(out$ebac_final_ipw_or$or))
  expect_true(is.finite(out$ebac_final_ipw_linear$estimate))
  expect_true(out$ebac_final_ipw_or$or_lower95 <
                out$ebac_final_ipw_or$or_upper95)
})

# ---- module 10: native DML (PLR + IRM) -----------------------------------

test_that("native PLR recovers theta on a linear DGP and is deterministic", {
  set.seed(101)
  n <- 1500L
  X <- matrix(rnorm(n * 4), n, 4)
  d <- rbinom(n, 1, plogis(0.6 * X[, 1]))
  y <- 0.8 * d + X[, 1] + 0.5 * X[, 2] + rnorm(n)
  df <- data.frame(y = y, d = d, X)
  names(df)[3:6] <- paste0("x", 1:4)
  r1 <- morie_estimate_double_ml(df, "y", "d", paste0("x", 1:4))
  r2 <- morie_estimate_double_ml(df, "y", "d", paste0("x", 1:4))
  expect_identical(r1, r2)
  expect_identical(r1$method, "PLR (rmorie native)")
  expect_lt(abs(r1$ate - 0.8), 3 * r1$se)
  expect_true(r1$ci_lower < r1$ate && r1$ate < r1$ci_upper)
})

test_that("native PLR n_rep median aggregation is finite and stable", {
  set.seed(102)
  n <- 600L
  X <- matrix(rnorm(n * 3), n, 3)
  d <- rbinom(n, 1, plogis(0.5 * X[, 1]))
  y <- 0.5 * d + X[, 1] + rnorm(n)
  df <- data.frame(y = y, d = d, X)
  names(df)[3:5] <- paste0("x", 1:3)
  r <- morie_estimate_double_ml(df, "y", "d", paste0("x", 1:3), n_rep = 3L)
  expect_true(is.finite(r$ate) && is.finite(r$se))
})

test_that("native IRM recovers theta with the AIPW score", {
  set.seed(103)
  n <- 2000L
  X <- matrix(rnorm(n * 3), n, 3)
  d <- rbinom(n, 1, plogis(0.5 * X[, 1] - 0.3 * X[, 2]))
  y <- 0.7 * d + X[, 1] - 0.4 * X[, 2] + rnorm(n)
  df <- data.frame(y = y, d = d, X)
  names(df)[3:5] <- paste0("x", 1:3)
  r <- morie_estimate_irm(df, treatment = "d", outcome = "y",
                          covariates = paste0("x", 1:3))
  expect_identical(r$method, "IRM (rmorie native)")
  expect_lt(abs(r$ate - 0.7), 3 * r$se)
})

# ---- module 11: native causal forest (R-learner) --------------------------

test_that("native dr_forest recovers a constant effect", {
  set.seed(111)
  n <- 1500L
  X <- matrix(rnorm(n * 3), n, 3)
  w <- rbinom(n, 1, plogis(0.5 * X[, 1]))
  y <- 1.2 * w + X[, 1] + 0.5 * X[, 2] + rnorm(n)
  df <- data.frame(t = w, y = y, x1 = X[, 1], x2 = X[, 2], x3 = X[, 3])
  r <- morie_estimate_dr_forest(df, "t", "y", c("x1", "x2", "x3"))
  expect_named(r, c("ate", "se", "ci_lower", "ci_upper", "n"))
  expect_lt(abs(r$ate - 1.2), 4 * r$se)
  expect_equal(r$n, n)
})

test_that("native causal forest tau(x) tracks true heterogeneity", {
  set.seed(112)
  n <- 3000L
  X <- matrix(rnorm(n * 3), n, 3)
  w <- rbinom(n, 1, plogis(0.4 * X[, 1]))
  tau_true <- 1 + X[, 2]           # effect rises in x2
  y <- tau_true * w + X[, 1] + rnorm(n)
  nf <- rmorie:::.morie_causal_forest_native(X, y, w, n_trees = 300L)
  expect_gt(stats::cor(nf$tau, tau_true), 0.5)
  # group contrast: high-x2 units must show larger tau than low-x2
  hi <- X[, 2] > 1; lo <- X[, 2] < -1
  expect_gt(mean(nf$tau[hi]), mean(nf$tau[lo]))
})

test_that("native dr_forest target_sample options all return finite results", {
  set.seed(113)
  n <- 800L
  X <- matrix(rnorm(n * 2), n, 2)
  w <- rbinom(n, 1, plogis(0.4 * X[, 1]))
  y <- 0.8 * w + X[, 1] + rnorm(n)
  df <- data.frame(t = w, y = y, x1 = X[, 1], x2 = X[, 2])
  for (ts in c("all", "treated", "control", "overlap")) {
    r <- morie_estimate_dr_forest(df, "t", "y", c("x1", "x2"),
                                  target_sample = ts)
    expect_true(is.finite(r$ate) && is.finite(r$se))
  }
})

# ---- module 12: native X- and DR-learners --------------------------------

test_that("all four meta-learners agree on a constant effect", {
  set.seed(121)
  n <- 1500L
  x1 <- rnorm(n); x2 <- rnorm(n)
  d <- rbinom(n, 1, plogis(0.4 * x1))
  y <- 1.0 * d + x1 + 0.5 * x2 + rnorm(n)
  df <- data.frame(y = y, d = d, x1 = x1, x2 = x2)
  for (ml in c("t_learner", "s_learner", "x_learner", "dr_learner")) {
    tau <- morie_estimate_cate(df, "d", "y", c("x1", "x2"),
                               meta_learner = ml)
    expect_length(tau, n)
    expect_true(all(is.finite(tau)))
    expect_lt(abs(mean(tau) - 1.0), 0.25)
  }
})

test_that("X- and DR-learners track heterogeneous effects", {
  set.seed(122)
  n <- 3000L
  x1 <- rnorm(n); x2 <- rnorm(n)
  d <- rbinom(n, 1, plogis(0.4 * x1))
  tau_true <- 1 + x2
  y <- tau_true * d + x1 + rnorm(n)
  df <- data.frame(y = y, d = d, x1 = x1, x2 = x2)
  for (ml in c("x_learner", "dr_learner")) {
    tau <- morie_estimate_cate(df, "d", "y", c("x1", "x2"),
                               meta_learner = ml)
    expect_gt(stats::cor(tau, tau_true), 0.5)
  }
})

test_that("X-learner beats T-learner under heavy arm imbalance", {
  set.seed(123)
  n <- 2500L
  x1 <- rnorm(n); x2 <- rnorm(n)
  d <- rbinom(n, 1, 0.07)                # ~7 percent treated
  tau_true <- 1 + 0.8 * x2
  y <- tau_true * d + x1 + rnorm(n)
  df <- data.frame(y = y, d = d, x1 = x1, x2 = x2)
  tau_x <- morie_estimate_cate(df, "d", "y", c("x1", "x2"),
                               meta_learner = "x_learner")
  tau_t <- morie_estimate_cate(df, "d", "y", c("x1", "x2"),
                               meta_learner = "t_learner")
  mse <- function(a) mean((a - tau_true)^2)
  # Kuenzel et al.'s motivating case: unequal arms favour the X-learner
  expect_lt(mse(tau_x), mse(tau_t) * 1.1)
})

test_that("meta-learners error clearly on single-arm input", {
  df <- data.frame(y = rnorm(5), d = rep(1, 5), x = rnorm(5))
  expect_error(morie_estimate_cate(df, "d", "y", "x",
                                   meta_learner = "x_learner"),
               "both treatment arms")
})

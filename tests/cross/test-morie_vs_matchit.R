# SPDX-License-Identifier: AGPL-3.0-or-later
# Cross-validation: native matcher vs MatchIt. MatchIt is a TEST-ONLY
# dependency here (Suggests); the production path never touches it.
library(testthat)
library(rmorie)

test_that("native matcher agrees with MatchIt on matched-set size and ATE", {
  skip_if_not_installed("MatchIt")
  set.seed(101)
  n <- 500L
  x1 <- rnorm(n); x2 <- rnorm(n)
  d <- rbinom(n, 1, plogis(0.6 * x1 - 0.5 * x2))
  y <- 0.7 * d + x1 + rnorm(n)
  df <- data.frame(y = y, d = d, x1 = x1, x2 = x2)

  r_m <- morie_matching_nearest_neighbor(df, "d", c("x1", "x2"))
  mi <- MatchIt::matchit(d ~ x1 + x2, data = df,
                         method = "nearest", distance = "glm",
                         m.order = "largest")
  md_ref <- MatchIt::match.data(mi)

  # identical matched-set sizes
  expect_equal(nrow(r_m$matched_data), nrow(md_ref))
  # post-matching ATE (difference in means) within 1e-3
  ate <- function(m) mean(m$y[m$d == 1]) - mean(m$y[m$d == 0])
  expect_equal(ate(r_m$matched_data), ate(md_ref), tolerance = 1e-3)
})

test_that("native matcher matches MatchIt pair-for-pair on tie-free data", {
  skip_if_not_installed("MatchIt")
  set.seed(202)
  n <- 200L
  x1 <- rnorm(n)
  d <- rbinom(n, 1, plogis(0.8 * x1))
  df <- data.frame(d = d, x1 = x1)
  rownames(df) <- as.character(seq_len(n))

  r_m <- morie_matching_nearest_neighbor(df, "d", "x1")
  mi <- MatchIt::matchit(d ~ x1, data = df, method = "nearest",
                         distance = "glm", m.order = "largest")
  mm <- mi$match.matrix
  ref_pairs <- data.frame(
    treated_idx = rep(rownames(mm), ncol(mm)),
    control_idx = as.character(mm),
    stringsAsFactors = FALSE
  )
  ref_pairs <- ref_pairs[!is.na(ref_pairs$control_idx), ]
  got <- r_m$match_pairs[, c("treated_idx", "control_idx")]
  key <- function(p) sort(paste(p$treated_idx, p$control_idx))
  expect_identical(key(got), key(ref_pairs))
})

test_that("large-n ATE agreement within 1e-2", {
  skip_if_not_installed("MatchIt")
  skip_on_cran()
  set.seed(303)
  n <- 50000L
  x1 <- rnorm(n); x2 <- rnorm(n)
  d <- rbinom(n, 1, plogis(0.4 * x1 + 0.3 * x2 - 0.5))
  y <- 0.5 * d + 0.6 * x1 + rnorm(n)
  df <- data.frame(y = y, d = d, x1 = x1, x2 = x2)
  r_m <- morie_matching_nearest_neighbor(df, "d", c("x1", "x2"))
  mi <- MatchIt::matchit(d ~ x1 + x2, data = df, method = "nearest",
                         distance = "glm", m.order = "largest")
  md_ref <- MatchIt::match.data(mi)
  ate <- function(m) mean(m$y[m$d == 1]) - mean(m$y[m$d == 0])
  expect_equal(ate(r_m$matched_data), ate(md_ref), tolerance = 1e-2)
})

test_that("native mahalanobis agrees with MatchIt on set size and ATE", {
  skip_if_not_installed("MatchIt")
  set.seed(404)
  # generous control pool (rare treatment): greedy processing order
  # then has negligible effect, so both matchers find near-identical
  # sets and the estimate comparison is sharp
  n <- 1500L
  x1 <- rnorm(n); x2 <- rnorm(n)
  d <- rbinom(n, 1, plogis(-2.2 + 0.5 * x1 - 0.4 * x2))
  y <- 0.6 * d + 0.8 * x1 + rnorm(n)
  df <- data.frame(y = y, d = d, x1 = x1, x2 = x2)
  r_m <- morie_matching_mahalanobis(df, "d", c("x1", "x2"))
  mi <- MatchIt::matchit(d ~ x1 + x2, data = df, method = "nearest",
                         distance = "mahalanobis")
  md_ref <- MatchIt::match.data(mi)
  expect_equal(nrow(r_m$matched_data), nrow(md_ref))
  ate <- function(m) mean(m$y[m$d == 1]) - mean(m$y[m$d == 0])
  # greedy processing order differs (MatchIt data-order vs ours), so
  # exact pair identity is not guaranteed; the estimate must agree.
  expect_equal(ate(r_m$matched_data), ate(md_ref), tolerance = 2e-2)
})

# ---- module 3/4: exact + CEM vs MatchIt ----------------------------------

test_that("cross: native exact matches MatchIt exact (units + weighted ATT)", {
  skip_if_not_installed("MatchIt")
  set.seed(11)
  n <- 1200L
  region <- sample(c("N", "S", "E", "W"), n, replace = TRUE)
  year <- sample(2019:2021, n, replace = TRUE)
  d <- rbinom(n, 1, plogis(-1 + 0.5 * (region == "N")))
  y <- 1.5 * d + (region == "N") + rnorm(n)
  df <- data.frame(region, year, d, y, stringsAsFactors = FALSE)

  ours <- morie_matching_exact(df, "d", c("region", "year"))
  mi <- MatchIt::matchit(d ~ region + year, data = df, method = "exact")
  md_mi <- MatchIt::match.data(mi)

  expect_identical(nrow(ours$matched_data), nrow(md_mi))
  expect_identical(ours$n_treated, sum(md_mi$d == 1))
  expect_identical(ours$n_matched_control, sum(md_mi$d == 0))

  watt <- function(md) {
    w <- md$weights
    sum(md$y * w * (md$d == 1)) / sum(w * (md$d == 1)) -
      sum(md$y * w * (md$d == 0)) / sum(w * (md$d == 0))
  }
  expect_equal(watt(ours$matched_data), watt(md_mi), tolerance = 1e-6)
})

test_that("cross: native CEM tracks MatchIt cem on weighted ATT", {
  skip_if_not_installed("MatchIt")
  skip_if_not_installed("cem")
  set.seed(12)
  n <- 1500L
  x1 <- rnorm(n); x2 <- rnorm(n)
  d <- rbinom(n, 1, plogis(-1.2 + 0.5 * x1 - 0.4 * x2))
  y <- 2 * d + x1 + 0.5 * x2 + rnorm(n)
  df <- data.frame(x1, x2, d, y)

  ours <- morie_matching_cem(df, "d", c("x1", "x2"), n_bins = 5L)
  mi <- MatchIt::matchit(d ~ x1 + x2, data = df, method = "cem",
                         cutpoints = list(x1 = 5L, x2 = 5L))
  md_mi <- MatchIt::match.data(mi)

  watt <- function(md) {
    w <- md$weights
    sum(md$y * w * (md$d == 1)) / sum(w * (md$d == 1)) -
      sum(md$y * w * (md$d == 0)) / sum(w * (md$d == 0))
  }
  # Different (both valid) binning conventions -> same estimand, loose bar.
  expect_equal(watt(ours$matched_data), watt(md_mi), tolerance = 5e-2)
  # Both must land near the true effect of 2.
  expect_lt(abs(watt(ours$matched_data) - 2), 0.15)
})

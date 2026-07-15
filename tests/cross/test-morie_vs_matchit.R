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

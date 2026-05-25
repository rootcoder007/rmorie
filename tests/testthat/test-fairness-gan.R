# SPDX-License-Identifier: AGPL-3.0-or-later
library(testthat)

set.seed(1)

# ---------------------------------------------------------------------------
# spatial_gan
# ---------------------------------------------------------------------------

test_that("morie_fairness_spatial_gan returns rich result", {
  set.seed(1)
  pts <- matrix(rnorm(60), ncol = 2L)
  r <- morie_fairness_spatial_gan(pts, steps = 5L,
                                  batch_size = 8L, latent_dim = 4L,
                                  hidden = 8L, seed = 1L)
  expect_s3_class(r, "morie_fairness_result")
  expect_true("backend" %in% names(r))
  if (isTRUE(r$fitted)) {
    expect_true(is.function(r$sample))
    out <- r$sample(5L, seed = 2L)
    expect_equal(dim(out), c(5L, 2L))
  } else {
    expect_true(length(r$warnings) >= 1L)
  }
})

test_that("morie_fairness_spatial_gan rejects bad shape", {
  r <- morie_fairness_spatial_gan(matrix(rnorm(9), ncol = 3L), steps = 2L)
  expect_false(isTRUE(r$fitted))
  expect_true(length(r$warnings) >= 1L)
})

test_that("morie_fairness_spatial_gan rejects too-few rows", {
  r <- morie_fairness_spatial_gan(matrix(c(1, 2), ncol = 2L), steps = 2L)
  expect_false(isTRUE(r$fitted))
})

# ---------------------------------------------------------------------------
# ctgan_debiaser
# ---------------------------------------------------------------------------

test_that("morie_fairness_ctgan_debiaser missing column path", {
  df <- data.frame(group = c("A","B"), outcome = c(1,0),
                   stringsAsFactors = FALSE)
  r <- morie_fairness_ctgan_debiaser(df, outcome_col = "outcome",
                                     feature_cols = c("x1"),
                                     group_col = "group", n = 5L)
  expect_false(isTRUE(r$fitted))
  expect_true(length(r$warnings) >= 1L)
})

test_that("morie_fairness_ctgan_debiaser empty feature_cols", {
  df <- data.frame(group = c("A","B"), outcome = c(1,0),
                   stringsAsFactors = FALSE)
  r <- morie_fairness_ctgan_debiaser(df, outcome_col = "outcome",
                                     feature_cols = character(0),
                                     group_col = "group", n = 5L)
  expect_false(isTRUE(r$fitted))
})

test_that("morie_fairness_ctgan_debiaser single group rejected", {
  df <- data.frame(group = rep("A", 4), outcome = c(1,1,0,0),
                   x1 = rnorm(4), stringsAsFactors = FALSE)
  r <- morie_fairness_ctgan_debiaser(df, "outcome", "x1",
                                     group_col = "group", n = 5L)
  expect_false(isTRUE(r$fitted))
})

test_that("morie_fairness_ctgan_debiaser bad privileged rejected", {
  set.seed(2)
  df <- data.frame(group = sample(c("A","B"), 30, TRUE),
                   outcome = sample(0:1, 30, TRUE),
                   x1 = rnorm(30), stringsAsFactors = FALSE)
  r <- morie_fairness_ctgan_debiaser(df, "outcome", "x1",
                                     privileged = "Z", n = 5L)
  expect_false(isTRUE(r$fitted))
})

test_that("morie_fairness_ctgan_debiaser end-to-end", {
  set.seed(3)
  n <- 60L
  df <- data.frame(
    group = sample(c("A","B"), n, TRUE),
    outcome = rbinom(n, 1, 0.5),
    x1 = rnorm(n), x2 = rnorm(n),
    stringsAsFactors = FALSE
  )
  r <- morie_fairness_ctgan_debiaser(df, "outcome", c("x1","x2"),
                                     group_col = "group",
                                     privileged = "A",
                                     n = 20L, steps = 5L, seed = 1L)
  expect_s3_class(r, "morie_fairness_result")
  if (isTRUE(r$fitted)) {
    expect_equal(nrow(r$debiased), 20L)
    expect_true(all(c("x1","x2") %in% names(r$debiased)))
  } else {
    expect_true(length(r$warnings) >= 1L)
  }
})
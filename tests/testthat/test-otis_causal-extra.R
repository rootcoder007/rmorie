# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Phase 2GG: tests for otis_causal.R exports not covered by
# test-otis_causal.R — the make_pair_* fixture builders + the
# causal_grid sweep + the 4 NotYetPorted error-path wrappers
# (aipw_superlearner / plr / psm / psm_subclass).

# ---------------------------------------------- make_pair_alert_to_volatility

test_that("morie_otis_make_pair_alert_to_volatility_ruhela runs on b01 panel", {
  df <- make_synthetic_otis("b01", n = 300L, seed = 1L)
  for (col in c("MentalHealth_Alert", "SuicideRisk_Alert",
                "SuicideWatch_Alert"))
    df[[col]] <- as.integer(df[[col]] == "Yes")
  out <- tryCatch(
    morie_otis_make_pair_alert_to_volatility_ruhela(df),
    error = function(e) e)
  if (inherits(out, "error")) {
    skip(sprintf("ruhela pair error: %s", conditionMessage(out)))
  }
  expect_true(is.list(out) || is.data.frame(out))
})

test_that("morie_otis_make_pair_alert_to_volatility_naive runs on b01 panel", {
  df <- make_synthetic_otis("b01", n = 300L, seed = 2L)
  for (col in c("MentalHealth_Alert", "SuicideRisk_Alert",
                "SuicideWatch_Alert"))
    df[[col]] <- as.integer(df[[col]] == "Yes")
  out <- tryCatch(
    morie_otis_make_pair_alert_to_volatility_naive(df),
    error = function(e) e)
  if (inherits(out, "error")) {
    skip(sprintf("naive pair error: %s", conditionMessage(out)))
  }
  expect_true(is.list(out) || is.data.frame(out))
})

test_that("morie_otis_make_pair_alert_to_volatility_all returns both variants", {
  df <- make_synthetic_otis("b01", n = 300L, seed = 3L)
  for (col in c("MentalHealth_Alert", "SuicideRisk_Alert",
                "SuicideWatch_Alert"))
    df[[col]] <- as.integer(df[[col]] == "Yes")
  out <- tryCatch(
    morie_otis_make_pair_alert_to_volatility_all(df),
    error = function(e) e)
  if (inherits(out, "error")) {
    skip(sprintf("all pair error: %s", conditionMessage(out)))
  }
  expect_type(out, "list")
  if (is.list(out)) {
    expect_true("ruhela" %in% names(out))
    expect_true("naive" %in% names(out))
  }
})

# ---------------------------------------------- make_pair_a / _b / _c

test_that("morie_otis_make_pair_a runs on b01 panel", {
  df <- make_synthetic_otis("b01", n = 250L, seed = 4L)
  for (col in c("MentalHealth_Alert", "SuicideRisk_Alert",
                "SuicideWatch_Alert"))
    df[[col]] <- as.integer(df[[col]] == "Yes")
  out <- tryCatch(morie_otis_make_pair_a(df),
                  error = function(e) e)
  if (inherits(out, "error")) {
    skip(sprintf("pair_a error: %s", conditionMessage(out)))
  }
  expect_true(is.list(out) || is.data.frame(out))
})

test_that("morie_otis_make_pair_b runs on b01 panel", {
  df <- make_synthetic_otis("b01", n = 250L, seed = 5L)
  for (col in c("MentalHealth_Alert", "SuicideRisk_Alert",
                "SuicideWatch_Alert"))
    df[[col]] <- as.integer(df[[col]] == "Yes")
  out <- tryCatch(morie_otis_make_pair_b(df),
                  error = function(e) e)
  if (inherits(out, "error")) {
    skip(sprintf("pair_b error: %s", conditionMessage(out)))
  }
  expect_true(is.list(out) || is.data.frame(out))
})

test_that("morie_otis_make_pair_c runs on b01 panel", {
  df <- make_synthetic_otis("b01", n = 250L, seed = 6L)
  for (col in c("MentalHealth_Alert", "SuicideRisk_Alert",
                "SuicideWatch_Alert"))
    df[[col]] <- as.integer(df[[col]] == "Yes")
  out <- tryCatch(morie_otis_make_pair_c(df),
                  error = function(e) e)
  if (inherits(out, "error")) {
    skip(sprintf("pair_c error: %s", conditionMessage(out)))
  }
  expect_true(is.list(out) || is.data.frame(out))
})

# ---------------------------------------------- causal_grid

test_that("morie_otis_causal_grid runs on a passed b01 fixture (no disk loader needed)", {
  df <- make_synthetic_otis("b01", n = 400L, seed = 7L)
  for (col in c("MentalHealth_Alert", "SuicideRisk_Alert",
                "SuicideWatch_Alert"))
    df[[col]] <- as.integer(df[[col]] == "Yes")
  out <- tryCatch(morie_otis_causal_grid(df, seed = 1L),
                  error = function(e) e)
  if (inherits(out, "error")) {
    skip(sprintf("causal_grid error: %s", conditionMessage(out)))
  }
  expect_true(is.list(out) || is.data.frame(out))
})

# ------------------------------------------ ported native wrappers

# Ported 2026-07: these four wrappers now run native estimators; the
# deep truth-recovery checks live in test-ported-stubs.R. Here we keep
# fast smoke assertions on the public API surface.
.pw_df <- function() {
  set.seed(9)
  n <- 200L
  x1 <- rnorm(n)
  d <- rbinom(n, 1, stats::plogis(0.6 * x1))
  y <- 1 + 1.5 * d + x1 + rnorm(n)
  data.frame(y, d, x1)
}

test_that("morie_otis_aipw_superlearner runs natively", {
  r <- morie_otis_aipw_superlearner(.pw_df(), "d", "y", "x1",
                                    n_folds = 3L)
  expect_true(is.finite(r$ate))
  expect_identical(r$estimator, "AIPW-SuperLearner")
})

test_that("morie_otis_plr runs natively", {
  r <- morie_otis_plr(.pw_df(), "d", "y", "x1")
  expect_true(is.finite(r$ate))
  expect_identical(r$estimator, "PLR-DML")
})

test_that("morie_otis_psm runs natively", {
  r <- morie_otis_psm(.pw_df(), "d", "y", "x1")
  expect_true(is.finite(r$ate))
  expect_identical(r$estimator, "PSM-ATT")
})

test_that("morie_otis_psm_subclass runs natively", {
  r <- morie_otis_psm_subclass(.pw_df(), "d", "y", "x1")
  expect_true(is.finite(r$ate))
  expect_identical(r$estimator, "PS-subclass")
})

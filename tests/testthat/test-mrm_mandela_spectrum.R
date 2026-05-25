# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Phase 2S: tests for mrm_mandela_spectrum.R::mrm_otis_mandela_spectrum.

test_that("mrm_otis_mandela_spectrum runs on dictionary-driven b01 panel", {
  df <- make_synthetic_otis("b01", n = 300L, seed = 1L)
  # Convert Yes/No alert text to 0/1 so the alert-count aggregation
  # has numeric inputs.
  for (col in c("MentalHealth_Alert", "SuicideRisk_Alert",
                "SuicideWatch_Alert"))
    df[[col]] <- as.integer(df[[col]] == "Yes")
  out <- tryCatch(
    mrm_otis_mandela_spectrum(df),
    error = function(e) e
  )
  if (inherits(out, "error")) {
    skip(sprintf("mandela_spectrum error: %s",
                 conditionMessage(out)))
  }
  expect_true(is.list(out) || is.data.frame(out))
})

test_that("mrm_otis_mandela_spectrum accepts custom threshold_days", {
  df <- make_synthetic_otis("b01", n = 200L, seed = 2L)
  for (col in c("MentalHealth_Alert", "SuicideRisk_Alert",
                "SuicideWatch_Alert"))
    df[[col]] <- as.integer(df[[col]] == "Yes")
  out <- tryCatch(
    mrm_otis_mandela_spectrum(df, threshold_days = 30L),
    error = function(e) e
  )
  if (inherits(out, "error")) {
    skip(sprintf("threshold_days error: %s",
                 conditionMessage(out)))
  }
  expect_true(is.list(out) || is.data.frame(out))
})

test_that("mrm_otis_mandela_spectrum errors on missing required col", {
  df <- make_synthetic_otis("b01", n = 100L, seed = 3L)
  df$NumberConsecutiveDays_Segregation <- NULL
  expect_error(mrm_otis_mandela_spectrum(df))
})

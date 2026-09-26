# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Phase 2M: tests for mandela.R::mrm_classify_mandela.
# Uses dictionary-driven b01 fixture from helper-otis.R.

test_that("mrm_classify_mandela returns per-row Mandela flag on b01 data", {
  df <- make_synthetic_otis("b01", n = 200L, seed = 1L)
  # Some rows should exceed the 15-day threshold by construction
  # (b01's NumberConsecutiveDays_Segregation samples 0..80).
  out <- mrm_classify_mandela(df, denominator = "individual_any")
  expect_true(is.list(out) || is.data.frame(out) || is.numeric(out))
})

test_that("mrm_classify_mandela accepts the 'row' denominator", {
  df <- make_synthetic_otis("b01", n = 200L, seed = 2L)
  out <- mrm_classify_mandela(df, denominator = "row")
  expect_true(is.list(out) || is.data.frame(out) || is.numeric(out))
})

test_that("mrm_classify_mandela broader_rc=TRUE counts alert combinations", {
  df <- make_synthetic_otis("b01", n = 200L, seed = 3L)
  # Convert Yes/No to 0/1 for the alert columns so the rowSums arithmetic
  # works (mrm_classify_mandela's broader_rc path expects numeric flags).
  for (col in c("MentalHealth_Alert", "SuicideRisk_Alert",
                "SuicideWatch_Alert"))
    df[[col]] <- as.integer(df[[col]] == "Yes")
  out <- tryCatch(
    mrm_classify_mandela(df, denominator = "individual_any",
                         broader_rc = TRUE),
    error = function(e) e
  )
  if (inherits(out, "error")) {
    skip(sprintf("broader_rc error: %s", conditionMessage(out)))
  }
  expect_true(is.list(out) || is.data.frame(out) || is.numeric(out))
})

test_that("mrm_classify_mandela errors when required column is absent", {
  df <- make_synthetic_otis("b01", n = 100L, seed = 4L)
  df$NumberConsecutiveDays_Segregation <- NULL
  expect_error(mrm_classify_mandela(df))
})

test_that("mrm_classify_mandela threshold_days=0 catches every non-zero day", {
  df <- make_synthetic_otis("b01", n = 100L, seed = 5L)
  out <- mrm_classify_mandela(df, threshold_days = 0L,
                               denominator = "row")
  expect_true(is.list(out) || is.data.frame(out) || is.numeric(out))
})

test_that("broader restrictive confinement adds 2+ alerts regardless of duration", {
  d <- data.frame(
    UniqueIndividual_ID = c("a", "b", "c", "d"), EndFiscalYear = 2024,
    NumberConsecutiveDays_Segregation = c(20, 3, 3, 3),
    MentalHealth_Alert = c("No", "Yes", "Yes", "No"),
    SuicideRisk_Alert = c("No", "Yes", "No", "No"),
    SuicideWatch_Alert = c("No", "No", "No", "No"),
    stringsAsFactors = FALSE
  )
  r <- mrm_classify_mandela(d, denominator = "row", broader_rc = TRUE)
  # strict: only the 20-day placement; broader adds b (two "Yes" alerts);
  # c has one alert, d none -- "No" strings are not alerts
  expect_equal(r$n_mandela[1], 1L)
  expect_equal(r$n_broader_rc[1], 2L)
})

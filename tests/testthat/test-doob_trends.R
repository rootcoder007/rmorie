# SPDX-License-Identifier: AGPL-3.0-or-later
# Coverage tests for R/doob_trends.R

test_that("analyze_doob_table1_releases returns morie_result", {
  res <- analyze_doob_table1_releases()
  expect_true(is.list(res))
  expect_true("title" %in% names(res))
  expect_true("summary_lines" %in% names(res))
  expect_true("tables" %in% names(res))
  expect_true("payload" %in% names(res))
})

test_that("analyze_doob_table2_flow returns morie_result", {
  res <- analyze_doob_table2_flow()
  expect_true(is.list(res))
  expect_true("title" %in% names(res))
  expect_true(length(res$tables) >= 1L)
})

test_that("analyze_doob_table3_age_overrepresentation returns morie_result", {
  res <- analyze_doob_table3_age_overrepresentation()
  expect_true(is.list(res))
  expect_true("title" %in% names(res))
})

test_that("pettitt_changepoint normal series", {
  set.seed(2026L)
  series <- c(rnorm(20, 0), rnorm(20, 2))
  res <- pettitt_changepoint(series)
  expect_named(res, c("change_point_index", "U_max", "p_value", "note"))
  expect_true(is.integer(res$change_point_index))
  expect_true(is.numeric(res$U_max))
  expect_true(is.numeric(res$p_value))
})

test_that("pettitt_changepoint returns NA for tiny series", {
  res <- pettitt_changepoint(c(1, 2, 3))
  expect_true(is.na(res$change_point_index))
})

test_that("pettitt_changepoint handles NA in series (filters)", {
  res <- pettitt_changepoint(c(1, NA, 2, 3, 4, NA, 5))
  expect_true(is.numeric(res$U_max))
})

test_that("decoupling_test happy path", {
  set.seed(1L)
  crime <- rnorm(20)
  imp <- 0.5 * crime + rnorm(20, sd = 0.5)
  res <- decoupling_test(crime, imp, years = 2000:2019)
  expect_true("payload" %in% names(res))
  expect_true(is.numeric(res$payload$r_pearson))
  expect_true(is.numeric(res$payload$p_value))
})

test_that("decoupling_test without years argument", {
  set.seed(2L)
  crime <- rnorm(15)
  imp <- rnorm(15)
  res <- decoupling_test(crime, imp)
  expect_true("payload" %in% names(res))
})

test_that("decoupling_test length mismatch returns warning result", {
  res <- decoupling_test(1:5, 1:10)
  expect_true(any(grepl("length mismatch", res$warnings)))
})

test_that("decoupling_test too-short series returns warning result", {
  res <- decoupling_test(1:3, 1:3)
  expect_true(any(grepl(">= 5", res$warnings)))
})

test_that("decoupling_test perfect correlation branch", {
  x <- as.numeric(seq_len(20))
  res <- decoupling_test(x, x)
  expect_equal(res$payload$r_pearson, 1)
  expect_equal(res$payload$p_value, 0)
})

test_that("analyze_doob_full_affidavit assembles 3 sections", {
  res <- analyze_doob_full_affidavit()
  expect_true("tables" %in% names(res))
  expect_true(length(res$tables) >= 1L)
  expect_true(grepl("Affidavit", res$title))
})

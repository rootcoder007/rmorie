# SPDX-License-Identifier: AGPL-3.0-or-later
library(testthat)

set.seed(1)

test_that("explain_file returns known explanation", {
  s <- explain_file("power_summary.csv")
  expect_type(s, "character")
  expect_true(nchar(s) > 50)
  expect_true(grepl("power", s, ignore.case = TRUE))
})

test_that("explain_file strips path", {
  s <- explain_file("some/dir/power_summary.csv")
  expect_true(grepl("power", s, ignore.case = TRUE))
})

test_that("explain_file falls back on extension mismatch", {
  s <- explain_file("power_summary.tsv")
  expect_true(grepl("power", s, ignore.case = TRUE))
})

test_that("explain_file unknown name returns fallback listing", {
  s <- explain_file("__no_such_file.csv")
  expect_true(grepl("No registered explanation", s))
  expect_true(grepl("Known files", s))
})

test_that("explain_known_files returns sorted character vector", {
  v <- explain_known_files()
  expect_type(v, "character")
  expect_true(length(v) > 5L)
  expect_equal(v, sort(v))
  expect_true("power_summary.csv" %in% v)
})

test_that("cheatsheet prints and returns body", {
  expect_output(out <- cheatsheet())
  expect_type(out, "character")
  expect_true(grepl("morie", out, ignore.case = TRUE))
})

test_that("explain_file covers data-wrangling entries", {
  expect_true(grepl("missing", explain_file("data_na_summary.csv"),
                    ignore.case = TRUE))
  expect_true(grepl("wrangling", explain_file("data_wrangling_log.csv"),
                    ignore.case = TRUE))
})

test_that("explain_file covers descriptive-statistics entries", {
  expect_true(grepl("binomial", explain_file("binomial_summaries.csv"),
                    ignore.case = TRUE))
  expect_true(grepl("survey", explain_file(
    "binomial_summaries_survey_weighted.csv"
  ), ignore.case = TRUE))
})

test_that("explain_file covers frequentist outputs", {
  expect_true(nzchar(explain_file("frequentist_effect_sizes.csv")))
  expect_true(nzchar(explain_file("frequentist_hypothesis_tests.csv")))
  expect_true(nzchar(explain_file("frequentist_heavy_drinking_prevalence_ci.csv")))
})

test_that("explain_file covers randomization examples", {
  expect_true(nzchar(explain_file("randomization_block_blueprints.csv")))
  expect_true(nzchar(explain_file(
    "randomization_schedule_example_heavy_drinking_30d.csv"
  )))
})
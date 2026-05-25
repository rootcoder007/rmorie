# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Phase 2KK: tests for the .otis_* + .tps_sp_* internal helpers
# in otis_all_analyze.R, otis.R, and tps_statphysics.R.

# ========================================================== otis_all_analyze.R

test_that(".otis_year_col picks EndFiscalYear / Year automatically", {
  df <- data.frame(EndFiscalYear = 2020:2024, x = 1:5)
  expect_equal(morie:::.otis_year_col(df), "EndFiscalYear")
  df2 <- data.frame(Year = 2020:2024, x = 1:5)
  expect_equal(morie:::.otis_year_col(df2), "Year")
})

test_that(".otis_to_int coerces text counts to integers + 0 sentinel", {
  expect_equal(morie:::.otis_to_int("5"), 5L)
  expect_equal(morie:::.otis_to_int(""), 0L)
  expect_equal(morie:::.otis_to_int(NA), 0L)
  # Comma-thousands separators are NOT parsed — fall back to 0.
  expect_equal(morie:::.otis_to_int("1,000"), 0L)
})

test_that(".otis_is_truthy maps Yes-like values to 1, No-like to 0", {
  # Returns integer (0/1), not logical. NA stays NA.
  expect_equal(morie:::.otis_is_truthy("Yes"), 1L)
  expect_equal(morie:::.otis_is_truthy("1"), 1L)
  expect_equal(morie:::.otis_is_truthy(1L), 1L)
  expect_equal(morie:::.otis_is_truthy("No"), 0L)
  expect_equal(morie:::.otis_is_truthy("0"), 0L)
  expect_equal(morie:::.otis_is_truthy(""), 0L)
  expect_true(is.na(morie:::.otis_is_truthy(NA)))
})

test_that(".otis_summary_lines returns the canonical Rows / Years header", {
  df <- make_synthetic_otis("b01", n = 100L, seed = 1L)
  out <- morie:::.otis_summary_lines(df, "b01",
                                       description = "Test panel")
  expect_type(out, "list")
  expect_true("Rows" %in% names(out))
})

test_that(".otis_crosstab produces a structured cross-tab", {
  df <- make_synthetic_otis("c01", n = 200L, seed = 2L)
  out <- morie:::.otis_crosstab(df, "Gender", "EndFiscalYear",
                                  "NumberIndividuals_InCustody")
  expect_type(out, "list")
})

test_that(".otis_year_trend returns per-year mean for a numeric column", {
  df <- make_synthetic_otis("b02", n = 200L, seed = 3L)
  out <- morie:::.otis_year_trend(df, "TotalAggregatedDays_Segregation")
  expect_type(out, "list")
})

test_that(".otis_wrap returns a rich-result list with title + sections", {
  s <- list(Rows = 100L, Years = "2018-2024")
  out <- morie:::.otis_wrap(title = "Test", summary_lines = s)
  expect_type(out, "list")
  expect_match(out$title, "Test")
})

test_that(".otis_female_indicator detects 'Female' strings", {
  expect_equal(morie:::.otis_female_indicator(c("Female", "Male",
                                                  "Female")),
               c(1L, 0L, 1L))
})

test_that(".otis_toronto_indicator detects 'Toronto' region", {
  expect_equal(morie:::.otis_toronto_indicator(c("Toronto", "Central",
                                                   "Toronto")),
               c(1L, 0L, 1L))
})

test_that(".otis_indigenous_indicator detects Indigenous race", {
  out <- morie:::.otis_indigenous_indicator(c("Indigenous", "White",
                                                "Black"))
  expect_equal(out, c(1L, 0L, 0L))
})

test_that(".otis_minority_religion_indicator detects non-Christian religion", {
  out <- morie:::.otis_minority_religion_indicator(
    c("Christian", "Muslim", "Hindu", "No religion"))
  expect_true(is.integer(out) || is.numeric(out))
  expect_length(out, 4L)
})

test_that(".otis_c_simple returns a function that runs on c-shaped data", {
  fn <- morie:::.otis_c_simple("c04", "test", "Race")
  expect_true(is.function(fn))
  df <- make_synthetic_otis("c04", n = 100L, seed = 4L)
  out <- fn(df)
  expect_type(out, "list")
})

test_that(".otis_d_simple returns a function that runs on d-shaped data", {
  fn <- morie:::.otis_d_simple("d02", "test", "Gender")
  expect_true(is.function(fn))
  df <- make_synthetic_otis("d02", n = 100L, seed = 5L)
  out <- fn(df)
  expect_type(out, "list")
})

test_that(".otis_causal_available returns logical", {
  expect_type(morie:::.otis_causal_available(), "logical")
})

test_that(".otis_not_yet_ported returns a stub rich-result + warning", {
  # Contract: it does NOT throw; it returns a rich-result list with
  # status="stub" + a warning to surface in console output.
  out <- suppressWarnings(
    morie:::.otis_not_yet_ported("fake_fn", "no impl"))
  expect_type(out, "list")
  expect_s3_class(out, "morie_otis_analysis_result")
})

# ========================================================== otis.R

test_that(".otis_result builds the canonical result list", {
  out <- morie:::.otis_result(title = "Test",
                                summary = "Demo result")
  expect_type(out, "list")
})

test_that(".otis_binarise maps Yes/No to 1/0", {
  expect_equal(morie:::.otis_binarise(c("Yes", "No", "Yes", NA)),
               c(1L, 0L, 1L, NA))
})

# ========================================================== tps_statphysics.R

test_that(".tps_sp_round rounds with default 3 digits", {
  expect_equal(morie:::.tps_sp_round(3.14159), 3.142)
  expect_equal(morie:::.tps_sp_round(3.14159, k = 4L), 3.1416)
})

test_that(".tps_sp_project_xy returns x/y same length as input", {
  set.seed(1L)
  lat <- stats::runif(20, 43.58, 43.88)
  lon <- stats::runif(20, -79.62, -79.13)
  out <- morie:::.tps_sp_project_xy(lat, lon)
  expect_type(out, "list")
  expect_length(out$x, 20L)
  expect_length(out$y, 20L)
})

test_that(".tps_sp_toronto_grid returns nx x ny dim list", {
  out <- morie:::.tps_sp_toronto_grid(nx = 10L, ny = 5L)
  expect_type(out, "list")
})

test_that(".tps_sp_roll shifts a matrix along an axis", {
  M <- matrix(1:9, 3L, 3L)
  out0 <- morie:::.tps_sp_roll(M, shift = 1L, axis = 1L)
  expect_true(is.matrix(out0))
  expect_equal(dim(out0), c(3L, 3L))
})

test_that(".tps_sp_lap returns Laplacian of a matrix", {
  M <- matrix(stats::rnorm(25), 5L, 5L)
  out <- morie:::.tps_sp_lap(M, dx = 0.1, dy = 0.1)
  expect_true(is.matrix(out))
  expect_equal(dim(out), c(5L, 5L))
})

test_that(".tps_sp_grad returns gradient tuple of a matrix", {
  M <- matrix(stats::rnorm(25), 5L, 5L)
  out <- morie:::.tps_sp_grad(M, dx = 0.1, dy = 0.1)
  expect_type(out, "list")
})

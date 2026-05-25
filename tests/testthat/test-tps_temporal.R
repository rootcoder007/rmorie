# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Unit tests for R/tps_temporal.R: YoY trend, seasonal pattern,
# Pettitt change-point, ARIMA(1,1,1) forecast. Synthetic OCC_YEAR /
# OCC_MONTH / OCC_DOW / OCC_HOUR drive each callable.

set.seed(1L)

.mk_yoy <- function(n = 400L, seed = 1L, start = 2010L, end = 2024L) {
  set.seed(seed)
  data.frame(
    OCC_YEAR  = sample(start:end, n, replace = TRUE),
    OCC_MONTH = sample(1:12, n, replace = TRUE),
    OCC_DOW   = sample(c("Mon","Tue","Wed","Thu","Fri","Sat","Sun"),
                       n, replace = TRUE),
    OCC_HOUR  = sample(0:23, n, replace = TRUE),
    OCC_DAY   = sample(1:28, n, replace = TRUE)
  )
}

test_that("morie_tps_year_over_year_trend fits OLS", {
  rr <- morie_tps_year_over_year_trend(.mk_yoy(), ds_name = "Synth")
  expect_s3_class(rr, "morie_tps_temporal_result")
  expect_true(is.numeric(rr$slope))
  expect_true(rr$direction %in% c("INCREASING", "DECREASING", "FLAT"))
})

test_that("morie_tps_year_over_year_trend warns on missing year col", {
  rr <- morie_tps_year_over_year_trend(data.frame(x = 1:5))
  expect_true(any(grepl("missing", rr$warnings)))
})

test_that("morie_tps_year_over_year_trend warns on <3 years", {
  df <- data.frame(OCC_YEAR = c(2020, 2020))
  rr <- morie_tps_year_over_year_trend(df)
  expect_true(any(grepl("usable year", rr$warnings)))
})

test_that("morie_tps_year_over_year_trend filters bad years", {
  df <- data.frame(OCC_YEAR = c(1800, 2200))
  rr <- morie_tps_year_over_year_trend(df)
  expect_true(any(grepl("no usable years", rr$warnings)))
})

test_that("morie_tps_seasonal_pattern runs chi-square per cycle", {
  rr <- morie_tps_seasonal_pattern(.mk_yoy(), ds_name = "Synth")
  expect_s3_class(rr, "morie_tps_temporal_result")
  expect_true(!is.null(rr$month))
  expect_true(!is.null(rr$dow))
  expect_true(!is.null(rr$hour))
})

test_that("morie_tps_seasonal_pattern warns when no cycles present", {
  rr <- morie_tps_seasonal_pattern(data.frame(x = 1:5))
  expect_true(any(grepl("OCC_MONTH", rr$warnings)))
})

test_that("morie_tps_changepoint_detection: Pettitt over 6+ years", {
  rr <- morie_tps_changepoint_detection(.mk_yoy(n = 600L), ds_name = "Synth")
  expect_s3_class(rr, "morie_tps_temporal_result")
  expect_true(is.integer(rr$changepoint_year))
  expect_true(rr$p_value >= 0 && rr$p_value <= 1)
})

test_that("morie_tps_changepoint_detection warns on missing year col", {
  rr <- morie_tps_changepoint_detection(data.frame(x = 1:5))
  expect_true(any(grepl("missing", rr$warnings)))
})

test_that("morie_tps_changepoint_detection warns on <6 years", {
  df <- data.frame(OCC_YEAR = 2020:2023)
  rr <- morie_tps_changepoint_detection(df)
  expect_true(any(grepl(">=6", rr$warnings)))
})

test_that("morie_tps_arima_forecast runs over 24+ months", {
  set.seed(1L)
  rr <- morie_tps_arima_forecast(.mk_yoy(n = 800L), h = 6L,
                                  ds_name = "Synth")
  expect_s3_class(rr, "morie_tps_temporal_result")
  if (length(rr$forecast) > 0L) {
    expect_equal(length(rr$forecast), 6L)
  }
})

test_that("morie_tps_arima_forecast warns on too few months", {
  df <- data.frame(OCC_YEAR = c(2020, 2020), OCC_MONTH = c(1, 2),
                   OCC_DAY = c(1, 1))
  rr <- morie_tps_arima_forecast(df, h = 6L)
  expect_true(any(grepl(">=24", rr$warnings)))
})

test_that("print.morie_tps_temporal_result fires", {
  rr <- morie_tps_year_over_year_trend(.mk_yoy(), ds_name = "PrintCheck")
  expect_output(print(rr), "Year-over-year")
})
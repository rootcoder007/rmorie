# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Unit tests for R/tps_stochastic.R: Hawkes / SARIMA / Langevin /
# Fokker-Planck. Uses OCC_YEAR/OCC_MONTH/OCC_DAY synthetic incidents
# so `.tps_stoch_date_series` returns a non-empty POSIXct vector
# without touching disk or network.

set.seed(1L)

.mk_tps_dates <- function(n = 600L, start_year = 2015L, seed = 1L) {
  set.seed(seed)
  base <- as.Date(sprintf("%d-01-01", start_year))
  d <- sort(base + sample.int(365 * 6, n, replace = TRUE))
  data.frame(
    OCC_YEAR  = as.integer(format(d, "%Y")),
    OCC_MONTH = as.integer(format(d, "%m")),
    OCC_DAY   = as.integer(format(d, "%d"))
  )
}

test_that("morie_tps_hawkes_temporal_fit fits on synthetic times", {
  set.seed(1L)
  df <- .mk_tps_dates(n = 400L)
  rr <- morie_tps_hawkes_temporal_fit(df, ds_name = "Synth")
  expect_s3_class(rr, "morie_tps_stochastic_result")
  expect_true(is.numeric(rr$mu))
  expect_true(is.numeric(rr$kappa))
  expect_true(is.numeric(rr$omega))
  expect_true(rr$n > 0)
})

test_that("morie_tps_hawkes_temporal_fit warns on tiny input", {
  df <- .mk_tps_dates(n = 20L)
  rr <- morie_tps_hawkes_temporal_fit(df)
  expect_true(any(grepl("timestamps", rr$warnings)))
})

test_that("morie_tps_hawkes_temporal_fit subsamples large input", {
  set.seed(1L)
  df <- .mk_tps_dates(n = 1500L)
  rr <- morie_tps_hawkes_temporal_fit(df, max_n = 200L)
  expect_lte(rr$n, 200L)
})

test_that("morie_tps_sarima_forecast runs over 36+ months", {
  set.seed(1L)
  df <- .mk_tps_dates(n = 1500L, start_year = 2010L)
  rr <- morie_tps_sarima_forecast(df, ds_name = "Synth", h = 6L,
                                   order = c(0L, 1L, 1L),
                                   seasonal = c(0L, 0L, 0L, 12L))
  expect_s3_class(rr, "morie_tps_stochastic_result")
})

test_that("morie_tps_sarima_forecast warns when too few timestamps", {
  df <- .mk_tps_dates(n = 10L)
  rr <- morie_tps_sarima_forecast(df)
  expect_true(any(grepl("36", rr$warnings)))
})

test_that("morie_tps_sarima_forecast validates order/seasonal length", {
  df <- .mk_tps_dates(n = 100L)
  expect_error(morie_tps_sarima_forecast(df, order = c(1L, 1L),
                                          seasonal = c(0L, 1L, 1L, 12L)))
})

test_that("morie_tps_langevin_simulate fits + simulates OU paths", {
  set.seed(1L)
  df <- .mk_tps_dates(n = 800L)
  rr <- morie_tps_langevin_simulate(df, ds_name = "Synth",
                                    n_paths = 10L, T_days = 30L)
  expect_s3_class(rr, "morie_tps_stochastic_result")
  expect_equal(nrow(rr$paths), 10L)
  expect_true(is.numeric(rr$theta))
})

test_that("morie_tps_langevin_simulate warns on small input", {
  df <- .mk_tps_dates(n = 10L)
  rr <- morie_tps_langevin_simulate(df, n_paths = 5L, T_days = 10L)
  expect_true(any(grepl("timestamps", rr$warnings)))
})

test_that("morie_tps_fokker_planck_grid evolves density", {
  set.seed(1L)
  df <- .mk_tps_dates(n = 800L)
  rr <- morie_tps_fokker_planck_grid(df, ds_name = "Synth",
                                      n_grid = 24L, n_steps = 30L)
  expect_s3_class(rr, "morie_tps_stochastic_result")
  expect_equal(length(rr$grid), 24L)
  expect_equal(length(rr$density), 24L)
  expect_true(all(rr$density >= 0))
})

test_that("morie_tps_fokker_planck_grid warns on small input", {
  df <- .mk_tps_dates(n = 5L)
  rr <- morie_tps_fokker_planck_grid(df)
  expect_true(any(grepl("timestamps", rr$warnings)))
})

test_that("print.morie_tps_stochastic_result fires", {
  set.seed(1L)
  df <- .mk_tps_dates(n = 400L)
  rr <- morie_tps_hawkes_temporal_fit(df, ds_name = "PrintCheck")
  expect_output(print(rr), "Hawkes")
})

test_that("stochastic callables accept OCC_DATE fallback", {
  set.seed(1L)
  d <- sort(as.Date("2015-01-01") + sample.int(2000, 400))
  df <- data.frame(OCC_DATE = as.character(d))
  rr <- morie_tps_hawkes_temporal_fit(df, ds_name = "DateOnly")
  expect_s3_class(rr, "morie_tps_stochastic_result")
})
test_that(".tps_stoch_date_series handles TPS month names and epoch-ms OCC_DATE", {
  # TPS publishes OCC_MONTH as month names.
  df_names <- data.frame(
    OCC_YEAR = c(2020L, 2021L), OCC_MONTH = c("January", "July"),
    OCC_DAY = c(5L, 12L), stringsAsFactors = FALSE
  )
  ts1 <- rmorie:::.tps_stoch_date_series(df_names)
  expect_length(ts1, 2L)
  expect_identical(format(sort(ts1), "%Y-%m-%d"), c("2020-01-05", "2021-07-12"))

  # ArcGIS serves OCC_DATE as epoch milliseconds; must parse, not error.
  df_ms <- data.frame(OCC_DATE = c(1577836800000, 1609459200000))
  ts2 <- rmorie:::.tps_stoch_date_series(df_ms)
  expect_length(ts2, 2L)
  expect_identical(format(sort(ts2), "%Y"), c("2020", "2021"))

  # Unparseable character dates degrade to empty, never an error.
  df_bad <- data.frame(OCC_DATE = c("not-a-date", "also-bad"),
                       stringsAsFactors = FALSE)
  expect_silent(rmorie:::.tps_stoch_date_series(df_bad))
})

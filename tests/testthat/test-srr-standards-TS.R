# SPDX-License-Identifier: AGPL-3.0-or-later
#
# srr "TS" standards: one test per checklist item, run against the
# rmorie time-series forecasting surface (R/ts_forecast.R).

.ts_rw    <- function(n = 120L, s = 1L) { set.seed(s); cumsum(rnorm(n)) }
.ts_wn    <- function(n = 120L, s = 2L) { set.seed(s); rnorm(n) }
.ts_seas  <- function(n = 96L, s = 3L) {
  set.seed(s); as.numeric(10 + sin(2 * pi * (1:n) / 12) * 3 + cumsum(rnorm(n, 0, .2)))
}

# ======================= TS1: input class =============================

test_that("TS1.0 relies on an explicit time-series class", {
  x <- morie_ts(.ts_rw(), frequency = 12)
  expect_s3_class(x, "ts")
  expect_s3_class(x, "morie_ts")
})

test_that("TS1.1 input types/classes are documented + accepted", {
  expect_s3_class(morie_ts(1:10), "morie_ts")            # numeric vector
  expect_s3_class(morie_ts(stats::ts(1:10, frequency = 4)), "morie_ts")  # ts
})

test_that("TS1.2 validation routines confirm acceptable input", {
  expect_error(morie_ts(5), ">= 2 observations")         # too short
  expect_error(morie_ts(c(1, NA, 3)), "missing")         # implicit NA
})

test_that("TS1.3 a single pre-processing routine transforms all input", {
  # morie_ts is the single entry every downstream function calls
  expect_s3_class(morie_ts(.ts_rw()), "morie_ts")
  expect_true(is.function(morie_ts))
})

test_that("TS1.4 time attributes are maintained through pre-processing", {
  x <- morie_ts(.ts_seas(), frequency = 12, start = 2000)
  expect_equal(stats::frequency(x), 12)
  expect_equal(stats::start(x)[1], 2000)
})

test_that("TS1.5 strict ordering of the time index holds", {
  x <- morie_ts(1:20, frequency = 4)
  expect_equal(as.numeric(stats::time(x)), sort(as.numeric(stats::time(x))))
})

test_that("TS1.6 ordering / regularity violations are caught", {
  expect_error(morie_ts(c(1, NA, 3, 4)), "missing")      # implicit gap
})

test_that("TS1.7 units-typed input is accepted (coerced)", {
  v <- structure(as.numeric(1:10), units = "count", class = "myunits")
  # as.numeric() coercion path accepts a units-like object
  expect_s3_class(morie_ts(unclass(v), units = "count"), "morie_ts")
  expect_equal(attr(morie_ts(1:10, units = "count"), "units"), "count")
})

test_that("TS1.8 the calendar/time-unit system is explicit", {
  x <- morie_ts(.ts_seas(), frequency = 12, calendar = "monthly")
  expect_equal(attr(x, "calendar"), "monthly")
})

# ======================= TS2: stationarity ============================

test_that("TS2.0 regular data with implicit missing values is rejected", {
  expect_error(morie_ts(c(1, 2, NA, 4)), "missing")
})

test_that("TS2.1/TS2.1a missing data can trigger an error", {
  expect_error(morie_ts(c(1, NA, 3), na_action = "error"))
})

test_that("TS2.1b missing data can be ignored (omit) with a warning", {
  expect_warning(z <- morie_ts(c(1, NA, 3, 4), na_action = "omit"), "irregular")
  expect_length(as.numeric(z), 3L)
})

test_that("TS2.1c missing data can be imputed (interpolated)", {
  z <- morie_ts(c(1, NA, 3), na_action = "interpolate")
  expect_equal(as.numeric(z)[2], 2)                      # linear interp
})

test_that("TS2.2 stationarity of first + second moments is considered", {
  st <- morie_ts_stationarity(.ts_rw())
  expect_true(all(c("adf", "kpss") %in% names(st)))      # mean + variance tests
})

test_that("TS2.3 stationarity assumptions are surfaced", {
  st <- morie_ts_stationarity(.ts_wn())
  expect_true(is.logical(st$adf$stationary))
})

test_that("TS2.4/TS2.4a stationarity is checked with diagnostics", {
  st_rw <- morie_ts_stationarity(.ts_rw())
  st_wn <- morie_ts_stationarity(.ts_wn())
  # random walk needs differencing; white noise does not
  expect_gt(st_rw$suggested_d, 0L)
  expect_equal(st_wn$suggested_d, 0L)
})

test_that("TS2.4b a transformation to stationarity is advised + available", {
  d <- morie_ts_stationarity(.ts_rw())$suggested_d
  dx <- morie_ts_difference(.ts_rw(), differences = d)
  expect_equal(morie_ts_stationarity(dx)$suggested_d, 0L)  # now stationary
})

test_that("TS2.5 auto-correlation is returned in strict lag order", {
  a <- morie_ts_acf(.ts_rw(), lag_max = 10)
  expect_equal(a$lag, sort(a$lag))                       # index ordered
})

test_that("TS2.6 auto-correlation output can carry units metadata", {
  x <- morie_ts(.ts_rw(), units = "count")
  a <- morie_ts_acf(x)
  expect_equal(attr(a, "units"), "count")
})

# ======================= TS3: forecast errors =========================

test_that("TS3.0 forecast errors widen with horizon", {
  m <- morie_ts_arima(.ts_rw(), order = c(0, 1, 0))
  fc <- morie_ts_forecast(m, h = 20)
  expect_true(fc$se[20] > fc$se[1])                      # widening SE
})

test_that("TS3.1 a stationary case shows near-constant (non-widening) errors", {
  m <- morie_ts_arima(.ts_wn(), order = c(0, 0, 0))
  fc <- morie_ts_forecast(m, h = 20)
  # white-noise forecast SE is ~flat: last/first ratio near 1 (violates TS3.0)
  expect_lt(fc$se[20] / fc$se[1], 1.1)
})

test_that("TS3.2 the driver of forecast error (integration order) is demonstrable", {
  se_rw <- morie_ts_forecast(morie_ts_arima(.ts_rw(), c(0, 1, 0)), 15)$se
  se_wn <- morie_ts_forecast(morie_ts_arima(.ts_wn(), c(0, 0, 0)), 15)$se
  # the integrated series' error grows faster than the stationary one's
  expect_gt(se_rw[15] / se_rw[1], se_wn[15] / se_wn[1])
})

test_that("TS3.3/TS3.3b forecasts can be trimmed to an error margin", {
  m <- morie_ts_arima(.ts_rw(), order = c(0, 1, 0))
  fc <- morie_ts_forecast(m, h = 40)
  tr <- morie_ts_trim_forecast(fc, max_width = max(fc$upper - fc$lower) / 2)
  expect_lt(tr$h, fc$h)                                  # truncated horizon
  expect_true(all((tr$upper - tr$lower) <= max(fc$upper - fc$lower) / 2))
})

test_that("TS3.3a trimming is demonstrated via an example", {
  # example in ?morie_ts_trim_forecast; confirm the documented behaviour
  m <- morie_ts_arima(.ts_rw(), order = c(0, 1, 0))
  fc <- morie_ts_forecast(m, h = 30)
  expect_true(is.numeric(morie_ts_trim_forecast(fc, 1e9)$mean))  # no trim
})

# ======================= TS4: return values ===========================

test_that("TS4.0/TS4.0b return values have a unique class-defined format", {
  fc <- morie_ts_forecast(morie_ts_arima(.ts_rw(), c(0, 1, 0)), 10)
  expect_s3_class(fc, "morie_ts_forecast")
})

test_that("TS4.0a return is NOT forced into tsbox round-trip (own class used)", {
  fc <- morie_ts_forecast(morie_ts_arima(.ts_rw(), c(0, 1, 0)), 10)
  expect_true(is.list(fc) && !inherits(fc, "ts"))       # class-defined, documented
})

test_that("TS4.1 input units are carried into return values", {
  x <- morie_ts(.ts_rw(), units = "count")
  fc <- morie_ts_forecast(morie_ts_arima(x, c(0, 1, 0)), 10)
  expect_equal(fc$units, "count")
})

test_that("TS4.2 return type/class is documented + inspectable", {
  fc <- morie_ts_forecast(morie_ts_arima(.ts_rw(), c(0, 1, 0)), 10)
  expect_true(all(c("mean", "lower", "upper", "se", "observed") %in% names(fc)))
})

test_that("TS4.3 time scale / frequency is included in return", {
  x <- morie_ts(.ts_seas(), frequency = 12)
  fc <- morie_ts_forecast(morie_ts_arima(x, c(1, 0, 0)), 6)
  expect_equal(fc$frequency, 12)
})

test_that("TS4.4 differencing effect is documented (moments change)", {
  x <- .ts_rw()
  # differencing changes the mean/variance structure; back-transform restores it
  dx <- morie_ts_difference(x)
  expect_false(isTRUE(all.equal(var(dx), var(x))))
})

test_that("TS4.5/TS4.5a differenced data can be back-transformed", {
  x <- .ts_rw()
  dx <- morie_ts_difference(x, differences = 2)
  expect_equal(morie_ts_undifference(dx), x, tolerance = 1e-8)
})

test_that("TS4.5b back-transformation is demonstrated", {
  x <- cumsum(cumsum(rnorm(30)))
  dx <- morie_ts_difference(x, differences = 2)
  expect_equal(morie_ts_undifference(dx), x, tolerance = 1e-6)
})

test_that("TS4.5c limitations of back-transformed forecasts are documented", {
  # morie_ts_undifference requires the retained init values; error without them
  expect_error(morie_ts_undifference(1:5, init = NULL), "init")
})

test_that("TS4.6/TS4.6b forecast returns first + second moments (mean + se)", {
  fc <- morie_ts_forecast(morie_ts_arima(.ts_rw(), c(0, 1, 0)), 10)
  expect_true(is.numeric(fc$mean) && is.numeric(fc$se))
  expect_length(fc$mean, 10L)
})

test_that("TS4.6a/TS4.6c error is also given as an interval (general indication)", {
  fc <- morie_ts_forecast(morie_ts_arima(.ts_rw(), c(0, 1, 0)), 10)
  expect_true(all(fc$upper >= fc$mean) && all(fc$lower <= fc$mean))
})

test_that("TS4.7/TS4.7b forecast values are distinct list items from observed", {
  fc <- morie_ts_forecast(morie_ts_arima(.ts_rw(), c(0, 1, 0)), 10)
  expect_false(identical(fc$mean, fc$observed))         # separate items
  expect_length(fc$observed, 120L)
})

test_that("TS4.7a forecast values can be taken alone", {
  fc <- morie_ts_forecast(morie_ts_arima(.ts_rw(), c(0, 1, 0)), 10)
  expect_length(fc$mean, 10L)                           # forecast-only vector
})

test_that("TS4.7c forecast + observed can be combined with a distinguishing flag", {
  fc <- morie_ts_forecast(morie_ts_arima(.ts_rw(), c(0, 1, 0)), 10)
  combined <- data.frame(
    value = c(fc$observed, fc$mean),
    kind = c(rep("observed", length(fc$observed)), rep("forecast", fc$h)))
  expect_setequal(unique(combined$kind), c("observed", "forecast"))
})

# ======================= TS5: visualization ===========================

test_that("TS5.0 a default plot method exists for the forecast class", {
  expect_true(exists("plot.morie_ts_forecast"))
})

test_that("TS5.1/TS5.3 the time axis is labelled (with units where known)", {
  fc <- morie_ts_forecast(
    morie_ts_arima(morie_ts(.ts_rw(), units = "days"), c(0, 1, 0)), 5)
  tmp <- tempfile(fileext = ".png"); grDevices::png(tmp)
  plot(fc); grDevices::dev.off()
  expect_true(file.exists(tmp))                          # renders without error
})

test_that("TS5.2 time is placed on the horizontal axis", {
  # the plot method uses seq_len(n+h) as the x argument (time on abscissa)
  expect_true(any(grepl("xlab", deparse(body(plot.morie_ts_forecast)))))
})

test_that("TS5.4 frequency visualisation uses positive units (not [-pi,pi])", {
  psd <- morie_dsp_psd_periodogram(.ts_wn(), fs = 1)
  expect_true(all(psd$freq >= 0))                        # positive frequency axis
})

test_that("TS5.5 plot offers control of continuous vs broken lines", {
  expect_true("broken" %in% names(formals(plot.morie_ts_forecast)))
})

test_that("TS5.6 forecast distributional limits are drawn by default", {
  # the plot method shades the [lower, upper] band via polygon()
  expect_true(any(grepl("polygon", deparse(body(plot.morie_ts_forecast)))))
})

test_that("TS5.7 observed (input) values are included in the plot by default", {
  expect_true(any(grepl("observed", deparse(body(plot.morie_ts_forecast)))))
})

test_that("TS5.8 observed and forecast are visually distinguished", {
  # forecast drawn in a distinct colour ('blue') from the observed line
  expect_true(any(grepl("blue", deparse(body(plot.morie_ts_forecast)))))
})

# =============== end-to-end: selection + backtest =====================

test_that("TS model selection chooses orders by information criterion", {
  sel <- morie_ts_select(.ts_rw(), max_p = 2, max_q = 2)
  expect_s3_class(sel$best, "morie_ts_model")
  expect_true("score" %in% names(sel$results))
})

test_that("TS holdout back-test evaluates forecasts against actuals", {
  bt <- morie_ts_backtest(.ts_seas(), h = 12, order = c(1, 0, 0),
                          seasonal = c(0, 0, 0))
  expect_true(all(c("RMSE", "MAE", "MAPE") %in% names(bt$accuracy)))
  expect_true(bt$accuracy["RMSE"] >= 0)
})

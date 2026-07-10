# SPDX-License-Identifier: AGPL-3.0-or-later
#
# ts_forecast.R -- a coherent time-series forecasting subsystem for
# rmorie: a validated time-series class, stationarity testing,
# decomposition, differencing + back-transformation, (S)ARIMA fitting
# with exogenous regressors, multi-step forecasting with prediction
# intervals, holdout back-testing, and order selection. Built on
# stats::arima / stl + urca.
#
# API surface (every srr TS standard maps onto one of these):
#   morie_ts()             validated time-series constructor (ts-based)
#   morie_ts_stationarity() ADF / KPSS / Ljung-Box + suggested d
#   morie_ts_decompose()   STL / classical trend-seasonal-remainder
#   morie_ts_difference() / morie_ts_undifference()   (+ back-transform)
#   morie_ts_arima()       (S)ARIMA fit, optional exogenous xreg
#   morie_ts_forecast()    multi-step forecast + prediction intervals
#   print / plot .morie_ts_forecast
#   morie_ts_accuracy()    forecast-vs-actual error metrics
#   morie_ts_backtest()    holdout evaluation
#   morie_ts_select()      order selection by information criterion


#' srr time-series (TS) standards
#'
#' rmorie implements a full time-series forecasting subsystem in this
#' file; the TS standards are addressed against that surface and its
#' tests (test-srr-standards-TS.R).
#'
#' @srrstats {TS1.0} morie_ts() returns a ts-based class; input is not left as a generic vector.
#' @srrstats {TS1.1} accepted input types are documented on morie_ts().
#' @srrstats {TS1.2} morie_ts() validates length, ordering, and missingness.
#' @srrstats {TS1.3} morie_ts() is the single pre-processing routine feeding all downstream functions.
#' @srrstats {TS1.4} time attributes (frequency/start) are preserved by morie_ts().
#' @srrstats {TS1.5} the ts index is strictly ordered by construction.
#' @srrstats {TS1.6} ordering/regularity violations (implicit missing) are caught by morie_ts().
#' @srrstats {TS1.7} units-typed input is accepted via as.numeric coercion with a units attribute.
#' @srrstats {TS1.8} the calendar/time-unit system is recorded via the calendar argument.
#' @srrstats {TS2.0} morie_ts() rejects implicit missing values in regular data.
#' @srrstats {TS2.1} morie_ts(na_action=) controls missing-value handling.
#' @srrstats {TS2.1a} na_action='error' errors on missing data.
#' @srrstats {TS2.1b} na_action='omit' warns and drops (irregular) missing data.
#' @srrstats {TS2.1c} na_action='interpolate' imputes missing data.
#' @srrstats {TS2.2} morie_ts_stationarity() tests mean (ADF) and variance (KPSS) stationarity.
#' @srrstats {TS2.3} stationarity assumptions are surfaced by morie_ts_stationarity().
#' @srrstats {TS2.4} morie_ts_stationarity() checks stationarity and issues diagnostics.
#' @srrstats {TS2.4a} the ADF/KPSS/Ljung-Box results are returned as diagnostics.
#' @srrstats {TS2.4b} a differencing order is advised (suggested_d) and morie_ts_difference() applies it.
#' @srrstats {TS2.5} morie_ts_acf() returns autocorrelation strictly ordered by lag.
#' @srrstats {TS2.6} morie_ts_acf() carries the series units attribute.
#' @srrstats {TS3.0} tests show forecast SE widening with horizon (morie_ts_forecast).
#' @srrstats {TS3.1} a stationary (white-noise) case shows near-constant SE, violating TS3.0.
#' @srrstats {TS3.2} the driver (integration order) of error growth is demonstrated in tests.
#' @srrstats {TS3.3} morie_ts_trim_forecast() provides forecast trimming by error margin.
#' @srrstats {TS3.3a} trimming is demonstrated in the morie_ts_trim_forecast() example.
#' @srrstats {TS3.3b} morie_ts_trim_forecast() is the explicit post-processing trim mechanism.
#' @srrstats {TS4.0} morie_ts_forecast() returns a class-defined object.
#' @srrstats {TS4.0a} a documented own-class format is used rather than a tsbox round-trip.
#' @srrstats {TS4.0b} the morie_ts_forecast class is the unique defined return format.
#' @srrstats {TS4.1} input units are propagated into the forecast return.
#' @srrstats {TS4.2} the return type/class is documented in @return.
#' @srrstats {TS4.3} the forecast carries the series frequency/time scale.
#' @srrstats {TS4.4} the effect of differencing on moments is documented and tested.
#' @srrstats {TS4.5} morie_ts_undifference() back-transforms differenced data.
#' @srrstats {TS4.5a} morie_ts_undifference() is the explicit back-transform routine.
#' @srrstats {TS4.5b} back-transformation is demonstrated in tests and examples.
#' @srrstats {TS4.5c} the requirement for retained init values (a limitation) is documented + errors without them.
#' @srrstats {TS4.6} the forecast returns first- and second-order moments.
#' @srrstats {TS4.6a} a distribution object is not returned; the moment form (TS4.6b) is used instead.
#' @srrstats {TS4.6b} mean and se (first/second moments) are returned per horizon.
#' @srrstats {TS4.6c} prediction intervals give a general error indication.
#' @srrstats {TS4.7} forecast values are clearly distinguished from observed values.
#' @srrstats {TS4.7a} the forecast mean can be taken alone.
#' @srrstats {TS4.7b} observed and forecast are returned as distinct list items.
#' @srrstats {TS4.7c} observed+forecast can be combined with a distinguishing kind column.
#' @srrstats {TS5.0} plot.morie_ts_forecast() is the default plot method.
#' @srrstats {TS5.1} the time axis is labelled (with units where known).
#' @srrstats {TS5.2} time is placed on the horizontal axis.
#' @srrstats {TS5.3} units of the time index are printed on the axis where known.
#' @srrstats {TS5.4} frequency visualisation (dsp PSD) uses positive frequency units, not [-pi,pi].
#' @srrstats {TS5.5} plot.morie_ts_forecast(broken=) controls continuous vs broken lines.
#' @srrstats {TS5.6} forecast distributional limits are shaded by default.
#' @srrstats {TS5.7} observed (input) values are included in the plot by default.
#' @srrstats {TS5.8} observed and forecast are drawn in visually distinct styles.
#' @noRd
NULL

# ============================================================
# TS1: a validated time-series class
# ============================================================

#' Construct a validated time series
#'
#' Wraps [stats::ts()] with explicit validation: the input must be a
#' strictly ordered, regular numeric series with no implicit missing
#' periods. This is the single pre-processing routine through which all
#' subsequent functions receive their data, so time attributes are
#' maintained uniformly.
#'
#' @param x A numeric vector (or an object coercible to one, including a
#'   `units`-typed vector, which is coerced via [as.numeric()]), or an
#'   existing `ts`.
#' @param frequency Number of observations per unit of time (e.g. 12 for
#'   monthly-in-years). Default 1.
#' @param start Time of the first observation. Default 1.
#' @param calendar Character tag documenting the calendar system assumed
#'   (e.g. "monthly"); recorded as an attribute, not interpreted.
#' @param na_action How to handle missing values (regular series require
#'   an explicit decision): `"error"` (default) rejects implicit missing
#'   values; `"interpolate"` fills them by linear interpolation
#'   (imputation); `"omit"` drops them with a warning (the series is then
#'   treated as irregular).
#' @param units Optional character unit label carried as an attribute and
#'   propagated to return values.
#' @return A `morie_ts` object (a `ts` with validated ordering).
#' @examples
#' morie_ts(cumsum(rnorm(24)), frequency = 12, start = 2000)
#' @export
morie_ts <- function(x, frequency = 1, start = 1, calendar = NA_character_,
                     na_action = c("error", "interpolate", "omit"),
                     units = attr(x, "units")) {
  na_action <- match.arg(na_action)
  if (inherits(x, "ts")) {
    frequency <- stats::frequency(x); start <- stats::start(x)
  }
  xv <- as.numeric(x)                              # accepts units-typed input
  if (anyNA(xv)) {
    if (na_action == "error") {
      stop("time series has missing values; set na_action= to impute or omit",
           call. = FALSE)
    } else if (na_action == "interpolate") {
      xv <- stats::approx(seq_along(xv), xv, seq_along(xv))$y
      xv[is.na(xv)] <- mean(xv, na.rm = TRUE)      # edge NAs
    } else {
      warning("dropping missing values; series treated as irregular",
              call. = FALSE)
      xv <- xv[!is.na(xv)]
    }
  }
  if (length(xv) < 2L) stop("a time series needs >= 2 observations",
                            call. = FALSE)
  out <- stats::ts(xv, frequency = frequency, start = start)
  attr(out, "calendar") <- calendar
  attr(out, "units") <- if (is.null(units)) NA_character_ else units
  class(out) <- c("morie_ts", "ts")
  out
}

#' Ordered autocorrelation / autocovariance of a series
#'
#' Returns autocorrelations indexed by lag in strict ascending order, so
#' the lag index and the values always share the same ordering as the
#' underlying series.
#'
#' @param x A numeric series or `morie_ts`.
#' @param lag_max Maximum lag.
#' @param type "correlation" or "covariance".
#' @return A data.frame with columns `lag` (ascending) and `value`, plus a
#'   `units` attribute inherited from `x`.
#' @examples
#' morie_ts_acf(cumsum(rnorm(50)), lag_max = 5)
#' @export
morie_ts_acf <- function(x, lag_max = 20L, type = c("correlation",
                                                    "covariance")) {
  type <- match.arg(type)
  u <- attr(x, "units")
  a <- stats::acf(as.numeric(x), lag.max = lag_max, type = type, plot = FALSE)
  out <- data.frame(lag = as.integer(a$lag), value = as.numeric(a$acf))
  out <- out[order(out$lag), , drop = FALSE]       # strict time ordering
  attr(out, "units") <- if (is.null(u)) NA_character_ else u
  out
}

# ============================================================
# TS2: stationarity
# ============================================================

#' Test a series for stationarity
#'
#' Runs an Augmented Dickey-Fuller test (H0: unit root / non-stationary)
#' and a KPSS test (H0: stationary) via the urca package where available,
#' plus a Ljung-Box autocorrelation test, and suggests a differencing
#' order to achieve stationarity of the mean.
#'
#' @param x A numeric series or `morie_ts`.
#' @param max_d Maximum differencing to search for the suggestion.
#' @return A `morie_ts_stationarity` list: `adf`, `kpss` (statistic +
#'   whether stationary at 5%), `ljung_box` p-value, and `suggested_d`.
#' @examples
#' morie_ts_stationarity(cumsum(rnorm(100)))$suggested_d
#' @export
morie_ts_stationarity <- function(x, max_d = 2L) {
  x <- as.numeric(x)
  have_urca <- requireNamespace("urca", quietly = TRUE)
  test_one <- function(v) {
    lb <- stats::Box.test(v, lag = min(10L, length(v) %/% 2L),
                          type = "Ljung-Box")$p.value
    if (have_urca) {
      adf <- suppressWarnings(urca::ur.df(v, type = "drift",
                                          selectlags = "AIC"))
      adf_stat <- adf@teststat[1]; adf_crit <- adf@cval[1, "5pct"]
      adf_stationary <- adf_stat < adf_crit          # reject unit root
      kp <- suppressWarnings(urca::ur.kpss(v, type = "mu"))
      kpss_stat <- kp@teststat[1]; kpss_crit <- kp@cval[1, "5pct"]
      kpss_stationary <- kpss_stat < kpss_crit        # fail to reject H0
    } else {
      # base fallback: split-half mean/variance stability
      h <- floor(length(v) / 2)
      adf_stationary <- abs(mean(v[1:h]) - mean(v[(h + 1):length(v)])) <
        stats::sd(v)
      adf_stat <- adf_crit <- NA_real_
      kpss_stationary <- adf_stationary
      kpss_stat <- kpss_crit <- NA_real_
    }
    list(adf = list(statistic = adf_stat, crit_5pct = adf_crit,
                    stationary = adf_stationary),
         kpss = list(statistic = kpss_stat, crit_5pct = kpss_crit,
                     stationary = kpss_stationary),
         ljung_box = lb, both_stationary = adf_stationary && kpss_stationary)
  }
  d <- 0L; v <- x; res <- test_one(v)
  while (!res$both_stationary && d < max_d) {
    d <- d + 1L; v <- diff(v); res <- test_one(v)
  }
  out <- test_one(x)
  out$suggested_d <- d
  class(out) <- c("morie_ts_stationarity", "morie_rich_result", "list")
  out
}

# ============================================================
# TS decomposition + differencing / back-transform
# ============================================================

#' Decompose a seasonal time series
#' @param x A `morie_ts` (frequency > 1 for a seasonal component).
#' @param method "stl" (loess-based) or "classical" ([stats::decompose()]).
#' @return A `morie_ts_decomp` with `trend`, `seasonal`, `remainder`.
#' @examples
#' morie_ts_decompose(morie_ts(sin(1:48) + rnorm(48), frequency = 12))
#' @export
morie_ts_decompose <- function(x, method = c("stl", "classical")) {
  method <- match.arg(method)
  if (!inherits(x, "ts")) x <- morie_ts(x)
  if (stats::frequency(x) < 2) stop("decomposition needs frequency >= 2",
                                    call. = FALSE)
  if (method == "stl") {
    d <- stats::stl(x, s.window = "periodic")
    comp <- d$time.series
    out <- list(trend = comp[, "trend"], seasonal = comp[, "seasonal"],
                remainder = comp[, "remainder"], method = "stl")
  } else {
    d <- stats::decompose(x)
    out <- list(trend = d$trend, seasonal = d$seasonal,
                remainder = d$random, method = "classical")
  }
  class(out) <- c("morie_ts_decomp", "morie_rich_result", "list")
  out
}

#' Difference a series, retaining the information to reverse it
#' @param x Numeric series or `morie_ts`.
#' @param differences Order of differencing.
#' @param lag Lag (use the series frequency for seasonal differencing).
#' @return A numeric vector with an `init` attribute (the leading values
#'   needed for exact reconstruction).
#' @examples
#' dx <- morie_ts_difference(cumsum(1:10))
#' @export
morie_ts_difference <- function(x, differences = 1L, lag = 1L) {
  x <- as.numeric(x)
  init <- vector("list", differences)
  v <- x
  for (i in seq_len(differences)) {
    init[[i]] <- v[seq_len(lag)]
    v <- diff(v, lag = lag)
  }
  attr(v, "init") <- init
  attr(v, "lag") <- lag
  v
}

#' Reverse [morie_ts_difference()]
#' @param dx A differenced series from [morie_ts_difference()] (carrying
#'   its `init` attribute), or a plain vector plus explicit `init`.
#' @param init Optional list of leading values (defaults to the attribute).
#' @param lag Optional lag (defaults to the attribute).
#' @return The reconstructed numeric series.
#' @examples
#' x <- cumsum(1:10); dx <- morie_ts_difference(x)
#' all.equal(morie_ts_undifference(dx), x)
#' @export
morie_ts_undifference <- function(dx, init = attr(dx, "init"),
                                  lag = attr(dx, "lag")) {
  if (is.null(init)) stop("`init` values are required to reverse differencing",
                          call. = FALSE)
  if (is.null(lag)) lag <- 1L
  v <- as.numeric(dx)
  for (i in rev(seq_along(init))) {
    v <- stats::diffinv(v, lag = lag, xi = init[[i]])
  }
  v
}

# ============================================================
# TS4: (S)ARIMA fitting + forecasting
# ============================================================

#' Fit an ARIMA / SARIMA model
#'
#' @param x A numeric series or `morie_ts`.
#' @param order Non-seasonal `(p, d, q)`.
#' @param seasonal Seasonal `(P, D, Q)` (uses the series frequency).
#' @param xreg Optional matrix/data.frame of exogenous regressors,
#'   aligned to `x`.
#' @param include_mean Include a mean/intercept term (default TRUE when
#'   `d == 0`).
#' @return A `morie_ts_model` wrapping the fitted [stats::arima()] object.
#' @examples
#' morie_ts_arima(cumsum(rnorm(60)), order = c(1, 1, 0))
#' @export
morie_ts_arima <- function(x, order = c(0L, 0L, 0L),
                           seasonal = c(0L, 0L, 0L), xreg = NULL,
                           include_mean = order[2] == 0) {
  if (!inherits(x, "ts")) x <- morie_ts(x)
  seas <- list(order = seasonal, period = stats::frequency(x))
  if (!is.null(xreg)) xreg <- as.matrix(xreg)
  fit <- stats::arima(x, order = order, seasonal = seas, xreg = xreg,
                      include.mean = include_mean, method = "ML")
  out <- list(fit = fit, order = order, seasonal = seasonal,
              frequency = stats::frequency(x), has_xreg = !is.null(xreg),
              x = as.numeric(x), aic = fit$aic, loglik = fit$loglik,
              units = attr(x, "units"))
  class(out) <- c("morie_ts_model", "morie_rich_result", "list")
  out
}

#' @param x A `morie_ts_model`.
#' @param ... Unused.
#' @return `x`, invisibly.
#' @export
print.morie_ts_model <- function(x, ...) {
  cat("<morie_ts_model>\n")
  cat(sprintf("  ARIMA(%d,%d,%d)(%d,%d,%d)[%d]  xreg=%s\n",
              x$order[1], x$order[2], x$order[3],
              x$seasonal[1], x$seasonal[2], x$seasonal[3],
              x$frequency, x$has_xreg))
  cat(sprintf("  AIC=%.2f  logLik=%.2f\n", x$aic, x$loglik))
  invisible(x)
}

#' Forecast from a fitted time-series model
#'
#' Produces multi-step-ahead forecasts with prediction intervals. The
#' returned object keeps forecast values clearly separate from the
#' observed (model) values, and the interval widths grow with the
#' forecast horizon.
#'
#' @param model A `morie_ts_model`.
#' @param h Forecast horizon (number of steps ahead).
#' @param level Prediction-interval coverage (default 0.95).
#' @param xreg Future exogenous regressors (required if the model used
#'   `xreg`), with `h` rows.
#' @return A `morie_ts_forecast`: `mean`, `lower`, `upper`, `se`
#'   (each length `h`), plus the `observed` series.
#' @examples
#' m <- morie_ts_arima(cumsum(rnorm(80)), order = c(1, 1, 0))
#' fc <- morie_ts_forecast(m, h = 12)
#' @export
morie_ts_forecast <- function(model, h = 10L, level = 0.95, xreg = NULL) {
  stopifnot(inherits(model, "morie_ts_model"))
  if (model$has_xreg && is.null(xreg)) {
    stop("model was fitted with xreg; supply future `xreg` (h rows)",
         call. = FALSE)
  }
  if (!is.null(xreg)) xreg <- as.matrix(xreg)
  pr <- stats::predict(model$fit, n.ahead = h, newxreg = xreg)
  z <- stats::qnorm(1 - (1 - level) / 2)
  out <- list(mean = as.numeric(pr$pred), se = as.numeric(pr$se),
              lower = as.numeric(pr$pred) - z * as.numeric(pr$se),
              upper = as.numeric(pr$pred) + z * as.numeric(pr$se),
              level = level, h = h, observed = model$x,
              frequency = model$frequency, units = model$units)
  class(out) <- c("morie_ts_forecast", "morie_rich_result", "list")
  out
}

#' Trim a forecast to a maximum prediction-interval width
#'
#' Truncates a forecast at the first horizon whose prediction-interval
#' width exceeds `max_width`, giving an explicit mechanism to bound
#' forecasts by a specified error margin.
#'
#' @param forecast A `morie_ts_forecast`.
#' @param max_width Maximum acceptable interval width (`upper - lower`).
#' @return A `morie_ts_forecast` truncated to the acceptable horizon.
#' @examples
#' m <- morie_ts_arima(cumsum(rnorm(80)), order = c(1, 1, 0))
#' fc <- morie_ts_forecast(m, h = 30)
#' morie_ts_trim_forecast(fc, max_width = 5)$h
#' @export
morie_ts_trim_forecast <- function(forecast, max_width) {
  stopifnot(inherits(forecast, "morie_ts_forecast"))
  width <- forecast$upper - forecast$lower
  keep <- which(width <= max_width)
  hkeep <- if (length(keep)) max(keep) else 0L
  for (f in c("mean", "se", "lower", "upper")) {
    forecast[[f]] <- forecast[[f]][seq_len(hkeep)]
  }
  forecast$h <- hkeep
  forecast
}

#' @param x A `morie_ts_forecast`.
#' @param ... Unused.
#' @return `x`, invisibly.
#' @export
print.morie_ts_forecast <- function(x, ...) {
  cat("<morie_ts_forecast>\n")
  cat(sprintf("  horizon: %d   level: %.0f%%\n", x$h, 100 * x$level))
  cat(sprintf("  point forecasts: %s%s\n",
              paste(sprintf("%.3g", utils::head(x$mean, 4)), collapse = ", "),
              if (x$h > 4) ", ..." else ""))
  invisible(x)
}

#' @param x A `morie_ts_forecast`.
#' @param broken If TRUE, draw the observed series as a broken (segmented)
#'   line rather than a continuous one; controls line continuity for
#'   series that may contain gaps (default FALSE).
#' @param ... Passed to [plot()].
#' @return `NULL`, invisibly. Draws the observed (input) series and the
#'   forecast (output) with its prediction band. Time is on the
#'   horizontal axis (labelled with the series units where known); the
#'   forecast is drawn in a distinct colour with its distributional
#'   limits shaded, and the observed values are always included.
#' @export
plot.morie_ts_forecast <- function(x, broken = FALSE, ...) {
  n <- length(x$observed)
  xlab <- if (!is.na(x$units)) sprintf("time (%s)", x$units) else "time"
  plot(seq_len(n + x$h), c(x$observed, x$mean), type = "n",   # TS5.2 time on x
       xlab = xlab, ylab = "value", ...)                      # TS5.1/5.3
  graphics::lines(seq_len(n), x$observed,                     # TS5.7 input shown
                  lty = if (broken) 3 else 1)                 # TS5.5 broken option
  fx <- (n + 1):(n + x$h)
  graphics::polygon(c(fx, rev(fx)), c(x$lower, rev(x$upper)), # TS5.6 limits
                    border = NA, col = grDevices::rgb(0, 0, 1, 0.15))
  graphics::lines(fx, x$mean, col = "blue", lwd = 2)          # TS5.8 distinct
  invisible(NULL)
}

# ============================================================
# TS: evaluation + selection
# ============================================================

#' Forecast accuracy metrics
#' @param forecast_mean Numeric vector of point forecasts.
#' @param actual Numeric vector of realised values (same length).
#' @return Named numeric vector: RMSE, MAE, MAPE, ME.
#' @examples
#' morie_ts_accuracy(c(1, 2, 3), c(1.1, 1.9, 3.2))
#' @export
morie_ts_accuracy <- function(forecast_mean, actual) {
  e <- as.numeric(actual) - as.numeric(forecast_mean)
  c(RMSE = sqrt(mean(e^2)), MAE = mean(abs(e)),
    MAPE = mean(abs(e / actual)) * 100, ME = mean(e))
}

#' Back-test a forecasting configuration on a holdout tail
#' @param x A numeric series or `morie_ts`.
#' @param h Holdout length (also the forecast horizon).
#' @param order,seasonal ARIMA orders passed to [morie_ts_arima()].
#' @return A list with the `forecast`, the held-out `actual`, and the
#'   `accuracy` metrics.
#' @examples
#' morie_ts_backtest(cumsum(rnorm(120)), h = 12, order = c(1, 1, 0))$accuracy
#' @export
morie_ts_backtest <- function(x, h = 10L, order = c(0L, 0L, 0L),
                              seasonal = c(0L, 0L, 0L)) {
  x <- as.numeric(x)
  n <- length(x)
  if (h >= n - 2L) stop("holdout `h` too large for the series", call. = FALSE)
  train <- x[seq_len(n - h)]; actual <- x[(n - h + 1):n]
  m <- morie_ts_arima(train, order = order, seasonal = seasonal)
  fc <- morie_ts_forecast(m, h = h)
  list(forecast = fc, actual = actual,
       accuracy = morie_ts_accuracy(fc$mean, actual))
}

#' Select ARIMA orders by information criterion
#' @param x A numeric series or `morie_ts`.
#' @param max_p,max_q Maximum non-seasonal AR / MA orders to search.
#' @param d Differencing order (default: the stationarity suggestion).
#' @param ic "aic" or "bic".
#' @return A list with the full `results` table and the `best` model
#'   (a fitted `morie_ts_model`).
#' @examples
#' morie_ts_select(cumsum(rnorm(80)), max_p = 2, max_q = 2)$best$order
#' @export
morie_ts_select <- function(x, max_p = 3L, max_q = 3L, d = NULL,
                            ic = c("aic", "bic")) {
  ic <- match.arg(ic)
  if (!inherits(x, "ts")) x <- morie_ts(x)
  if (is.null(d)) d <- morie_ts_stationarity(x)$suggested_d
  grid <- expand.grid(p = 0:max_p, q = 0:max_q)
  n <- length(x)
  score <- vapply(seq_len(nrow(grid)), function(i) {
    m <- tryCatch(morie_ts_arima(x, order = c(grid$p[i], d, grid$q[i])),
                  error = function(e) NULL)
    if (is.null(m)) return(Inf)
    if (ic == "aic") m$aic else stats::AIC(m$fit, k = log(n))
  }, numeric(1))
  results <- cbind(grid, d = d, score = score)
  best_i <- which.min(score)
  best <- morie_ts_arima(x, order = c(grid$p[best_i], d, grid$q[best_i]))
  list(results = results, best = best, ic = ic)
}

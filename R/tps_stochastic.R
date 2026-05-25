# SPDX-License-Identifier: AGPL-3.0-or-later
#' Stochastic-physics-of-crime analyses for TPS data
#'
#' R port of \code{morie.tps_stochastic}. Four jurisdiction-agnostic
#' callables: temporal-only exponential Hawkes self-exciting fit,
#' seasonal ARIMA forecast on monthly counts, Euler-Maruyama
#' Ornstein-Uhlenbeck simulation, and a 1-D Fokker-Planck density
#' evolution. The R port keeps optimisation in base R
#' (\code{stats::optim}) and the seasonal forecast in
#' \code{stats::arima} so no external time-series package is needed.
#'
#' All functions return a multi-section \code{morie_rich_result} list.
#'
#' Functions
#' ---------
#'
#' \itemize{
#'   \item \code{\link{morie_tps_hawkes_temporal_fit}}: fit mu, kappa,
#'     omega of an exponential-kernel Hawkes process to incident
#'     times; report branching ratio + AIC/BIC.
#'   \item \code{\link{morie_tps_sarima_forecast}}: seasonal ARIMA on
#'     monthly counts with train / hold-out MAPE.
#'   \item \code{\link{morie_tps_langevin_simulate}}: Euler-Maruyama
#'     OU SDE paths fitted to daily counts.
#'   \item \code{\link{morie_tps_fokker_planck_grid}}: 1-D
#'     finite-difference density evolution under OU drift+diffusion.
#' }
#'
#' References: Mohler et al. 2011 (self-exciting point process crime);
#' Short, D'Orsogna, Bertozzi 2010 (stochastic physics of crime).
#'
#' @name morie_tps_stochastic
NULL


# ---------------------------------------------------------------------------
# Internal helpers (NOT exported)
# ---------------------------------------------------------------------------

.tps_stoch_result <- function(title, call, summary_lines = list(),
                               warnings = character(0),
                               interpretation = "",
                               ...) {
  out <- list(
    title = title,
    call = call,
    summary_lines = summary_lines,
    warnings = warnings,
    interpretation = interpretation,
    ...
  )
  class(out) <- c("morie_tps_stochastic_result",
                   "morie_rich_result", "list")
  out
}

.tps_stoch_round <- function(x, k) {
  if (!is.finite(x)) return(NA_real_)
  round(x, k)
}

# Return a numeric vector of incident times (POSIXct).
# Prefer integer-triple OCC_YEAR/OCC_MONTH/OCC_DAY (local-time
# decomposition unaffected by ArcGIS UTC conversion); fall back to
# OCC_DATE / REPORT_DATE.
.tps_stoch_date_series <- function(df, min_year = 2014L) {
  ts <- NULL
  if (all(c("OCC_YEAR", "OCC_MONTH", "OCC_DAY") %in% names(df))) {
    y <- suppressWarnings(as.integer(df[["OCC_YEAR"]]))
    m <- suppressWarnings(as.integer(df[["OCC_MONTH"]]))
    d <- suppressWarnings(as.integer(df[["OCC_DAY"]]))
    keep <- !is.na(y) & !is.na(m) & !is.na(d)
    if (any(keep)) {
      ts <- suppressWarnings(as.POSIXct(
        sprintf("%04d-%02d-%02d", y[keep], m[keep], d[keep]),
        tz = "UTC"
      ))
      ts <- ts[!is.na(ts)]
    }
  }
  if (is.null(ts) || length(ts) == 0L) {
    for (col in c("OCC_DATE", "REPORT_DATE")) {
      if (col %in% names(df)) {
        ts <- suppressWarnings(as.POSIXct(
          as.character(df[[col]]), tz = "UTC"))
        ts <- ts[!is.na(ts)]
        if (length(ts) > 0L) break
      }
    }
  }
  if (is.null(ts) || length(ts) == 0L) {
    return(as.POSIXct(character(0), tz = "UTC"))
  }
  if (!is.null(min_year)) {
    yrs <- as.POSIXlt(ts)$year + 1900L
    ts <- ts[yrs >= as.integer(min_year)]
  }
  sort(ts)
}

# Negative log-likelihood of exponential-kernel Hawkes:
#   lambda(t) = mu + kappa*omega * sum_{t_i<t} exp(-omega*(t - t_i))
# Closed-form integral: mu*T + kappa * sum_i (1 - exp(-omega*(T - t_i)))
.tps_stoch_neg_loglik_hawkes <- function(params, t, T_window) {
  mu <- params[1L]
  kappa <- params[2L]
  omega <- params[3L]
  if (mu <= 0 || kappa < 0 || omega <= 0) return(1e12)
  n <- length(t)
  lam <- numeric(n)
  A <- 0.0
  last <- t[1L]
  lam[1L] <- mu
  if (n > 1L) {
    for (i in 2L:n) {
      A <- (A + 1) * exp(-omega * (t[i] - last))
      lam[i] <- mu + kappa * omega * A
      last <- t[i]
    }
  }
  log_lam <- log(pmax(lam, 1e-300))
  integral <- mu * T_window +
              kappa * sum(1 - exp(-omega * (T_window - t)))
  -(sum(log_lam) - integral)
}


# Build daily counts from a POSIXct vector. Returns list(dates, counts).
.tps_stoch_daily <- function(ts) {
  if (length(ts) == 0L) {
    return(list(dates = as.POSIXct(character(0), tz = "UTC"),
                counts = integer(0)))
  }
  d <- format(ts, "%Y-%m-%d")
  tab <- table(d)
  ord <- order(names(tab))
  tab <- tab[ord]
  dates <- suppressWarnings(as.POSIXct(names(tab), tz = "UTC"))
  list(dates = dates, counts = as.integer(tab))
}

# Build monthly counts from a POSIXct vector.
.tps_stoch_monthly <- function(ts) {
  if (length(ts) == 0L) {
    return(list(dates = as.POSIXct(character(0), tz = "UTC"),
                counts = integer(0)))
  }
  m <- format(ts, "%Y-%m")
  tab <- table(m)
  ord <- order(names(tab))
  tab <- tab[ord]
  dates <- suppressWarnings(as.POSIXct(paste0(names(tab), "-01"),
                                         tz = "UTC"))
  list(dates = dates, counts = as.integer(tab))
}


# ---------------------------------------------------------------------------
# 1. hawkes_temporal_fit
# ---------------------------------------------------------------------------

#' Temporal exponential-kernel Hawkes fit
#'
#' Maximum-likelihood fit of a temporal-only exponential Hawkes
#' process to incident times. Optimisation runs in base R
#' (\code{stats::optim}, Nelder-Mead). Reports background rate mu,
#' branching ratio kappa, decay omega, and the AIC / BIC of the fit.
#'
#' @param df A \code{data.frame} with TPS-shaped date columns.
#' @param ds_name Character label for the dataset.
#' @param max_n Maximum number of incident times to fit (random
#'   subsample seeded with 42 if exceeded).
#' @return A \code{morie_rich_result} list with \code{mu},
#'   \code{kappa}, \code{omega}, \code{branching}, \code{nll},
#'   \code{aic}, \code{bic}.
#' @export
morie_tps_hawkes_temporal_fit <- function(df, ds_name = "?",
                                           max_n = 5000L) {
  stopifnot(is.data.frame(df))
  call_str <- sprintf(
    "morie_tps_hawkes_temporal_fit(df=<%dr>, max_n=%d)",
    nrow(df), as.integer(max_n))

  dt <- .tps_stoch_date_series(df)
  if (length(dt) < 100L) {
    return(.tps_stoch_result(
      sprintf("Hawkes -- %s", ds_name), call_str,
      warnings = sprintf("only %d timestamps", length(dt)),
      interpretation = "No analysis: at least 100 timestamps required."
    ))
  }
  if (length(dt) > as.integer(max_n)) {
    set.seed(42L)
    dt <- sort(dt[sample.int(length(dt), as.integer(max_n))])
  }
  t0 <- min(dt)
  t <- as.numeric(difftime(dt, t0, units = "days"))
  t <- sort(t)
  T_window <- t[length(t)]
  n <- length(t)

  mean_dt <- mean(diff(t))
  if (!is.finite(mean_dt) || mean_dt == 0) mean_dt <- 1.0
  x0 <- c(mu = n / T_window, kappa = 0.4, omega = 1.0 / mean_dt)
  fit <- tryCatch(
    stats::optim(x0, .tps_stoch_neg_loglik_hawkes,
                  t = t, T_window = T_window,
                  method = "Nelder-Mead",
                  control = list(maxit = 1000L,
                                  reltol = 1e-4)),
    error = function(e) e
  )
  if (inherits(fit, "error")) {
    return(.tps_stoch_result(
      sprintf("Hawkes -- %s", ds_name), call_str,
      warnings = sprintf("fit failed: %s",
                           conditionMessage(fit)),
      interpretation = "No analysis: Hawkes optimisation failed."
    ))
  }
  mu <- fit$par[1L]
  kappa <- fit$par[2L]
  omega <- fit$par[3L]
  nll <- fit$value
  aic <- 2 * 3 + 2 * nll
  bic <- 3 * log(n) + 2 * nll

  warnings <- character(0)
  if (kappa >= 1) {
    warnings <- c(warnings,
      "Branching ratio kappa >= 1: process is non-stationary / explosive; treat with care.")
  }
  if (omega <= 0 || !is.finite(omega)) {
    warnings <- c(warnings, "Decay omega not positive-finite; fit unstable.")
  }

  stationary_text <- if (kappa < 1)
    "Branching ratio kappa < 1 means the process is stationary (does not explode). Mohler-style self-excitation is operating: events cluster in time around prior events."
  else
    "kappa >= 1 means the fit is EXPLOSIVE; treat with care."
  interp <- sprintf(
    paste0("Background rate %.3f events/day; each event triggers ",
           "on average %.2f offspring with decay timescale ",
           "1/omega = %.2f days. %s"),
    mu, kappa, 1.0 / max(omega, 1e-12), stationary_text)

  .tps_stoch_result(
    sprintf("Hawkes self-exciting fit -- %s", ds_name), call_str,
    summary_lines = list(
      `Events fitted` = n,
      `Time window (days)` = .tps_stoch_round(T_window, 1),
      `mu (background rate, /day)` = .tps_stoch_round(mu, 4),
      `kappa (branching ratio)` = .tps_stoch_round(kappa, 4),
      `omega (decay, /day)` = .tps_stoch_round(omega, 4),
      `Mean offspring per event` = .tps_stoch_round(kappa, 3),
      `Stationary?` = if (kappa < 1) "Yes (kappa<1)" else "EXPLOSIVE",
      `Negative log-likelihood` = .tps_stoch_round(nll, 1),
      `AIC` = .tps_stoch_round(aic, 1),
      `BIC` = .tps_stoch_round(bic, 1)
    ),
    warnings = warnings,
    interpretation = interp,
    n = n,
    mu = as.numeric(mu),
    kappa = as.numeric(kappa),
    omega = as.numeric(omega),
    branching = as.numeric(kappa),
    nll = as.numeric(nll),
    aic = as.numeric(aic),
    bic = as.numeric(bic),
    T_days = as.numeric(T_window)
  )
}


# ---------------------------------------------------------------------------
# 2. sarima_forecast
# ---------------------------------------------------------------------------

#' Seasonal ARIMA forecast on monthly incident counts
#'
#' Hold-out validation forecast: fits SARIMA(p,d,q)(P,D,Q)_12 with
#' \code{stats::arima} to the leading training months, forecasts the
#' last \code{h} months, and reports MAPE / RMSE.
#'
#' @param df A \code{data.frame}.
#' @param ds_name Character label.
#' @param h Hold-out horizon in months (default 12).
#' @param order Non-seasonal ARIMA order \code{c(p,d,q)}.
#' @param seasonal Seasonal order \code{c(P,D,Q,s)}; the 4th element
#'   is the seasonal period.
#' @return A \code{morie_rich_result} list with \code{aic},
#'   \code{bic}, \code{mape_pct}, \code{rmse}, \code{forecast},
#'   \code{actual}.
#' @export
morie_tps_sarima_forecast <- function(df, ds_name = "?", h = 12L,
                                       order = c(1L, 1L, 1L),
                                       seasonal = c(0L, 1L, 1L, 12L)) {
  stopifnot(is.data.frame(df), length(order) == 3L,
            length(seasonal) == 4L)
  call_str <- sprintf(
    "morie_tps_sarima_forecast(df=<%dr>, h=%d)",
    nrow(df), as.integer(h))

  dt <- .tps_stoch_date_series(df)
  if (length(dt) < 36L) {
    return(.tps_stoch_result(
      sprintf("SARIMA -- %s", ds_name), call_str,
      warnings = sprintf("need >=36 timestamps, got %d", length(dt)),
      interpretation = "No analysis: at least 36 timestamps required."
    ))
  }
  monthly <- .tps_stoch_monthly(dt)
  if (length(monthly$counts) < 36L) {
    return(.tps_stoch_result(
      sprintf("SARIMA -- %s", ds_name), call_str,
      warnings = sprintf("only %d months", length(monthly$counts)),
      interpretation = "No analysis: at least 36 months required."
    ))
  }
  h <- as.integer(h)
  n_total <- length(monthly$counts)
  train_n <- n_total - h
  if (train_n < 24L) {
    return(.tps_stoch_result(
      sprintf("SARIMA -- %s", ds_name), call_str,
      warnings = sprintf("only %d training months after hold-out", train_n),
      interpretation = "No analysis: training window too small."
    ))
  }
  train <- monthly$counts[seq_len(train_n)]
  test  <- monthly$counts[(train_n + 1L):n_total]
  test_dates <- monthly$dates[(train_n + 1L):n_total]

  first <- as.POSIXlt(monthly$dates[1])
  start_year <- first$year + 1900L
  start_month <- first$mon + 1L
  ts_train <- stats::ts(train, start = c(start_year, start_month),
                         frequency = as.integer(seasonal[4L]))

  fit <- tryCatch(
    stats::arima(ts_train,
                  order = as.integer(order[1:3]),
                  seasonal = list(order = as.integer(seasonal[1:3]),
                                   period = as.integer(seasonal[4L]))),
    error = function(e) e
  )
  if (inherits(fit, "error")) {
    return(.tps_stoch_result(
      sprintf("SARIMA -- %s", ds_name), call_str,
      warnings = sprintf("fit failed: %s",
                           conditionMessage(fit)),
      interpretation = "No analysis: SARIMA fit failed."
    ))
  }
  pred <- tryCatch(stats::predict(fit, n.ahead = h),
                    error = function(e) e)
  if (inherits(pred, "error")) {
    return(.tps_stoch_result(
      sprintf("SARIMA -- %s", ds_name), call_str,
      warnings = sprintf("predict failed: %s",
                           conditionMessage(pred)),
      interpretation = "No analysis: SARIMA forecast failed."
    ))
  }
  fc <- as.numeric(pred$pred)
  err <- as.numeric(test) - fc
  mape <- mean(abs(err / pmax(as.numeric(test), 1))) * 100
  rmse <- sqrt(mean(err ^ 2))

  aic <- as.numeric(stats::AIC(fit))
  k <- length(stats::coef(fit)) +
       (if ("sigma2" %in% names(fit)) 1L else 0L)
  bic <- aic + k * (log(train_n) - 2)

  ord_str <- sprintf("(%d,%d,%d)", order[1], order[2], order[3])
  seas_str <- sprintf("(%d,%d,%d)_%d",
                       seasonal[1], seasonal[2], seasonal[3], seasonal[4])

  .tps_stoch_result(
    sprintf("SARIMA%s x %s forecast -- %s",
              ord_str, seas_str, ds_name), call_str,
    summary_lines = list(
      `Training months` = train_n,
      `Hold-out months (h)` = h,
      `AIC` = .tps_stoch_round(aic, 1),
      `BIC` = .tps_stoch_round(bic, 1),
      `Hold-out MAPE` = sprintf("%.1f%%", mape),
      `Hold-out RMSE` = .tps_stoch_round(rmse, 1),
      `Forecast mean` = .tps_stoch_round(mean(fc), 1),
      `First test month` = format(test_dates[1], "%Y-%m-%d"),
      `Last test month` =
        format(test_dates[length(test_dates)], "%Y-%m-%d")
    ),
    interpretation = sprintf(
      paste0("SARIMA%s x %s trained on %d months, ",
             "held out %d. Hold-out MAPE = %.1f%%, RMSE = %.1f."),
      ord_str, seas_str, train_n, h, mape, rmse),
    n = n_total,
    aic = aic, bic = bic,
    mape_pct = mape, rmse = rmse,
    forecast = fc,
    actual = as.integer(test),
    test_dates = test_dates
  )
}


# ---------------------------------------------------------------------------
# 3. langevin_simulate
# ---------------------------------------------------------------------------

#' Euler-Maruyama Ornstein-Uhlenbeck simulation
#'
#' Fits an OU process \eqn{dX_t = theta(mu - X_t) dt + sigma dW_t}
#' to daily incident counts via OLS on first-differences, then runs
#' \code{n_paths} forward simulations of length \code{T_days}.
#'
#' @param df A \code{data.frame}.
#' @param ds_name Character label.
#' @param n_paths Number of forward paths to simulate.
#' @param T_days Forward horizon in days.
#' @param dt Time step (days).
#' @param seed RNG seed.
#' @return A \code{morie_rich_result} list with \code{theta},
#'   \code{mu}, \code{sigma}, \code{paths} (matrix of n_paths x
#'   n_steps), and final-day quantiles.
#' @export
morie_tps_langevin_simulate <- function(df, ds_name = "?",
                                         n_paths = 100L,
                                         T_days = 365L,
                                         dt = 1.0, seed = 42L) {
  stopifnot(is.data.frame(df))
  call_str <- sprintf(
    "morie_tps_langevin_simulate(df=<%dr>, n_paths=%d, T_days=%d)",
    nrow(df), as.integer(n_paths), as.integer(T_days))

  dt_ser <- .tps_stoch_date_series(df)
  if (length(dt_ser) < 60L) {
    return(.tps_stoch_result(
      sprintf("Langevin OU -- %s", ds_name), call_str,
      warnings = sprintf("only %d timestamps", length(dt_ser)),
      interpretation = "No analysis: at least 60 timestamps required."
    ))
  }
  daily <- .tps_stoch_daily(dt_ser)
  x <- as.numeric(daily$counts)
  if (length(x) < 2L) {
    return(.tps_stoch_result(
      sprintf("Langevin OU -- %s", ds_name), call_str,
      warnings = sprintf("only %d daily buckets", length(x)),
      interpretation = "No analysis: at least two daily buckets required."
    ))
  }
  dx <- diff(x)
  x_lag <- x[-length(x)]
  fit_ols <- stats::lm(dx ~ x_lag)
  ab <- stats::coef(fit_ols)
  a <- as.numeric(ab[1])
  b <- as.numeric(ab[2])
  theta <- max(1e-6, -b)
  mu <- if (b != 0) -a / b else mean(x)
  resid <- dx - (a + b * x_lag)
  sigma <- stats::sd(resid)
  if (!is.finite(sigma)) sigma <- 0.0

  n_steps <- max(1L, as.integer(T_days / dt))
  set.seed(as.integer(seed))
  paths <- matrix(0.0, nrow = as.integer(n_paths), ncol = n_steps)
  paths[, 1L] <- x[length(x)]
  sqrtdt <- sqrt(dt)
  if (n_steps > 1L) {
    for (k in 2L:n_steps) {
      z <- stats::rnorm(as.integer(n_paths))
      paths[, k] <- paths[, k - 1L] +
                     theta * (mu - paths[, k - 1L]) * dt +
                     sigma * sqrtdt * z
    }
  }
  final <- paths[, n_steps]
  qs <- stats::quantile(final, c(0.05, 0.5, 0.95), names = FALSE)
  half_life <- log(2) / max(theta, 1e-6)

  .tps_stoch_result(
    sprintf("Langevin OU SDE simulation -- %s", ds_name), call_str,
    summary_lines = list(
      `Historical days observed` = length(x),
      `Fitted theta (mean reversion)` = .tps_stoch_round(theta, 4),
      `Fitted mu (long-run mean)` = .tps_stoch_round(mu, 2),
      `Fitted sigma (volatility)` = .tps_stoch_round(sigma, 3),
      `Half-life of shocks (days)` = .tps_stoch_round(half_life, 2),
      `Simulated paths` = as.integer(n_paths),
      `Forecast horizon (days)` = as.integer(T_days),
      `Final day p5 / median / p95` = sprintf(
        "%.1f / %.1f / %.1f", qs[1], qs[2], qs[3])
    ),
    interpretation = sprintf(
      paste0("OU mean-reversion strength theta=%.3f -> shocks ",
             "decay with half-life %.1f days. Long-run daily mean ",
             "mu=%.1f. Stochastic-physics-of-crime style: treats ",
             "incidents as a noisy mean-reverting intensity field ",
             "(Langevin) rather than deterministic trend."),
      theta, half_life, mu),
    n = length(x),
    theta = theta, mu = mu, sigma = sigma,
    n_paths = as.integer(n_paths),
    T_days = as.integer(T_days),
    p5 = as.numeric(qs[1]),
    median = as.numeric(qs[2]),
    p95 = as.numeric(qs[3]),
    paths = paths
  )
}


# ---------------------------------------------------------------------------
# 4. fokker_planck_grid
# ---------------------------------------------------------------------------

#' 1-D Fokker-Planck density evolution under OU drift+diffusion
#'
#' Fits the OU parameters (theta, mu, sigma) on daily counts (same
#' OLS-on-first-differences as \code{morie_tps_langevin_simulate}),
#' then evolves an initial gaussian density centred on the last
#' observation by an explicit advection-diffusion finite-difference
#' scheme with reflective boundaries on a grid spanning
#' [0, 1.5 * max(counts) + 1].
#'
#' @param df A \code{data.frame}.
#' @param ds_name Character label.
#' @param n_grid Grid points (default 64).
#' @param n_steps Time steps (default 200, each of length 0.05 days).
#' @return A \code{morie_rich_result} list with \code{theta},
#'   \code{mu}, \code{sigma}, \code{grid}, \code{density},
#'   \code{stationary_peak}.
#' @export
morie_tps_fokker_planck_grid <- function(df, ds_name = "?",
                                          n_grid = 64L,
                                          n_steps = 200L) {
  stopifnot(is.data.frame(df))
  call_str <- sprintf(
    "morie_tps_fokker_planck_grid(df=<%dr>, n_grid=%d, n_steps=%d)",
    nrow(df), as.integer(n_grid), as.integer(n_steps))

  dt_ser <- .tps_stoch_date_series(df)
  if (length(dt_ser) < 60L) {
    return(.tps_stoch_result(
      sprintf("Fokker-Planck -- %s", ds_name), call_str,
      warnings = sprintf("only %d timestamps", length(dt_ser)),
      interpretation = "No analysis: at least 60 timestamps required."
    ))
  }
  daily <- .tps_stoch_daily(dt_ser)
  x <- as.numeric(daily$counts)
  if (length(x) < 2L) {
    return(.tps_stoch_result(
      sprintf("Fokker-Planck -- %s", ds_name), call_str,
      warnings = sprintf("only %d daily buckets", length(x)),
      interpretation = "No analysis: at least two daily buckets required."
    ))
  }
  dx_obs <- diff(x)
  x_lag <- x[-length(x)]
  fit_ols <- stats::lm(dx_obs ~ x_lag)
  ab <- stats::coef(fit_ols)
  a <- as.numeric(ab[1])
  b <- as.numeric(ab[2])
  theta <- max(1e-6, -b)
  mu <- if (b != 0) -a / b else mean(x)
  resid <- dx_obs - (a + b * x_lag)
  sigma <- stats::sd(resid)
  if (!is.finite(sigma)) sigma <- 0.0

  x_max <- max(x) * 1.5 + 1
  n_grid <- as.integer(n_grid)
  n_steps <- as.integer(n_steps)
  grid <- seq(0, x_max, length.out = n_grid)
  dx <- grid[2L] - grid[1L]
  dt_step <- 0.05  # days per step

  # Initial gaussian centred at last observation.
  p <- exp(-((grid - x[length(x)]) ^ 2) / (2 * (sigma + 1) ^ 2))
  s0 <- sum(p) * dx
  if (s0 > 0) p <- p / s0

  drift <- theta * (mu - grid)
  diff_const <- 0.5 * sigma ^ 2

  # Finite-difference advection-diffusion with periodic shifts that
  # are then re-imposed reflective at the boundaries.
  shift_left <- function(v) c(v[-1L], v[1L])
  shift_right <- function(v) c(v[length(v)], v[-length(v)])

  for (step in seq_len(n_steps)) {
    d2p <- (shift_left(p) - 2 * p + shift_right(p)) / dx ^ 2
    dp <- (shift_left(p) - shift_right(p)) / (2 * dx)
    rhs <- -drift * dp + diff_const * d2p
    p <- p + dt_step * rhs
    p[1L] <- p[2L]
    p[n_grid] <- p[n_grid - 1L]
    p <- pmax(p, 0)
    s <- sum(p) * dx
    if (s > 0) p <- p / s
  }

  stationary_peak <- as.numeric(grid[which.max(p)])
  stationary_var <- if (theta > 0) sigma ^ 2 / (2 * theta) else NA_real_

  .tps_stoch_result(
    sprintf("Fokker-Planck density evolution -- %s", ds_name), call_str,
    summary_lines = list(
      `Grid points` = n_grid,
      `Time steps` = n_steps,
      `Total simulated days` = .tps_stoch_round(n_steps * dt_step, 1),
      `mu (drift target)` = .tps_stoch_round(mu, 2),
      `sigma^2 (diffusion)` = .tps_stoch_round(sigma ^ 2, 2),
      `Stationary peak (grid x)` = .tps_stoch_round(stationary_peak, 2),
      `Empirical mean` = .tps_stoch_round(mean(x), 2),
      `Empirical std` = .tps_stoch_round(stats::sd(x), 2)
    ),
    interpretation = sprintf(
      paste0("Fokker-Planck of OU has stationary density ",
             "N(mu=%.1f, sigma^2/(2*theta)=%.1f). Compare the ",
             "evolved curve to the empirical histogram to assess ",
             "fit; a strong match is the stochastic-physics-of-",
             "crime null model."),
      mu, ifelse(is.na(stationary_var), NaN, stationary_var)),
    n = length(x),
    theta = theta, mu = mu, sigma = sigma,
    grid = grid,
    density = p,
    stationary_peak = stationary_peak,
    stationary_var = stationary_var
  )
}


# ---------------------------------------------------------------------------
# Print method
# ---------------------------------------------------------------------------

#' @export
print.morie_tps_stochastic_result <- function(x, ...) {
  cat(x$title, "\
", strrep("=", nchar(x$title)), "\
", sep = "")
  if (!is.null(x$call) && nzchar(x$call)) {
    cat("Call:", x$call, "\
\
", sep = " ")
  }
  if (length(x$summary_lines) > 0L) {
    nms <- names(x$summary_lines)
    label_w <- max(nchar(nms))
    for (i in seq_along(x$summary_lines)) {
      v <- x$summary_lines[[i]]
      if (is.numeric(v) && length(v) == 1L && is.finite(v)) {
        v <- format(v, digits = 5)
      }
      cat(sprintf("  %-*s  %s\
", label_w, nms[i], format(v)))
    }
    cat("\
")
  }
  if (length(x$warnings) > 0L) {
    for (w in x$warnings) cat("Warning:", w, "\
")
    cat("\
")
  }
  if (nzchar(x$interpretation)) {
    cat(x$interpretation, "\
")
  }
  invisible(x)
}

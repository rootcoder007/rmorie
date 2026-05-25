# SPDX-License-Identifier: AGPL-3.0-or-later
#' Temporal analyses for TPS crime data
#'
#' R port of \code{morie.tps_temporal}. Four jurisdiction-agnostic
#' callables operating on a Toronto Police Service-shaped
#' \code{data.frame}: yearly trend, seasonal cyclic stats,
#' Pettitt-style change-point on yearly counts, and an ARIMA(1,1,1)
#' forecast on monthly counts. All functions return a multi-section
#' \code{morie_rich_result} list so output can be printed directly
#' to a notebook.
#'
#' Functions
#' ---------
#'
#' \itemize{
#'   \item \code{\link{morie_tps_year_over_year_trend}}: OLS slope /
#'     intercept / R-squared on yearly incident counts.
#'   \item \code{\link{morie_tps_seasonal_pattern}}: month / DOW /
#'     hour cyclic counts plus chi-square uniformity tests.
#'   \item \code{\link{morie_tps_changepoint_detection}}: Pettitt's
#'     non-parametric change-point on yearly counts (no external
#'     change-point dependency).
#'   \item \code{\link{morie_tps_arima_forecast}}: ARIMA(1,1,1)
#'     forecast on monthly counts via \code{stats::arima}.
#' }
#'
#' @name morie_tps_temporal
NULL


# ---------------------------------------------------------------------------
# Internal helpers (NOT exported)
# ---------------------------------------------------------------------------

.tps_temporal_result <- function(title, call, summary_lines = list(),
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
  class(out) <- c("morie_tps_temporal_result", "morie_rich_result", "list")
  out
}

.tps_temporal_fmt_round <- function(x, k) {
  if (!is.finite(x)) return(NA_real_)
  round(x, k)
}

# Build monthly counts from an arbitrary date column. Returns a list
# with $dates (POSIXct, first-of-month) and $counts (integer).
.tps_temporal_monthly <- function(df) {
  dt <- NULL
  if (all(c("OCC_YEAR", "OCC_MONTH", "OCC_DAY") %in% names(df))) {
    y <- suppressWarnings(as.integer(df[["OCC_YEAR"]]))
    m <- suppressWarnings(as.integer(df[["OCC_MONTH"]]))
    d <- suppressWarnings(as.integer(df[["OCC_DAY"]]))
    keep <- !is.na(y) & !is.na(m) & !is.na(d)
    if (any(keep)) {
      dt <- suppressWarnings(as.POSIXct(
        sprintf("%04d-%02d-%02d", y[keep], m[keep], d[keep]),
        tz = "UTC"
      ))
      dt <- dt[!is.na(dt)]
    }
  }
  if (is.null(dt) || length(dt) == 0L) {
    for (col in c("OCC_DATE", "REPORT_DATE")) {
      if (col %in% names(df)) {
        dt <- suppressWarnings(as.POSIXct(as.character(df[[col]]),
                                          tz = "UTC"))
        dt <- dt[!is.na(dt)]
        if (length(dt) > 0L) break
      }
    }
  }
  if (is.null(dt) || length(dt) == 0L) {
    return(list(dates = as.POSIXct(character(0)), counts = integer(0)))
  }
  bucket <- format(dt, "%Y-%m")
  tab <- sort(table(bucket))
  ord <- order(names(tab))
  tab <- tab[ord]
  dates <- suppressWarnings(as.POSIXct(paste0(names(tab), "-01"),
                                         tz = "UTC"))
  list(dates = dates, counts = as.integer(tab))
}


# ---------------------------------------------------------------------------
# 1. year_over_year_trend
# ---------------------------------------------------------------------------

#' Year-over-year linear trend on incident counts
#'
#' Aggregates incident counts by year, restricts to the 1990-2030
#' window, fits an OLS line, and reports slope, intercept, and
#' R-squared.
#'
#' @param df A \code{data.frame} with one row per incident.
#' @param year_col Character. Name of the year column
#'   (default \code{"OCC_YEAR"}).
#' @param ds_name Character label for the dataset shown in titles.
#' @return A \code{morie_rich_result} list with \code{slope},
#'   \code{intercept}, \code{r2}, \code{direction}, \code{years},
#'   \code{counts}, \code{fitted}.
#' @export
morie_tps_year_over_year_trend <- function(df,
                                            year_col = "OCC_YEAR",
                                            ds_name = "?") {
  stopifnot(is.data.frame(df), is.character(year_col),
            length(year_col) == 1L)
  call_str <- sprintf("morie_tps_year_over_year_trend(df=<%dr>, year_col=%s)",
                       nrow(df), year_col)
  if (!(year_col %in% names(df))) {
    return(.tps_temporal_result(
      sprintf("YoY trend -- %s", ds_name), call_str,
      warnings = sprintf("%s missing", year_col),
      interpretation = sprintf("No analysis: column '%s' is absent.",
                                year_col)
    ))
  }
  y_raw <- suppressWarnings(as.integer(df[[year_col]]))
  y_raw <- y_raw[!is.na(y_raw) & y_raw >= 1990L & y_raw <= 2030L]
  if (length(y_raw) == 0L) {
    return(.tps_temporal_result(
      sprintf("YoY trend -- %s", ds_name), call_str,
      warnings = "no usable years in [1990, 2030]",
      interpretation = "No analysis: no rows in the valid year window."
    ))
  }
  tab <- table(y_raw)
  years <- as.numeric(names(tab))
  ord <- order(years)
  years <- years[ord]
  counts <- as.numeric(tab[ord])
  if (length(years) < 3L) {
    return(.tps_temporal_result(
      sprintf("YoY trend -- %s", ds_name), call_str,
      warnings = sprintf("only %d usable year(s)", length(years)),
      interpretation = "No analysis: at least three years required."
    ))
  }

  ybar <- mean(years)
  cbar <- mean(counts)
  x_c <- years - ybar
  denom <- sum(x_c * x_c) + 1e-300
  slope <- sum(x_c * (counts - cbar)) / denom
  intercept <- cbar - slope * ybar
  y_hat <- slope * years + intercept
  ss_res <- sum((counts - y_hat) ^ 2)
  ss_tot <- sum((counts - cbar) ^ 2)
  r2 <- if (ss_tot > 0) 1 - ss_res / ss_tot else NA_real_

  direction <- if (slope > 0) "INCREASING"
               else if (slope < 0) "DECREASING"
               else "FLAT"

  interp <- sprintf(
    paste0("Linear fit: count = %.1f * year + %.0f, R^2 = %.3f. ",
           "%s trend over the %d-%d window."),
    slope, intercept, ifelse(is.na(r2), NaN, r2),
    tools::toTitleCase(tolower(direction)),
    as.integer(min(years)), as.integer(max(years))
  )

  .tps_temporal_result(
    sprintf("Year-over-year trend -- %s", ds_name), call_str,
    summary_lines = list(
      `Years` = sprintf("%d-%d", as.integer(min(years)),
                                  as.integer(max(years))),
      `n years` = length(years),
      `Slope (incidents/year)` = .tps_temporal_fmt_round(slope, 2),
      `Intercept` = .tps_temporal_fmt_round(intercept, 2),
      `R-squared` = .tps_temporal_fmt_round(r2, 4),
      `Direction` = direction,
      `Mean count/year` = .tps_temporal_fmt_round(cbar, 1),
      `Min year, count` = sprintf("%d, %d",
                                    as.integer(years[which.min(counts)]),
                                    as.integer(min(counts))),
      `Max year, count` = sprintf("%d, %d",
                                    as.integer(years[which.max(counts)]),
                                    as.integer(max(counts)))
    ),
    interpretation = interp,
    n = length(years),
    years = as.integer(years),
    counts = as.integer(counts),
    fitted = y_hat,
    slope = slope,
    intercept = intercept,
    r2 = r2,
    direction = direction
  )
}


# ---------------------------------------------------------------------------
# 2. seasonal_pattern
# ---------------------------------------------------------------------------

#' Seasonal / cyclic incident-time patterns
#'
#' Counts incidents by month-of-year, day-of-week, and hour-of-day,
#' then runs a chi-square goodness-of-fit test against a uniform
#' distribution on each cycle.
#'
#' @param df A \code{data.frame}.
#' @param ds_name Character label.
#' @return A \code{morie_rich_result} list with per-cycle counts and
#'   chi-square p-values.
#' @export
morie_tps_seasonal_pattern <- function(df, ds_name = "?") {
  stopifnot(is.data.frame(df))
  call_str <- sprintf("morie_tps_seasonal_pattern(df=<%dr>)", nrow(df))

  cycle <- function(col, label) {
    if (!(col %in% names(df))) return(NULL)
    s <- df[[col]]
    s <- s[!is.na(s)]
    if (length(s) == 0L) return(NULL)
    tab <- sort(table(s))
    ord <- order(names(tab))
    tab <- tab[ord]
    obs <- as.numeric(tab)
    exp <- rep(sum(obs) / length(obs), length(obs))
    chi2 <- NA_real_
    pval <- NA_real_
    if (length(obs) >= 2L && all(exp > 0)) {
      ct <- tryCatch(
        suppressWarnings(stats::chisq.test(obs, p = exp / sum(exp))),
        error = function(e) NULL
      )
      if (!is.null(ct)) {
        chi2 <- as.numeric(ct$statistic)
        pval <- as.numeric(ct$p.value)
      }
    }
    list(label = label, buckets = names(tab),
         counts = as.integer(obs), chi2 = chi2, p = pval)
  }

  month <- cycle("OCC_MONTH", "Month of occurrence (1=Jan)")
  dow   <- cycle("OCC_DOW",   "Day of week of occurrence")
  hour  <- cycle("OCC_HOUR",  "Hour of day of occurrence")

  summary <- list(
    `Dataset` = ds_name,
    `Incidents` = nrow(df)
  )
  any_cycle <- FALSE
  for (entry in list(month, dow, hour)) {
    if (!is.null(entry)) {
      any_cycle <- TRUE
      summary[[paste(entry$label,
                      "chi-sq uniformity p", sep = " ")]] <-
        .tps_temporal_fmt_round(entry$p, 6)
    }
  }

  interp <- paste(
    "p < 0.05 in any cycle indicates incident times are NOT uniformly",
    "distributed over that cycle (e.g. weekday vs weekend,",
    "evening vs morning).")

  .tps_temporal_result(
    sprintf("Seasonal / cyclic patterns -- %s", ds_name), call_str,
    summary_lines = summary,
    warnings = if (!any_cycle) "no OCC_MONTH / OCC_DOW / OCC_HOUR found" else character(0),
    interpretation = interp,
    n = nrow(df),
    month = month,
    dow = dow,
    hour = hour
  )
}


# ---------------------------------------------------------------------------
# 3. changepoint_detection (Pettitt)
# ---------------------------------------------------------------------------

#' Pettitt-style change-point on yearly incident counts
#'
#' Implements Pettitt's non-parametric change-point statistic
#' U_t = sum_i sum_j sign(x_i - x_j) for i <= t < j, then reports
#' the year maximising |U_t| and an approximate p-value. No external
#' change-point dependency is required.
#'
#' @param df A \code{data.frame} with one row per incident.
#' @param year_col Year column name.
#' @param ds_name Character label.
#' @return A \code{morie_rich_result} list with
#'   \code{changepoint_year}, \code{K_statistic}, \code{p_value},
#'   \code{pre_mean}, \code{post_mean}.
#' @export
morie_tps_changepoint_detection <- function(df,
                                             year_col = "OCC_YEAR",
                                             ds_name = "?") {
  stopifnot(is.data.frame(df), is.character(year_col))
  call_str <- sprintf("morie_tps_changepoint_detection(df=<%dr>, year_col=%s)",
                       nrow(df), year_col)
  if (!(year_col %in% names(df))) {
    return(.tps_temporal_result(
      sprintf("Change-point -- %s", ds_name), call_str,
      warnings = sprintf("%s missing", year_col),
      interpretation = sprintf("No analysis: column '%s' is absent.",
                                year_col)
    ))
  }
  y_raw <- suppressWarnings(as.integer(df[[year_col]]))
  y_raw <- y_raw[!is.na(y_raw) & y_raw >= 1990L & y_raw <= 2030L]
  if (length(y_raw) == 0L) {
    return(.tps_temporal_result(
      sprintf("Change-point -- %s", ds_name), call_str,
      warnings = "no usable years in [1990, 2030]",
      interpretation = "No analysis: no rows in the valid year window."
    ))
  }
  tab <- table(y_raw)
  years <- as.numeric(names(tab))
  ord <- order(years)
  years <- years[ord]
  x <- as.numeric(tab[ord])
  n <- length(x)
  if (n < 6L) {
    return(.tps_temporal_result(
      sprintf("Change-point -- %s", ds_name), call_str,
      warnings = sprintf("need >=6 years, got %d", n),
      interpretation = "No analysis: at least six years are required."
    ))
  }

  # Pettitt statistic U_t (vectorised)
  U <- numeric(n)
  for (t in seq_len(n)) {
    if (t < n) {
      left <- x[seq_len(t)]
      right <- x[(t + 1L):n]
      U[t] <- sum(outer(left, right, FUN = function(a, b) sign(a - b)))
    } else {
      U[t] <- 0
    }
  }
  K <- which.max(abs(U))
  K_stat <- abs(U[K])
  p <- 2 * exp(-6 * K_stat ^ 2 / (n ^ 3 + n ^ 2))
  p <- min(1.0, p)
  bp_year <- as.integer(years[K])
  pre <- mean(x[seq_len(K)])
  post <- if (n > K) mean(x[(K + 1L):n]) else NA_real_

  delta_label <- if (is.na(post)) "n/a" else
                  format(round(post - pre, 1), nsmall = 1)

  interp <- sprintf(
    paste0("Estimated structural break in %d: pre-mean %.1f, ",
           "post-mean %s. p=%.4f -- %s at alpha=0.05."),
    bp_year, pre,
    ifelse(is.na(post), "n/a", sprintf("%.1f", post)),
    p,
    ifelse(p < 0.05, "significant", "not significant")
  )

  .tps_temporal_result(
    sprintf("Change-point (Pettitt) -- %s", ds_name), call_str,
    summary_lines = list(
      `Years` = sprintf("%d-%d",
                          as.integer(min(years)),
                          as.integer(max(years))),
      `n years` = n,
      `Estimated change-point year` = bp_year,
      `Pettitt K statistic` = .tps_temporal_fmt_round(K_stat, 2),
      `Approx p-value` = .tps_temporal_fmt_round(p, 6),
      `Mean count BEFORE change-point` = .tps_temporal_fmt_round(pre, 1),
      `Mean count AFTER change-point` = if (is.na(post)) "n/a" else
        .tps_temporal_fmt_round(post, 1),
      `Delta mean` = delta_label
    ),
    interpretation = interp,
    n = n,
    years = as.integer(years),
    counts = as.integer(x),
    changepoint_year = bp_year,
    K_statistic = K_stat,
    p_value = p,
    pre_mean = pre,
    post_mean = post
  )
}


# ---------------------------------------------------------------------------
# 4. arima_forecast
# ---------------------------------------------------------------------------

#' ARIMA(1,1,1) monthly-count forecast
#'
#' Builds a monthly count series via \code{stats::ts}, fits
#' ARIMA(1,1,1) with \code{stats::arima}, and forecasts \code{h}
#' periods ahead with \code{stats::predict}. AIC is reported from
#' the fit; BIC is computed manually as
#' \code{AIC + k * (log(n) - 2)}.
#'
#' @param df A \code{data.frame}.
#' @param h Forecast horizon in months.
#' @param ds_name Character label.
#' @return A \code{morie_rich_result} list with \code{forecast},
#'   \code{aic}, \code{bic}, \code{n_train}.
#' @export
morie_tps_arima_forecast <- function(df, h = 12L, ds_name = "?") {
  stopifnot(is.data.frame(df))
  call_str <- sprintf("morie_tps_arima_forecast(df=<%dr>, h=%d)",
                       nrow(df), as.integer(h))
  monthly <- .tps_temporal_monthly(df)
  if (length(monthly$counts) < 24L) {
    return(.tps_temporal_result(
      sprintf("ARIMA -- %s", ds_name), call_str,
      warnings = sprintf("need >=24 months, got %d",
                           length(monthly$counts)),
      interpretation = "No analysis: at least 24 months required."
    ))
  }

  first <- as.POSIXlt(monthly$dates[1])
  start_year <- first$year + 1900L
  start_month <- first$mon + 1L
  ts_obj <- stats::ts(monthly$counts,
                       start = c(start_year, start_month),
                       frequency = 12L)

  fit <- tryCatch(
    stats::arima(ts_obj, order = c(1L, 1L, 1L)),
    error = function(e) e
  )
  if (inherits(fit, "error")) {
    return(.tps_temporal_result(
      sprintf("ARIMA -- %s", ds_name), call_str,
      warnings = sprintf("fit failed: %s", conditionMessage(fit)),
      interpretation = "No analysis: ARIMA fit failed."
    ))
  }
  pred <- tryCatch(stats::predict(fit, n.ahead = as.integer(h)),
                    error = function(e) e)
  if (inherits(pred, "error")) {
    return(.tps_temporal_result(
      sprintf("ARIMA -- %s", ds_name), call_str,
      warnings = sprintf("predict failed: %s",
                           conditionMessage(pred)),
      interpretation = "No analysis: ARIMA forecast failed."
    ))
  }

  fc <- as.numeric(pred$pred)
  se <- as.numeric(pred$se)
  aic <- as.numeric(stats::AIC(fit))
  # BIC = AIC + k * (log(n) - 2), where k = number of estimated params
  k <- length(stats::coef(fit)) +
       (if ("sigma2" %in% names(fit)) 1L else 0L)
  n_eff <- length(monthly$counts)
  bic <- aic + k * (log(n_eff) - 2)

  last_month_str <- format(monthly$dates[length(monthly$dates)],
                            "%Y-%m")

  .tps_temporal_result(
    sprintf("ARIMA(1,1,1) %d-month forecast -- %s",
              as.integer(h), ds_name), call_str,
    summary_lines = list(
      `Training months` = length(monthly$counts),
      `Last training month` = last_month_str,
      `AIC` = .tps_temporal_fmt_round(aic, 1),
      `BIC` = .tps_temporal_fmt_round(bic, 1),
      `Forecast horizon (h)` = as.integer(h),
      `Forecast mean` = .tps_temporal_fmt_round(mean(fc), 1)
    ),
    interpretation = sprintf(
      paste0("ARIMA(1,1,1) trained on %d months through %s; ",
             "mean forecast over the next %d month(s) is %.1f ",
             "(AIC=%.1f, BIC=%.1f)."),
      length(monthly$counts), last_month_str,
      as.integer(h), mean(fc), aic, bic),
    n = length(monthly$counts),
    forecast = fc,
    forecast_se = se,
    aic = aic,
    bic = bic,
    n_train = length(monthly$counts)
  )
}


# ---------------------------------------------------------------------------
# Print method (shared with tps_stochastic)
# ---------------------------------------------------------------------------

#' @export
print.morie_tps_temporal_result <- function(x, ...) {
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

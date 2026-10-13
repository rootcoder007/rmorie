# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Research P16: court backlog, Little's law on a finite docket (research/lean/P16Backlog.lean; Little 1961).
#
#   Research.P16.occupancy_integral   integral_0^T N(t) dt = sum_i (d_i - a_i)
#   Research.P16.little               L = lambda * W (time-average pending = filing rate x mean disposition time)
#   Research.P16.little_backlog       a reported mean disposition time and filing rate determine the backlog
#   Research.P16.little_target        a backlog target fixes the mean disposition time that achieves it

#' Court backlog arithmetic: Little's law on a docket
#'
#' For cases with filing times \code{arrivals} and disposition times
#' \code{dispositions} inside a window \eqn{[0, T]}, the time-average number of
#' pending cases equals the filing rate times the mean time to disposition,
#' with no probability model at all: the integral of the pending count is the
#' sum of the case durations (\code{Research.P16.occupancy_integral},
#' \code{little}). A court that publishes a mean disposition time and a filing
#' rate has therefore published its average backlog (\code{little_backlog}),
#' and a backlog target fixes the mean disposition time that achieves it
#' (\code{little_target}). Cases still pending at \eqn{T} make the mean of
#' disposed cases a truncated estimate; the function reports how many are
#' censored so the reader sees the dark-figure problem that follows.
#' @param arrivals Filing times (non-negative).
#' @param dispositions Disposition times, at least the filing times; \code{NA}
#'   for cases still pending at \code{horizon}.
#' @param horizon Window end \eqn{T}; by default the latest disposition.
#' @param target_backlog Optional backlog target \eqn{L^*} for which the
#'   required mean disposition time is reported.
#' @return A list with \code{n}, \code{n_censored}, \code{horizon},
#'   \code{filing_rate}, \code{mean_disposition_time} (over disposed cases),
#'   \code{average_backlog} (\eqn{\lambda \bar W}), \code{occupancy_integral},
#'   \code{pending_at_horizon}, \code{required_mean_time} (when a target is
#'   given) and \code{theorems}.
#' @examples
#' a <- c(0, 1, 2, 4, 5, 7); d <- c(3, 2.5, 6, 5, 9, 10)
#' morie_court_backlog(a, d, horizon = 10, target_backlog = 1)
#' @export
morie_court_backlog <- function(arrivals, dispositions, horizon = NULL, target_backlog = NULL) {
  a <- as.numeric(arrivals)
  d <- as.numeric(dispositions)
  n <- length(a)
  if (length(d) != n || n == 0L) stop("arrivals and dispositions must have the same positive length", call. = FALSE)
  if (anyNA(a) || any(a < 0)) stop("arrivals must be non-negative", call. = FALSE)
  if (is.null(horizon)) horizon <- max(d, na.rm = TRUE)
  if (!is.numeric(horizon) || length(horizon) != 1L || horizon <= 0) stop("horizon must be a single positive number", call. = FALSE)
  if (any(a > horizon)) stop("every arrival must lie inside the horizon", call. = FALSE)
  censored <- is.na(d) | d > horizon
  if (any(!censored & d < a)) stop("dispositions cannot precede arrivals", call. = FALSE)
  d_obs <- d[!censored]
  a_obs <- a[!censored]
  occupancy <- sum(d_obs - a_obs) + sum(horizon - a[censored])   # censored cases are pending to the horizon
  rate <- n / horizon
  mean_wait <- if (length(d_obs)) mean(d_obs - a_obs) else NA_real_
  out <- list(n = n, n_censored = sum(censored), horizon = horizon, filing_rate = rate,
              mean_disposition_time = mean_wait,
              average_backlog = occupancy / horizon, occupancy_integral = occupancy,
              pending_at_horizon = sum(censored),
              little_identity_check = if (!any(censored)) occupancy / horizon - rate * mean_wait else NA_real_)
  if (!is.null(target_backlog)) {
    if (target_backlog < 0) stop("target_backlog must be non-negative", call. = FALSE)
    out$required_mean_time <- target_backlog / rate
  }
  out$theorems <- c("Research.P16.occupancy_integral", "Research.P16.little", "Research.P16.little_backlog",
                    "Research.P16.little_target")
  out
}

#   Research.P16Censoring.true_mean_ge         true mean >= (sum t + sum a)/(n + m)
#   Research.P16Censoring.lower_bound_sub      L - tbar = m/(n+m) (abar - tbar)
#   Research.P16Censoring.bias_lower           true mean - tbar >= m/(n+m) (abar - tbar)
#   Research.P16Censoring.disposed_understates  abar >= tbar  =>  tbar <= true mean
#   Research.P16Censoring.no_upper_bound       no upper bound without a cap on pending durations

#' The disposed-cases mean as a bound: what the pending cases imply
#'
#' A cohort of \eqn{n} disposed cases with durations \eqn{t_i} and \eqn{m}
#' pending cases with elapsed ages \eqn{a_j}. The published disposed-cases
#' mean \eqn{\bar t} is compared with the cohort's true mean, whose pending
#' durations are unknown but at least the ages. The true mean is at least
#' \eqn{L = (\sum t + \sum a)/(n + m)} (\code{Research.P16Censoring.true_mean_ge}),
#' and \eqn{L - \bar t = \frac{m}{n+m}(\bar a - \bar t)}
#' (\code{lower_bound_sub}), so the published mean understates the truth for
#' certain whenever the pending cases are on average older than it
#' (\code{disposed_understates}) by at least that amount (\code{bias_lower}).
#' No upper bound exists without a cap on the pending durations
#' (\code{no_upper_bound}): this is the P1 dark-figure structure again.
#' @param disposed Durations of the disposed cases (non-negative).
#' @param pending_ages Elapsed ages of the pending cases (non-negative).
#' @return A list with \code{n}, \code{m}, \code{disposed_mean},
#'   \code{pending_age} (mean age of the pending cases), \code{lower_bound}
#'   (\eqn{L}), \code{bias_lower} (\eqn{\frac{m}{n+m}(\bar a - \bar t)}),
#'   \code{understates} (\eqn{\bar t \le \bar a}), \code{upper_bound}
#'   (\code{Inf}) and \code{theorems}.
#' @examples
#' b <- morie_backlog_censoring(disposed = c(30, 45, 60, 90, 120), pending_ages = c(100, 150, 200))
#' c(disposed = b$disposed_mean, lower = b$lower_bound, bias = b$bias_lower, understates = b$understates)
#' @export
morie_backlog_censoring <- function(disposed, pending_ages) {
  if (!is.numeric(disposed) || !is.numeric(pending_ages)) stop("disposed and pending_ages must be numeric", call. = FALSE)
  if (anyNA(disposed) || anyNA(pending_ages)) stop("no missing values allowed", call. = FALSE)
  if (any(disposed < 0) || any(pending_ages < 0)) stop("durations and ages must be non-negative", call. = FALSE)
  n <- length(disposed)
  m <- length(pending_ages)
  if (n < 1L || m < 1L) stop("need at least one disposed and one pending case", call. = FALSE)
  tbar <- sum(disposed) / n
  abar <- sum(pending_ages) / m
  L <- (sum(disposed) + sum(pending_ages)) / (n + m)
  list(
    n = n, m = m, disposed_mean = tbar, pending_age = abar, lower_bound = L,
    bias_lower = (m / (n + m)) * (abar - tbar),
    understates = tbar <= abar,
    upper_bound = Inf,
    theorems = c("Research.P16Censoring.true_mean_ge", "Research.P16Censoring.lower_bound_sub",
                 "Research.P16Censoring.bias_lower", "Research.P16Censoring.disposed_understates",
                 "Research.P16Censoring.no_upper_bound")
  )
}

#   Research.P16.little_target                 a backlog fixes the mean time: W = L / lambda
#   Research.P16Censoring.band_floor_le_mean   the band floors' share-weighted sum bounds the mean below

#' Published court statistics read through Little's law
#'
#' Courts publish cases received, cases pending and a mean time to disposition over disposed
#' cases. Over a window of \code{days} days with \code{received} filings, the time-average
#' number pending \eqn{L} and the filing rate \eqn{\lambda} fix the mean time a case spends in
#' court, \eqn{W = L/\lambda} (\code{Research.P16.little}, read backwards as
#' \code{little_target}). The published mean counts only the cases disposed, so it is a
#' truncated mean that understates the truth when the pending cases are older
#' (\code{Research.P16Censoring.disposed_understates}); \code{ratio} is how far. With the
#' published shares of disposed cases by duration band and the bands' lower edges, the
#' disposed mean is at least the share-weighted sum of the floors
#' (\code{Research.P16Censoring.band_floor_le_mean}); an open top band leaves no upper bound
#' (\code{no_upper_bound}).
#' @param received Cases received in each window.
#' @param days Length of each window in days.
#' @param pending Time-average number of cases pending over each window (the mean of the
#'   published month-end counts, for a monthly series).
#' @param mean_days Published mean days to disposition of the cases disposed in each window.
#' @param band_shares Optional matrix (one row per window, one column per band) of the shares of
#'   disposed cases in each duration band.
#' @param band_floors Lower edge of each band in days (the first is usually 0).
#' @return A data frame with \code{filing_rate} (per day), \code{implied_mean_days}
#'   (\eqn{L/\lambda}), \code{published_mean_days}, \code{ratio} (implied over published),
#'   \code{little_backlog} (\eqn{\lambda} times the published mean, the backlog the published
#'   mean would imply), \code{pending}, \code{band_floor_bound} (when bands are given) and the
#'   \code{theorems} attribute.
#' @examples
#' # Ontario Court of Justice, criminal, April 2024: 21,265 received in 30 days, 148,904
#' # pending, 175 published mean days; disposed shares under 10, 10-15, 15-18, over 18 months
#' morie_backlog_published(21265, 30, 148904, 175,
#'                         band_shares = rbind(c(0.808, 0.105, 0.034, 0.053)),
#'                         band_floors = c(0, 304, 456, 548))
#' @export
morie_backlog_published <- function(received, days, pending, mean_days, band_shares = NULL, band_floors = NULL) {
  n <- max(length(received), length(days), length(pending), length(mean_days))
  chk <- function(x, nm, pos = TRUE) {
    if (!is.numeric(x) || anyNA(x) || any(!is.finite(x))) stop(sprintf("%s must be finite numbers", nm), call. = FALSE)
    if (pos && any(x <= 0)) stop(sprintf("%s must be positive", nm), call. = FALSE)
    if (!pos && any(x < 0)) stop(sprintf("%s must be non-negative", nm), call. = FALSE)
    if (!length(x) %in% c(1L, n)) stop(sprintf("%s must have length 1 or %d", nm, n), call. = FALSE)
    rep_len(as.numeric(x), n)
  }
  received <- chk(received, "received")
  days <- chk(days, "days")
  pending <- chk(pending, "pending", pos = FALSE)
  mean_days <- chk(mean_days, "mean_days")
  rate <- received / days
  out <- data.frame(filing_rate = rate, implied_mean_days = pending / rate, published_mean_days = mean_days,
                    ratio = (pending / rate) / mean_days, little_backlog = rate * mean_days, pending = pending)
  thm <- c("Research.P16.little", "Research.P16.little_backlog", "Research.P16.little_target",
           "Research.P16Censoring.disposed_understates", "Research.P16Censoring.no_upper_bound")
  if (!is.null(band_shares)) {
    s <- as.matrix(band_shares)
    if (nrow(s) == 1L && n > 1L) s <- s[rep(1L, n), , drop = FALSE]
    if (nrow(s) != n) stop(sprintf("band_shares must have one row per window (%d)", n), call. = FALSE)
    if (is.null(band_floors) || length(band_floors) != ncol(s)) {
      stop("band_floors must give one lower edge per band", call. = FALSE)
    }
    if (anyNA(s) || any(s < 0)) stop("band_shares must be non-negative", call. = FALSE)
    if (any(abs(rowSums(s) - 1) > 0.01)) stop("each row of band_shares must sum to 1 (within rounding)", call. = FALSE)
    if (!is.numeric(band_floors) || anyNA(band_floors) || any(band_floors < 0) || is.unsorted(band_floors)) {
      stop("band_floors must be non-negative and increasing", call. = FALSE)
    }
    out$band_floor_bound <- as.numeric(s %*% band_floors) / rowSums(s)
    thm <- c(thm, "Research.P16Censoring.band_floor_le_mean")
  }
  attr(out, "theorems") <- thm
  out
}

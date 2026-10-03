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
  a <- as.numeric(arrivals); d <- as.numeric(dispositions)
  n <- length(a)
  if (length(d) != n || n == 0L) stop("arrivals and dispositions must have the same positive length", call. = FALSE)
  if (anyNA(a) || any(a < 0)) stop("arrivals must be non-negative", call. = FALSE)
  if (is.null(horizon)) horizon <- max(d, na.rm = TRUE)
  if (!is.numeric(horizon) || length(horizon) != 1L || horizon <= 0) stop("horizon must be a single positive number", call. = FALSE)
  if (any(a > horizon)) stop("every arrival must lie inside the horizon", call. = FALSE)
  censored <- is.na(d) | d > horizon
  if (any(!censored & d < a)) stop("dispositions cannot precede arrivals", call. = FALSE)
  d_obs <- d[!censored]; a_obs <- a[!censored]
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

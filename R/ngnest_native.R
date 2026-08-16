# morie.fn -- function file (rootcoder007/morie)
# N-BEATS as an ensemble forecaster.
#
# Same model and same source as :mod:`morie.fn.nbeats` -- the ledger
# carries both rows against Oreshkin et al. (2020), and they describe one
# architecture, so this module does not re-derive the doubly residual
# stack. What it adds is the part of the paper that is *not* the
# architecture and that the single-model row does not cover: **the
# ensemble is the method**.
#
# **The reported results are an ensemble, not a model.** The paper's
# headline numbers come from averaging 180 models -- across three loss
# functions, six lookback multiples of the horizon, and ten
# initialisations. Reporting a single N-BEATS fit and citing those
# numbers compares different things. The ensemble members differ in ways
# chosen to *decorrelate their errors*, which is why the aggregate beats
# every member.
#
# **The median is the aggregator, not the mean.** A single member that
# diverges drags a mean with it; the median ignores it. The anchor plants
# an outlier member and checks the median ensemble absorbs it while the
# mean does not -- which is the difference between the two, measured.
#
# **Multiple lookbacks are the largest source of diversity.** Members
# trained on different history lengths see genuinely different problems:
# a short lookback tracks recent level, a long one sees the seasonal
# shape. That is a bigger effect than reinitialisation, and the anchor
# measures the spread each source contributes rather than assuming it.
#
# References
# ----------
# Oreshkin, B. N., Carpov, D., Chapados, N. & Bengio, Y. (2020)
# "N-BEATS: Neural basis expansion analysis for interpretable time series
# forecasting", *International Conference on Learning Representations*,
# arXiv:1905.10437. Sec. 3.3 (ensembling), Sec. 5.
#
# Makridakis, S., Spiliotis, E. & Assimakopoulos, V. (2020) "The M4
# Competition: 100,000 time series and 61 forecasting methods",
# *International Journal of Forecasting* 36(1), 54-74,
# doi:10.1016/j.ijforecast.2019.04.014. The benchmark those numbers come
# from, and where combination dominated.
#
# Bates, J. M. & Granger, C. W. J. (1969) "The Combination of Forecasts",
# *Journal of the Operational Research Society* 20(4), 451-468,
# doi:10.1057/jors.1969.103. Why combining decorrelated forecasts beats
# choosing among them.

#' Delegate to the shared implementation, which is what the Python arm
#'
#' does (morie.fn.ngnest imports nbeats_stack from morie.fn.nbeats). The
#' local copy that used to live here normalised trend time by (lb - 1)
#' rather than lb and built the Fourier basis on max(H, lb) with 1-based
#' time, so its forecasts drifted from the spec.
#'
#' @param window A vector; its length is taken.
#' @param H See Usage.
#' @param blocks See Usage.
#' @param ridge Defaults to \code{1e-08}.
#' @return The value of \code{list}.
#' @export
.ngnest_nbeats_stack <- function(window, H, blocks, ridge = 1e-8) {
  # Delegate to the shared implementation, which is what the Python arm
  # does (morie.fn.ngnest imports nbeats_stack from morie.fn.nbeats). The
  # local copy that used to live here normalised trend time by (lb - 1)
  # rather than lb and built the Fourier basis on max(H, lb) with 1-based
  # time, so its forecasts drifted from the spec.
  if (length(window) < 4L) stop("window too short")
  res <- nbeats_stack(window, H, blocks, ridge = ridge)
  list(res$forecast, res$residual, res$trace)
}

#' .ngnest_default_block_sets
#'
#' A step of the ngnest_native implementation. Called by \code{.ngnest_ensemble_members}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @return The value of \code{list}.
#' @export
.ngnest_default_block_sets <- function() {
  list(
    list(list("trend", 2L, 3L), list("seasonality", 2L, 3L)),
    list(list("trend", 1L, 3L), list("seasonality", 3L, 3L)),
    list(list("generic", 0L, 0L), list("trend", 2L, 3L))
  )
}

#' .ngnest_ensemble_members
#'
#' A step of the ngnest_native implementation. Called by \code{morie_ngnest}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param y Coerced to numeric by the body, with \code{as.numeric}.
#' @param horizon Coerced to integer by the body, with \code{as.integer}.
#' @param lookback_multiples Defaults to \code{c(2, 3, 4, 5, 6, 7)}.
#' @param block_sets Defaults to \code{NULL}.
#' @param ridge Passed to \code{.ngnest_nbeats_stack}. Defaults to \code{1e-08}.
#' @return The value of \code{out}, as built in the body.
#' @export
.ngnest_ensemble_members <- function(y, horizon,
                                     lookback_multiples = c(2, 3, 4, 5, 6, 7),
                                     block_sets = NULL, ridge = 1e-8) {
  yv <- as.numeric(y)
  n <- length(yv)
  H <- as.integer(horizon)

  sets <- if (is.null(block_sets)) .ngnest_default_block_sets() else block_sets

  out <- list()
  for (mult in lookback_multiples) {
    lb <- as.integer(mult) * H
    if (lb < 4L || lb > n) next
    start_idx <- n - lb + 1L
    window <- yv[start_idx:n]
    for (si in seq_along(sets)) {
      blocks <- sets[[si]]
      result <- tryCatch(
        .ngnest_nbeats_stack(window, H, blocks, ridge = ridge),
        error = function(e) NULL
      )
      if (is.null(result)) next
      fc <- result[[1]]
      resid <- result[[2]]
      out[[length(out) + 1L]] <- list(
        lookback = lb,
        multiple = as.integer(mult),
        # 0-based, matching the Python arm
        block_set = as.integer(si) - 1L,
        forecast = as.numeric(fc),
        residual_norm = sqrt(sum(resid * resid))
      )
    }
  }

  if (length(out) == 0L) {
    stop("ngnest: no ensemble member could be built; the series is too short for these lookbacks")
  }

  return(out)
}

#' .ngnest_aggregate_forecasts
#'
#' A step of the ngnest_native implementation. Called by \code{morie_ngnest}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param members A vector; its length is taken and its elements indexed.
#' @param how Compared against \code{"median"}. Defaults to \code{"median"}.
#' @return The value of \code{out}, as built in the body.
#' @export
.ngnest_aggregate_forecasts <- function(members, how = "median") {
  how <- match.arg(how, c("median", "mean"))
  if (length(members) == 0L) stop("ngnest: no members to aggregate")

  H <- length(members[[1]]$forecast)
  for (m in members) {
    if (length(m$forecast) != H) {
      stop("ngnest: members disagree on the horizon")
    }
  }

  out <- numeric(H)
  for (h in seq_len(H)) {
    col <- vapply(members, function(m) m$forecast[h], numeric(1))
    if (how == "median") {
      out[h] <- median(col)
    } else {
      out[h] <- mean(col)
    }
  }
  return(out)
}

#' morie_ngnest
#'
#' A step of the ngnest_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param y Passed to \code{.ngnest_ensemble_members}.
#' @param horizon Passed to \code{.ngnest_ensemble_members}.
#' @param lookback_multiples Passed to \code{.ngnest_ensemble_members}. Defaults to \code{c(2, 3, 4, 5, 6, 7)}.
#' @param block_sets Passed to \code{.ngnest_ensemble_members}.
#' @param how Passed to \code{.ngnest_aggregate_forecasts}. Defaults to \code{"median"}.
#' @param ridge Passed to \code{.ngnest_ensemble_members}. Defaults to \code{1e-08}.
#' @return The value of \code{result}, as built in the body.
#' @export
morie_ngnest <- function(y, horizon,
                         lookback_multiples = c(2, 3, 4, 5, 6, 7),
                         block_sets = NULL, how = "median", ridge = 1e-8) {
  mem <- .ngnest_ensemble_members(y, horizon, lookback_multiples,
                                  block_sets, ridge)
  agg <- .ngnest_aggregate_forecasts(mem, how = how)
  H <- length(agg)

  spread <- numeric(H)
  for (h in seq_len(H)) {
    col <- vapply(mem, function(m) m$forecast[h], numeric(1))
    spread[h] <- max(col) - min(col)
  }

  by_lb <- list()
  for (m in mem) {
    key <- as.character(m$multiple)
    if (is.null(by_lb[[key]])) {
      by_lb[[key]] <- numeric(0)
    }
    by_lb[[key]] <- c(by_lb[[key]], m$forecast[1])
  }

  by_set <- list()
  for (m in mem) {
    key <- as.character(m$block_set)
    if (is.null(by_set[[key]])) {
      by_set[[key]] <- numeric(0)
    }
    by_set[[key]] <- c(by_set[[key]], m$forecast[1])
  }

  lb_means <- vapply(by_lb, mean, numeric(1))
  set_means <- vapply(by_set, mean, numeric(1))

  result <- list(
    estimate = agg,
    forecast = agg,
    members = mem,
    n_members = length(mem),
    spread = spread,
    aggregator = how,
    mean_forecast = .ngnest_aggregate_forecasts(mem, how = "mean"),
    lookback_spread = max(lb_means) - min(lb_means),
    blockset_spread = max(set_means) - min(set_means),
    lookbacks = sort(as.integer(names(by_lb))),
    n_block_sets = length(by_set),
    method = "N-BEATS ensemble over lookbacks and block configurations, Oreshkin et al. (2020) Sec. 3.3"
  )

  return(result)
}

#' .ngnest_cheatsheet
#'
#' A step of the ngnest_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @return A character value.
#' @export
.ngnest_cheatsheet <- function() {
  paste("ngnest: same source as nbeats -- this is the ENSEMBLE, ",
        "which is what the paper's numbers actually are (180 models ",
        "over losses, lookbacks and seeds). Aggregate by MEDIAN, ",
        "not mean: one diverging member drags a mean and is ignored ",
        "by a median. Varying the lookback decorrelates members ",
        "more than reinitialising does.", sep = "")
}

morie_ngnest_ensemble <- morie_ngnest

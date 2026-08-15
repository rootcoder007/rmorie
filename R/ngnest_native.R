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

.ngnest_nbeats_stack <- function(window, H, blocks, ridge = 1e-8) {
  lb <- length(window)
  if (lb < 4L) stop("window too short")

  current_input <- as.numeric(window)
  fc_sum <- rep(0, H)

  for (block in blocks) {
    btype <- block[[1]]
    p1 <- as.integer(block[[2]])
    p2 <- as.integer(block[[3]])

    if (btype == "trend") {
      deg <- max(p1, 1L)
      idx_lb <- seq_len(lb)
      idx_fc <- lb + seq_len(H)
      denom <- max(lb - 1L, 1L)
      t_lb <- (idx_lb - 1L) / denom
      t_fc <- (idx_fc - 1L) / denom

      B_lb <- sapply(0:deg, function(d) t_lb^d)
      B_fc <- sapply(0:deg, function(d) t_fc^d)

      BtB <- crossprod(B_lb)
      reg <- ridge * diag(ncol(B_lb))
      theta <- tryCatch(
        solve(BtB + reg, crossprod(B_lb, current_input)),
        error = function(e) rep(0, ncol(B_lb))
      )

      backcast <- as.vector(B_lb %*% theta)
      fc_block <- as.vector(B_fc %*% theta)
    } else if (btype == "seasonality") {
      n_harm <- max(p1, 1L)
      period <- max(H, lb)

      idx_lb <- seq_len(lb)
      idx_fc <- lb + seq_len(H)

      basis_lb <- matrix(0, lb, 2L * n_harm)
      basis_fc <- matrix(0, H, 2L * n_harm)
      for (k in seq_len(n_harm)) {
        basis_lb[, 2L * k - 1L] <- cos(2 * pi * k * idx_lb / period)
        basis_lb[, 2L * k]      <- sin(2 * pi * k * idx_lb / period)
        basis_fc[, 2L * k - 1L] <- cos(2 * pi * k * idx_fc / period)
        basis_fc[, 2L * k]      <- sin(2 * pi * k * idx_fc / period)
      }

      BtB <- crossprod(basis_lb)
      reg <- ridge * diag(ncol(basis_lb))
      theta <- tryCatch(
        solve(BtB + reg, crossprod(basis_lb, current_input)),
        error = function(e) rep(0, ncol(basis_lb))
      )

      backcast <- as.vector(basis_lb %*% theta)
      fc_block <- as.vector(basis_fc %*% theta)
    } else {
      last_val <- current_input[lb]
      fc_block <- rep(last_val, H)
      backcast <- rep(mean(current_input), lb)
    }

    fc_sum <- fc_sum + fc_block
    current_input <- current_input - backcast
  }

  residual <- current_input
  return(list(fc_sum, residual, NULL))
}

.ngnest_default_block_sets <- function() {
  list(
    list(list("trend", 2L, 3L), list("seasonality", 2L, 3L)),
    list(list("trend", 1L, 3L), list("seasonality", 3L, 3L)),
    list(list("generic", 0L, 0L), list("trend", 2L, 3L))
  )
}

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
        block_set = as.integer(si),
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

.ngnest_cheatsheet <- function() {
  paste("ngnest: same source as nbeats -- this is the ENSEMBLE, ",
        "which is what the paper's numbers actually are (180 models ",
        "over losses, lookbacks and seeds). Aggregate by MEDIAN, ",
        "not mean: one diverging member drags a mean and is ignored ",
        "by a median. Varying the lookback decorrelates members ",
        "more than reinitialising does.", sep = "")
}

morie_ngnest_ensemble <- morie_ngnest

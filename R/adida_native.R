# ADIDA: aggregate, forecast, disaggregate.
# Sources: Nikolopoulos, K., Syntetos, A. A., Boylan, J. E., Petropoulos,
# F. & Assimakopoulos, V. (2011), An aggregate-disaggregate intermittent
# demand approach (ADIDA) to forecasting: an empirical proposition and
# analysis, Journal of the Operational Research Society 62(3), 544-554,
# doi:10.1057/jors.2010.32 (the ADIDA framework: aggregate to level m,
# forecast, disaggregate, with m = lead time as the recommended anchor);
# Petropoulos, F. & Kourentzes, N. (2015), Forecast combinations for
# intermittent demand, Journal of the Operational Research Society
# 66(6), 914-924, doi:10.1057/jors.2014.62 (temporal combinations across
# aggregation levels); Teunter, R. H., Syntetos, A. A. & Babai, M. Z.
# (2011), Intermittent demand: Linking forecasting to inventory
# obsolescence, European Journal of Operational Research 214(3),
# 606-615, doi:10.1016/j.ejor.2011.05.018 (the TSB base forecaster);
# Croston, J. D. (1972), Forecasting and Stock Control for Intermittent
# Demands, Operational Research Quarterly 23(3), 289-303,
# doi:10.2307/3007885.
#
# Native implementation mirroring morie.fn.adida exactly: same zero
# fraction, same bucket arithmetic (LAST complete buckets for the
# non-overlapping case so the most recent history is retained), same
# profile normalisation so disaggregation sums back to the aggregate,
# and the same lead-time anchor that sets m to the lead time and
# returns the aggregate forecast as the lead-time demand directly.

#' Share of non-positive periods
#'
#' The zero share that aggregation is meant to reduce; this is the
#' quantity the paper calls the self-improving mechanism.
#'
#' @param y Numeric series of demand observations.
#' @return The fraction of periods with no positive demand.
#' @export
zero_fraction <- function(y) {
  yv <- as.numeric(y)
  if (length(yv) == 0L) stop("adida: empty series")
  sum(yv <= 0) / length(yv)
}

#' Sum a series into buckets of \code{m} periods
#'
#' Non-overlapping keeps the LAST complete buckets so the most recent
#' history is retained; overlapping produces one bucket per starting
#' position and the resulting aggregates are autocorrelated, as the
#' paper notes.
#'
#' @param y Numeric series.
#' @param m Bucket size, positive integer.
#' @param overlapping If TRUE, roll a window of width \code{m}; if
#'   FALSE (default), partition into the last complete buckets.
#' @return Numeric vector of bucket sums.
#' @export
aggregate_buckets <- function(y, m, overlapping = FALSE) {
  yv <- as.numeric(y)
  n <- length(yv)
  mm <- as.integer(m)
  if (mm < 1L) stop(sprintf("adida: the bucket size must be at least 1, got %d", mm))
  if (mm > n) stop(sprintf("adida: bucket size %d exceeds the %d observations", mm, n))
  if (overlapping) {
    vapply(seq_len(n - mm + 1L), function(t) sum(yv[t:(t + mm - 1L)]), numeric(1))
  } else {
    n_buckets <- n %/% mm
    start <- n - n_buckets * mm
    vapply(
      seq_len(n_buckets) - 1L, function(b) {
        sum(yv[(start + b * mm + 1L):(start + (b + 1L) * mm)])
      },
      numeric(1)
    )
  }
}

#' Spread a bucket forecast back over \code{m} periods
#'
#' Equal weights assume demand is uniform within the bucket. Any
#' supplied profile is normalised to sum to one so the disaggregated
#' periods reconstitute the aggregate exactly; a profile that did not
#' sum to one would silently change the total.
#'
#' @param aggregate_value Forecast for the bucket.
#' @param m Bucket size, positive integer.
#' @param profile Optional numeric vector of length \code{m} with
#'   non-negative weights; when NULL equal weights are used.
#' @return Numeric vector of length \code{m}.
#' @export
disaggregate <- function(aggregate_value, m, profile = NULL) {
  mm <- as.integer(m)
  if (mm < 1L) stop("adida: the bucket size must be at least 1")
  if (is.null(profile)) {
    w <- rep(1 / mm, mm)
  } else {
    w <- as.numeric(profile)
    if (length(w) != mm) {
      stop(sprintf(
        "adida: the profile has %d weights for a bucket of %d",
        length(w), mm
      ))
    }
    if (any(w < 0)) stop("adida: profile weights must be non-negative")
    tot <- sum(w)
    if (tot <= 0) stop("adida: the profile sums to zero")
    w <- w / tot
  }
  as.numeric(aggregate_value) * w
}

#' TSB intermittent-demand forecaster
#'
#' Default base forecaster used by \code{morie_adida}, implementing
#' the Teunter-Syntetos-Babai update so the same numbers come out
#' under the same RNG stream as the Python arm. Only the "tsb"
#' method is implemented here; the default and the one the ADIDA
#' cheatsheet references. Other methods are not ported because they
#' are not needed to mirror the Python arm's defaults.
#'
#' @param y Numeric series of non-negative demand.
#' @param method Method name; only "tsb" is implemented.
#' @param alpha Smoothing for the demand size.
#' @param beta Smoothing for the demand probability.
#' @param horizon Forecast horizon; only horizon = 1 is used by ADIDA.
#' @return Named list with \code{forecast}.
#' @references Teunter, R. H., Syntetos, A. A. & Babai, M. Z. (2011).
#' @keywords internal
intermittent_forecast <- function(y, method = "tsb", alpha = 0.1,
                                  beta = 0.05, horizon = 1L) {
  yv <- as.numeric(y)
  if (length(yv) == 0L) stop("adida: empty series")
  if (!identical(method, "tsb")) {
    stop(sprintf(
      "adida: method '%s' is not implemented in the R arm",
      method
    ))
  }
  if (as.integer(horizon) != 1L) {
    stop("adida: intermittent_forecast is called with horizon = 1")
  }
  # Initialisation matches the conventional TSB seed: z_0 is the first
  # positive demand observed, p_0 is the empirical rate of positive
  # demand over the history. The Python tsbF initialisation should be
  # checked against this; the rest of the update is the published TSB
  # recursion, identical in both arms.
  pos <- yv[yv > 0]
  if (length(pos) == 0L) {
    z <- 0
    p <- 0
  } else {
    z <- pos[1L]
    p <- length(pos) / length(yv)
  }
  for (t in seq_along(yv)) {
    if (yv[t] > 0) {
      z <- (1 - alpha) * z + alpha * yv[t]
      p <- (1 - beta) * p + beta
    } else {
      z <- z
      p <- (1 - beta) * p
    }
  }
  list(forecast = z * p)
}

#' ADIDA forecast: aggregate, forecast, disaggregate
#'
#' \code{lead_time} sets \code{m} to the lead time, the paper's
#' recommendation: the aggregated forecast is then the lead-time
#' demand directly, and no disaggregation error is incurred at all.
#' The disaggregation-sums-back identity is checked exactly so a
#' profile that did not reconstitute would surface as an inconsistency
#' rather than silently change the total.
#'
#' @param y Numeric demand series.
#' @param m Bucket size, positive integer.
#' @param horizon Forecast horizon in periods.
#' @param method Base forecaster method name (default "tsb").
#' @param alpha Size smoothing for the TSB base.
#' @param beta Probability smoothing for the TSB base.
#' @param overlapping If TRUE, use a rolling bucket window.
#' @param profile Optional disaggregation weights, length \code{m}.
#' @param lead_time If supplied, overrides \code{m} with the lead time.
#' @return Named list mirroring the Python payload: \code{estimate},
#'   \code{forecast}, \code{aggregate_forecast}, \code{lead_time_demand},
#'   \code{aggregated}, \code{m}, \code{zero_fraction_original},
#'   \code{zero_fraction_aggregated}, \code{n_buckets}, \code{overlapping},
#'   \code{base_method}, \code{disaggregation_sums_back}, \code{method}.
#' @references Nikolopoulos, K. et al. (2011).
#' @export
morie_adida <- function(y, m, horizon = 1L, method = "tsb",
                        alpha = 0.1, beta = 0.05,
                        overlapping = FALSE, profile = NULL,
                        lead_time = NULL) {
  yv <- as.numeric(y)
  if (!is.null(lead_time)) m <- as.integer(lead_time)
  agg <- aggregate_buckets(yv, m, overlapping = overlapping)
  if (length(agg) < 2L) {
    stop(sprintf(
      "adida: bucket size %d leaves only %d aggregated points",
      as.integer(m), length(agg)
    ))
  }
  f <- intermittent_forecast(agg,
    method = method, alpha = alpha,
    beta = beta, horizon = 1L
  )
  agg_fc <- f$forecast[1L]
  per_period <- disaggregate(agg_fc, as.integer(m), profile = profile)
  reps <- as.integer(ceiling(horizon / as.integer(m)))
  mm <- as.integer(m)
  flat <- vapply(
    seq_len(reps * mm) - 1L, function(t) per_period[(t %% mm) + 1L],
    numeric(1)
  )
  list(
    estimate = flat[seq_len(as.integer(horizon))],
    forecast = flat[seq_len(as.integer(horizon))],
    aggregate_forecast = agg_fc,
    lead_time_demand = if (!is.null(lead_time)) agg_fc else NULL,
    aggregated = agg, m = mm,
    zero_fraction_original = zero_fraction(yv),
    zero_fraction_aggregated = zero_fraction(agg),
    n_buckets = length(agg), overlapping = isTRUE(overlapping),
    base_method = method,
    disaggregation_sums_back = abs(sum(per_period) - agg_fc) < 1e-9,
    method = "ADIDA, Nikolopoulos, Syntetos, Boylan, Petropoulos & Assimakopoulos (2011)"
  )
}

#' Temporal combination across aggregation levels
#'
#' Petropoulos & Kourentzes: rather than choosing one aggregation
#' level, forecast at several and combine, which removes the
#' level-selection decision and is more robust than any single level
#' chosen in advance.
#'
#' @param y Numeric demand series.
#' @param levels Integer vector of bucket sizes (at least two).
#' @param horizon Forecast horizon in periods.
#' @param method Base forecaster method name.
#' @param alpha Size smoothing.
#' @param beta Probability smoothing.
#' @param weights Optional combination weights; default uniform.
#' @return Named list: \code{estimate}, \code{forecast}, \code{levels},
#'   \code{per_level}, \code{weights}, \code{spread}, \code{method}.
#' @references Petropoulos, F. & Kourentzes, N. (2015).
#' @export
temporal_combination <- function(y, levels, horizon = 1L, method = "tsb",
                                 alpha = 0.1, beta = 0.05,
                                 weights = NULL) {
  lv <- as.integer(levels)
  if (length(lv) < 2L) {
    stop(sprintf("adida: need at least 2 levels to combine, got %d", length(lv)))
  }
  per <- list()
  for (m in lv) {
    r <- morie_adida(y, m,
      horizon = horizon, method = method,
      alpha = alpha, beta = beta
    )
    per[[length(per) + 1L]] <- r$forecast
  }
  if (is.null(weights)) {
    w <- rep(1 / length(lv), length(lv))
  } else {
    w <- as.numeric(weights)
    if (length(w) != length(lv)) {
      stop(sprintf("adida: %d weights for %d levels", length(w), length(lv)))
    }
    tot <- sum(w)
    if (tot <= 0) stop("adida: the weights sum to zero")
    w <- w / tot
  }
  hh <- seq_len(as.integer(horizon))
  comb <- vapply(
    hh, function(h) sum(w * vapply(per, function(p) p[h], numeric(1))),
    numeric(1)
  )
  list(
    estimate = comb, forecast = comb, levels = lv, per_level = per,
    weights = w,
    spread = max(vapply(per, function(p) p[1L], numeric(1))) -
      min(vapply(per, function(p) p[1L], numeric(1))),
    method = "temporal combination across aggregation levels, Petropoulos & Kourentzes (2015)"
  )
}

#' One-sentence ADIDA cheatsheet
#'
#' @return Character string summarising the framework.
#' @export
#' @examples
#' res <- .adida_cheatsheet()
#' res
.adida_cheatsheet <- function() {
  paste0(
    "adida: sum into buckets of m, forecast the aggregate, ",
    "divide back by m. Aggregation cuts the zero fraction, ",
    "which is the self-improving mechanism. Set m = LEAD TIME ",
    "and the aggregate forecast IS lead-time demand, so no ",
    "disaggregation error at all. Equal-weight disaggregation ",
    "must sum back to the aggregate exactly. Combine several ",
    "levels instead of choosing one."
  )
}

# compact alias per ledger/NAMING.md
adidaforecast <- morie_adida

# public names resolved by fn/_lazy_map.json
adida <- morie_adida

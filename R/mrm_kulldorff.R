# SPDX-License-Identifier: AGPL-3.0-or-later

#' Kulldorff space-time scan statistic on TPS event data
#'
#' R parity of \code{morie.mrm_kulldorff.mrm_tps_kulldorff_scan()}.
#' Implements Kulldorff's 1997 Poisson log-likelihood-ratio space-time
#' scan with a Monte-Carlo permutation test for significance.
#'
#' The scan iterates over (centre, radius, time-window) tuples,
#' computing the Poisson LRT against \eqn{H_0} (events uniformly
#' distributed in space and time). The maximum LRT is the test
#' statistic; permutations of event timestamps generate the null.
#'
#' @references
#' Kulldorff, M. (1997). A spatial scan statistic. Communications in
#' Statistics: Theory and Methods, 26(6), 1481--1496.
#'
#' @return \code{mrm_tps_kulldorff_scan()} returns a named \code{list} with
#'   the most likely cluster, its Poisson log-likelihood-ratio statistic,
#'   the Monte-Carlo permutation p-value, and a plain-language
#'   \code{interpretation}.
#' @name mrm_kulldorff
#' @examples
#' \dontshow{if (requireNamespace("rmoriedata", quietly = TRUE)) withAutoprint(\{ # examplesIf}
#' if (FALSE) {
#'   tps <- morie_sample("tps_assault")
#'   mrm_tps_kulldorff_scan(tps, n_permutations = 49)
#' }
#' \dontshow{\}) # examplesIf}
NULL


#' Internal helper: Haversine Km Mat
#' @noRd
.haversine_km_mat <- function(lat1, lon1, lat2, lon2) {
  R <- 6371
  rad <- pi / 180
  dlat <- (lat2 - lat1) * rad
  dlon <- (lon2 - lon1) * rad
  a <- sin(dlat / 2)^2 + cos(lat1 * rad) * cos(lat2 * rad) * sin(dlon / 2)^2
  2 * R * asin(pmin(1, sqrt(a)))
}


#' Internal helper: Poisson Lrt
#' @noRd
.poisson_lrt <- function(n_obs, n_in, n_exp, n_tot) {
  if (n_in == 0 || n_obs == 0 || n_obs <= n_exp) {
    return(0.0)
  }
  obs_out <- n_tot - n_obs
  exp_out <- n_tot - n_exp
  if (obs_out == 0 || exp_out <= 0) {
    return(0.0)
  }
  n_obs * log(n_obs / n_exp) + obs_out * log(obs_out / exp_out)
}


#' Run a 3-d (lat, lon, time) Kulldorff scan with MC inference
#'
#' @param data data.frame with date_col, lat_col, lon_col.
#' @param date_col Column name of the event date (default
#'   \code{"OCC_DATE"}).
#' @param lat_col,lon_col WGS84 lat/long column names.
#' @param radii_km Candidate cylinder radii in km.
#' @param window_years Time-cylinder length in years.
#' @param n_centers Number of random candidate centres sub-sampled.
#' @param n_permutations Monte-Carlo permutations.
#' @param n_top_clusters Integer; maximum number of clusters to return,
#'   by descending LRT with no centre within the radius of a
#'   higher-LRT cluster.  Each p-value compares its LRT with the
#'   permutation null of the maximum LRT (SaTScan's rule).
#' @param seed Random seed.
#' @return A data.frame with one row per cluster (expected count
#'   \eqn{n_{space} n_{time} / n}, the space-time permutation
#'   expectation), with
#'   columns \code{center_lat}, \code{center_lon}, \code{radius_km},
#'   \code{t_start}, \code{t_end}, \code{n_observed}, \code{n_expected},
#'   \code{relative_risk}, \code{log_lrt}, \code{p_value}.
#' @export
#' @examplesIf requireNamespace("rmoriedata", quietly = TRUE)
#' if (FALSE) {
#'   tps <- morie_sample("tps_assault")
#'   mrm_tps_kulldorff_scan(tps, n_permutations = 49)
#' }
mrm_tps_kulldorff_scan <- function(
  data,
  date_col = "OCC_DATE",
  lat_col = "LAT_WGS84",
  lon_col = "LONG_WGS84",
  radii_km = c(1, 2, 3, 5, 8),
  window_years = 4,
  n_centers = 60L,
  n_permutations = 199L,
  n_top_clusters = 1L,
  seed = 42L
) {
  if (!is.numeric(n_top_clusters) || n_top_clusters < 1L) {
    stop("n_top_clusters must be a positive integer.", call. = FALSE)
  }
  stopifnot(is.data.frame(data))
  stopifnot(all(c(date_col, lat_col, lon_col) %in% names(data)))
  .rmorie_local_seed(as.integer(seed))

  df <- data[, c(date_col, lat_col, lon_col)]
  d <- suppressWarnings(as.POSIXct(df[[date_col]],
    format = "%m/%d/%Y %I:%M:%S %p",
    tz = "UTC"
  ))
  if (all(is.na(d))) d <- suppressWarnings(as.POSIXct(df[[date_col]], tz = "UTC"))
  df[[date_col]] <- as.Date(d)
  df <- df[stats::complete.cases(df), ]
  df <- df[order(df[[date_col]]), ]
  n <- nrow(df)
  if (n < 100L) {
    return(data.frame())
  }

  lat <- df[[lat_col]]
  lon <- df[[lon_col]]
  t <- as.integer(df[[date_col]])
  center_idx <- sample.int(n, min(n_centers, n))
  window_days <- round(window_years * 365.25)
  starts <- seq(min(t), max(t) - window_days, length.out = max(2L, (max(t) - min(t)) %/% window_days))

  cands <- list()
  scan_one <- function(t_obs, keep = FALSE) {
    best <- list(lrt = 0, ci = -1L, ri = -1L, ti = -1L, n_in = 0L,
                 n_space = 0L, n_time = 0L)
    for (ci in center_idx) {
      d_km <- .haversine_km_mat(lat[ci], lon[ci], lat, lon)
      for (ri in seq_along(radii_km)) {
        r <- radii_km[ri]
        in_space <- d_km <= r
        n_space <- sum(in_space)
        if (n_space < 5L) next
        for (ti in starts) {
          in_time <- (t_obs >= ti) & (t_obs < ti + window_days)
          if (!any(in_time)) next
          in_cyl <- in_space & in_time
          n_in_cyl <- sum(in_cyl)
          if (n_in_cyl < 5L) next
          n_exp <- n_space * sum(in_time) / n
          lrt <- .poisson_lrt(n_in_cyl, n_space, n_exp, n)
          cand <- list(
            lrt = lrt, ci = ci, ri = ri, ti = ti,
            n_in = n_in_cyl, n_space = n_space, n_time = sum(in_time)
          )
          # every cylinder with a positive LRT, for secondary clusters
          if (keep && lrt > 0) cands[[length(cands) + 1L]] <<- cand
          if (lrt > best$lrt) best <- cand
        }
      }
    }
    best
  }

  obs <- scan_one(t, keep = TRUE)
  null <- numeric(n_permutations)
  for (k in seq_len(n_permutations)) null[k] <- scan_one(sample(t))$lrt
  if (obs$ci < 0) {
    return(data.frame())
  }

  # secondary clusters: descending LRT, no centre within the radius of a
  # higher-LRT cluster
  cands <- cands[order(-vapply(cands, `[[`, numeric(1), "lrt"))]
  chosen <- list()
  for (cd in cands) {
    if (length(chosen) >= n_top_clusters) break
    ok <- all(vapply(chosen, function(ch) {
      .haversine_km_mat(lat[ch$ci], lon[ch$ci], lat[cd$ci], lon[cd$ci]) >
        radii_km[ch$ri]
    }, logical(1)))
    if (ok) chosen[[length(chosen) + 1L]] <- cd
  }
  do.call(rbind, lapply(chosen, function(cl) {
    # space-time permutation expectation (Kulldorff et al. 2005), the
    # same mu the scan maximised: n_space * n_time / n
    n_exp <- cl$n_space * cl$n_time / n
    data.frame(
      center_lat = lat[cl$ci],
      center_lon = lon[cl$ci],
      radius_km = radii_km[cl$ri],
      t_start = as.Date(cl$ti, origin = "1970-01-01"),
      t_end = as.Date(cl$ti + window_days, origin = "1970-01-01"),
      n_observed = cl$n_in,
      n_expected = round(n_exp, 2),
      relative_risk = round(cl$n_in / n_exp, 3),
      log_lrt = round(cl$lrt, 2),
      # every cluster is judged against the null of the maximum LRT
      p_value = round((sum(null >= cl$lrt) + 1) / (n_permutations + 1), 4)
    )
  }))
}

# SPDX-License-Identifier: AGPL-3.0-or-later
# Climate indices and climate-change diagnostics.
# Identical to the Python arm morie.fn.climidx.

#' Climate indices: ONI, PDO, QBO, degree heating weeks, cyclone energy, Hadley edge, scaling and downscaling
#'
#' \code{OniIndex}: Oceanic Nino Index (NOAA CPC), the centred 3-month mean of
#' the Nino-3.4 anomaly from the calendar-month climatology of the months
#' \code{base} (index range, default all); El Nino (La Nina) episodes are at
#' least \code{min_run} consecutive seasons with ONI at or above
#' \code{threshold} (at or below minus \code{threshold}).
#' \code{PdoIndex}: Pacific Decadal Oscillation, the standardised leading
#' principal component of monthly North Pacific SST anomalies (minus the
#' global-mean anomaly when \code{global_mean} is given, columns scaled by
#' \code{sqrt(weights)}); the largest loading is made negative.
#' \code{QboIndex}: deseasonalised equatorial 30 hPa zonal wind, its phase,
#' westerly onsets and periods. \code{DegreeHeatingWeeks}: Coral Reef Watch
#' HotSpots \code{max(SST - MMM, 0)}, degree heating weeks over the trailing
#' 84 days counting HotSpots of at least 1 degree, and alert levels 0 to 4.
#' \code{CycloneEnergy}: accumulated cyclone energy \code{1e-4 sum v^2}
#' (fixes at or above \code{threshold}) and power dissipation index
#' \code{sum v^3 dt}. \code{HurricaneTrack}: haversine step lengths,
#' translation speeds, bearings and the recurvature fix.
#' \code{HadleyEdge}: mean meridional mass streamfunction
#' \code{psi = (2 pi a cos(phi) / g) int v dp} (trapezoid rule from the top
#' level) and the zero crossings of psi at \code{level} poleward of its
#' northern maximum and southern minimum. \code{CcScaling}: binned
#' Clausius-Clapeyron scaling, the regression of the log \code{q} quantile of
#' precipitation on bin-centre temperature, rate \code{100 (exp(b) - 1)}
#' percent per degree, and the Bolton (1980) theoretical rate.
#' \code{SeaLevelSemiEmpirical}: Rahmstorf (2007) \code{dH/dt = a (T - T0)}
#' by least squares on mid-step temperatures, with projection.
#' \code{EmpiricalQuantileMap}: \code{Q_obs(F_hist(x))} with type 7
#' quantiles. \code{BcsdDownscale}: quantile-mapped coarse series, anomalies
#' from the observed climatology interpolated by inverse distance to fine
#' points and applied to the fine climatology. \code{ProbabilityRatio}:
#' exceedance probabilities, probability ratio and fraction of attributable risk.
#'
#' @param sst Numeric monthly (or daily for \code{DegreeHeatingWeeks}) series,
#'   or for \code{PdoIndex} a \code{T x N} matrix.
#' @param start_month Calendar month of the first value.
#' @param base Length-2 index range (0-based start, end exclusive) of the climatology months.
#' @param threshold ONI episode threshold, or ACE wind threshold, or exceedance threshold.
#' @param min_run Minimum episode length in seasons.
#' @param weights Area weights per column.
#' @param global_mean Global-mean SST series subtracted after deseasonalising.
#' @param u30 Monthly 30 hPa equatorial zonal wind.
#' @param mmm Maximum monthly mean climatology.
#' @param days_per_obs Days between observations.
#' @param window_days Accumulation window in days.
#' @param vmax Maximum sustained winds of one storm.
#' @param dt_hours Hours between fixes.
#' @param lat,lon Latitudes and longitudes (degrees).
#' @param radius_km Earth radius (km).
#' @param p Pressure levels (Pa, increasing downward).
#' @param v Zonal-mean meridional wind, levels by latitudes.
#' @param level Pressure level of the edge metric.
#' @param radius Earth radius (m).
#' @param g Gravity.
#' @param temp Temperatures.
#' @param precip Precipitation.
#' @param bin_width Temperature bin width.
#' @param q Quantile level.
#' @param min_count Minimum values per bin.
#' @param sea_level Sea-level series.
#' @param dt Time step.
#' @param future_temp Future temperature path.
#' @param obs,model_hist,model_future Observed, model-historical and model-future series.
#' @param obs_coarse,gcm_hist,gcm_future Lists of coarse-cell series.
#' @param coarse_xy,fine_xy Coordinate matrices of coarse cells and fine points.
#' @param fine_clim Fine-scale climatology.
#' @param multiplicative Ratio anomalies (precipitation) instead of differences.
#' @param power Inverse-distance exponent.
#' @param factual,counterfactual Samples of the factual and counterfactual climates.
#' @return A list; see each function.
#' @references Mantua, N. J. et al. (1997). A Pacific interdecadal climate
#'   oscillation with impacts on salmon production. BAMS 78, 1069-1079.
#'   Reed, R. J. et al. (1961). J. Geophys. Res. 66, 813-818.
#'   Liu, G., Strong, A. E. and Skirving, W. (2003). Eos 84, 137-144.
#'   Emanuel, K. (2005). Nature 436, 686-688. Davis, S. M. and Rosenlof,
#'   K. H. (2012). J. Climate 25, 1061-1078. Lenderink, G. and van Meijgaard,
#'   E. (2008). Nature Geoscience 1, 511-514. Rahmstorf, S. (2007). Science
#'   315, 368-370. Wood, A. W. et al. (2004). Climatic Change 62, 189-216.
#'   Stott, P. A., Stone, D. A. and Allen, M. R. (2004). Nature 432, 610-614.
#' @examples
#' OniIndex(rep(c(26, 27, 28, 27), 3))$oni
#' DegreeHeatingWeeks(c(29, 30.5, 31, 30), 29, days_per_obs = 7)$dhw
#' EmpiricalQuantileMap(c(10, 20, 30), c(0, 1, 2), c(0.5, 1.5, 5))
#' @export
OniIndex <- function(sst, start_month = 1, base = NULL, threshold = 0.5, min_run = 5) {
  a <- .ci_anom(sst, start_month, base)
  n <- length(a$anom)
  oni <- rep(NaN, n)
  for (i in seq_len(max(n - 2, 0)) + 1) oni[i] <- (a$anom[i - 1] + a$anom[i] + a$anom[i + 1]) / 3
  warm <- .ci_runs(!is.na(oni) & oni >= threshold, min_run)
  cold <- .ci_runs(!is.na(oni) & oni <= -threshold, min_run)
  list(anomalies = a$anom, climatology = a$clim, oni = oni, phase = ifelse(warm, 1L, ifelse(cold, -1L, 0L)))
}

.ci_anom <- function(x, start_month, base) {
  x <- as.numeric(x)
  n <- length(x)
  idx <- if (is.null(base)) seq_len(n) else (base[1] + 1):base[2]
  mon <- (start_month - 1 + seq_len(n) - 1) %% 12
  clim <- vapply(0:11, function(m) {
    v <- x[idx[mon[idx] == m]]
    if (length(v)) sum(v) / length(v) else NaN
  }, 0)
  list(anom = x - clim[mon + 1], clim = clim)
}

.ci_runs <- function(flag, min_run) {
  r <- rle(flag)
  rep(r$values & r$lengths >= min_run, r$lengths)
}

#' @rdname OniIndex
#' @export
PdoIndex <- function(sst, start_month = 1, weights = NULL, global_mean = NULL) {
  sst <- as.matrix(sst)
  T_ <- nrow(sst)
  N <- ncol(sst)
  A <- vapply(seq_len(N), function(j) .ci_anom(sst[, j], start_month, NULL)$anom, numeric(T_))
  A <- matrix(A, T_, N)
  if (!is.null(global_mean)) A <- A - .ci_anom(global_mean, start_month, NULL)$anom
  sw <- if (is.null(weights)) rep(1, N) else sqrt(as.numeric(weights))
  A <- sweep(A, 2, sw, "*")
  C <- crossprod(A) / (T_ - 1)
  ev <- eigen(C, symmetric = TRUE)
  e <- ev$vectors[, 1]
  if (e[which.max(abs(e))] > 0) e <- -e
  pc <- as.numeric(A %*% e)
  m <- sum(pc) / T_
  sd_ <- sqrt(sum((pc - m)^2) / (T_ - 1))
  list(index = (pc - m) / sd_, pc = pc, loadings = e, eigenvalue = ev$values[1],
       variance_fraction = ev$values[1] / sum(ev$values))
}

#' @rdname OniIndex
#' @export
QboIndex <- function(u30, start_month = 1) {
  a <- .ci_anom(u30, start_month, NULL)
  x <- a$anom
  n <- length(x)
  on <- if (n > 1) which(x[-n] < 0 & x[-1] >= 0) + 1L else integer(0)
  periods <- diff(on)
  list(index = x, climatology = a$clim, phase = ifelse(x >= 0, 1L, -1L), onsets = on,
       periods = periods, mean_period = if (length(periods)) sum(periods) / length(periods) else NaN)
}

#' @rdname OniIndex
#' @export
DegreeHeatingWeeks <- function(sst, mmm, days_per_obs = 1, window_days = 84) {
  hs <- pmax(as.numeric(sst) - mmm, 0)
  win <- round(window_days / days_per_obs)
  dhw <- numeric(length(hs))
  for (t in seq_along(hs)) {
    s <- 0
    for (j in max(1, t - win + 1):t) if (hs[j] >= 1) s <- s + hs[j]
    dhw[t] <- s * days_per_obs / 7
  }
  alert <- ifelse(hs <= 0, 0L, ifelse(hs < 1, 1L, ifelse(dhw < 4, 2L, ifelse(dhw < 8, 3L, 4L))))
  list(hotspot = hs, dhw = dhw, alert = alert)
}

#' @rdname OniIndex
#' @export
CycloneEnergy <- function(vmax, dt_hours = 6, threshold = 35) {
  v <- as.numeric(vmax)
  list(ace = 1e-4 * sum(v[v >= threshold]^2), pdi = sum(v^3) * dt_hours * 3600, vmax = max(v),
       duration_hours = dt_hours * (length(v) - 1))
}

#' @rdname OniIndex
#' @export
HurricaneTrack <- function(lat, lon, dt_hours = 6, radius_km = 6371) {
  rad <- pi / 180
  n <- length(lat)
  p1 <- lat[-n] * rad
  p2 <- lat[-1] * rad
  dl <- diff(lon) * rad
  a <- sin((p2 - p1) / 2)^2 + cos(p1) * cos(p2) * sin(dl / 2)^2
  d <- 2 * radius_km * asin(sqrt(a))
  b <- atan2(sin(dl) * cos(p2), cos(p1) * sin(p2) - sin(p1) * cos(p2) * cos(dl))
  rec <- NULL
  if (n > 2) for (i in 2:(n - 1)) if (lon[i] - lon[i - 1] < 0 && lon[i + 1] - lon[i] > 0) {
    rec <- i
    break
  }
  sp <- d / dt_hours
  list(distance = d, speed = sp, heading = (b / rad) %% 360, length = sum(d),
       mean_speed = sum(sp) / length(sp), recurvature = rec)
}

#' @rdname OniIndex
#' @export
HadleyEdge <- function(lat, p, v, level = 50000, radius = 6.371e6, g = 9.80665) {
  v <- as.matrix(v)
  K <- length(p)
  J <- length(lat)
  psi <- matrix(0, K, J)
  for (j in seq_len(J)) {
    cc <- 2 * pi * radius * cos(lat[j] * pi / 180) / g
    acc <- 0
    for (k in seq_len(K - 1) + 1) {
      acc <- acc + 0.5 * (v[k, j] + v[k - 1, j]) * (p[k] - p[k - 1])
      psi[k, j] <- cc * acc
    }
  }
  k <- min(max(which(p[seq_len(K - 1)] <= level)), K - 1)
  f <- (level - p[k]) / (p[k + 1] - p[k])
  lev <- psi[k, ] + f * (psi[k + 1, ] - psi[k, ])
  crossing <- function(js) {
    if (length(js) > 1) for (i in seq_len(length(js) - 1)) {
      a <- js[i]
      b <- js[i + 1]
      if (lev[a] * lev[b] <= 0 && lev[a] != lev[b]) return(lat[a] + (lat[b] - lat[a]) * lev[a] / (lev[a] - lev[b]))
    }
    NaN
  }
  north <- which(lat > 0)
  south <- which(lat < 0)
  en <- es <- NaN
  if (length(north)) en <- crossing(north[which.max(lev[north])]:J)
  if (length(south)) es <- crossing(south[which.min(lev[south])]:1)
  list(psi = psi, psi_level = lev, edge_north = en, edge_south = es)
}

.ci_q7 <- function(x, q) {
  s <- sort(as.numeric(x))
  h <- (length(s) - 1) * q
  lo <- floor(h)
  hi <- min(lo + 1, length(s) - 1)
  w <- h - lo
  if (w > 0) (1 - w) * s[lo + 1] + w * s[hi + 1] else s[lo + 1]
}

.ci_ols1 <- function(x, y) {
  n <- length(x)
  mx <- sum(x) / n
  my <- sum(y) / n
  b <- sum((x - mx) * (y - my)) / sum((x - mx)^2)
  c(my - b * mx, b)
}

#' @rdname OniIndex
#' @export
CcScaling <- function(temp, precip, bin_width = 2, q = 0.99, min_count = 10) {
  t <- as.numeric(temp)
  y <- as.numeric(precip)
  lo <- floor(min(t) / bin_width) * bin_width
  nb <- floor((max(t) - lo) / bin_width) + 1
  bin <- floor((t - lo) / bin_width)
  centres <- quants <- numeric(0)
  for (b in 0:(nb - 1)) {
    vals <- y[bin == b]
    if (length(vals) >= min_count) {
      qv <- .ci_q7(vals, q)
      if (qv > 0) {
        centres <- c(centres, lo + (b + 0.5) * bin_width)
        quants <- c(quants, qv)
      }
    }
  }
  ab <- .ci_ols1(centres, log(quants))
  tm <- sum(t) / length(t)
  list(rate = 100 * (exp(ab[2]) - 1), slope = ab[2], intercept = ab[1], bins = centres, quantiles = quants,
       cc_rate = 100 * 17.67 * 243.5 / (tm + 243.5)^2)
}

#' @rdname OniIndex
#' @export
SeaLevelSemiEmpirical <- function(temp, sea_level, dt = 1, future_temp = NULL) {
  T_ <- as.numeric(temp)
  H <- as.numeric(sea_level)
  n <- length(H)
  rate <- diff(H) / dt
  tm <- (T_[-n] + T_[-1]) / 2
  ab <- .ci_ols1(tm, rate)
  a <- ab[2]
  T0 <- -ab[1] / a
  proj <- numeric(0)
  if (!is.null(future_temp)) {
    h <- H[n]
    tp <- T_[n]
    for (tf in as.numeric(future_temp)) {
      h <- h + a * ((tp + tf) / 2 - T0) * dt
      proj <- c(proj, h)
      tp <- tf
    }
  }
  list(a = a, T0 = T0, intercept = ab[1], rate = rate, projection = proj)
}

#' @rdname OniIndex
#' @export
EmpiricalQuantileMap <- function(obs, model_hist, model_future) {
  h <- sort(as.numeric(model_hist))
  n <- length(h)
  vapply(as.numeric(model_future), function(x) {
    p <- if (x <= h[1]) 0 else if (x >= h[n]) 1 else {
      i <- max(which(h[-n] <= x))
      (i - 1 + (x - h[i]) / (h[i + 1] - h[i])) / (n - 1)
    }
    .ci_q7(obs, p)
  }, 0)
}

#' @rdname OniIndex
#' @export
BcsdDownscale <- function(obs_coarse, gcm_hist, gcm_future, coarse_xy, fine_xy, fine_clim,
                          multiplicative = FALSE, power = 2) {
  C <- length(obs_coarse)
  coarse_xy <- matrix(as.numeric(unlist(coarse_xy)), ncol = 2, byrow = is.list(coarse_xy))
  fine_xy <- matrix(as.numeric(unlist(fine_xy)), ncol = 2, byrow = is.list(fine_xy))
  bc <- lapply(seq_len(C), function(c) EmpiricalQuantileMap(obs_coarse[[c]], gcm_hist[[c]], gcm_future[[c]]))
  clim <- vapply(seq_len(C), function(c) sum(obs_coarse[[c]]) / length(obs_coarse[[c]]), 0)
  Tn <- length(bc[[1]])
  anom <- matrix(0, Tn, C)
  for (c in seq_len(C)) anom[, c] <- if (multiplicative) bc[[c]] / clim[c] else bc[[c]] - clim[c]
  F_ <- nrow(fine_xy)
  W <- matrix(0, F_, C)
  for (f in seq_len(F_)) {
    d <- sqrt((fine_xy[f, 1] - coarse_xy[, 1])^2 + (fine_xy[f, 2] - coarse_xy[, 2])^2)
    w <- if (min(d) == 0) as.numeric(seq_len(C) == which(d == 0)[1]) else d^(-power)
    W[f, ] <- w / sum(w)
  }
  fine <- matrix(0, Tn, F_)
  for (t in seq_len(Tn)) for (f in seq_len(F_)) {
    a <- sum(W[f, ] * anom[t, ])
    fine[t, f] <- if (multiplicative) fine_clim[f] * a else fine_clim[f] + a
  }
  list(fine = fine, bias_corrected = bc, anomalies = anom)
}

#' @rdname OniIndex
#' @export
ProbabilityRatio <- function(factual, counterfactual, threshold) {
  p1 <- sum(factual > threshold) / length(factual)
  p0 <- sum(counterfactual > threshold) / length(counterfactual)
  list(p1 = p1, p0 = p0, pr = if (p0 > 0) p1 / p0 else Inf, far = if (p1 > 0) 1 - p0 / p1 else NaN,
       return_period_1 = if (p1 > 0) 1 / p1 else Inf, return_period_0 = if (p0 > 0) 1 / p0 else Inf)
}

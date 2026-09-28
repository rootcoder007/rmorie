#' Agroclimatic indicators
#'
#' \code{GrowingDegreeDays}: averaging-method GDD with optional horizontal
#' cutoff (McMaster and Wilhelm 1997). \code{ChillAccumulation}: chill hours
#' (0 to 7.2 C) and Utah chill units (Richardson et al. 1974).
#' \code{LaiFromSavi}: METRIC leaf area index. \code{RainfallAdequacy}:
#' precipitation over crop evapotranspiration and per-period deficits.
#' \code{GrowingSeasonLength}: ETCCDI GSL. \code{ColdSpellDuration}: ETCCDI
#' CSDI. Identical to the Python arm \code{morie.fn.agroclim}.
#'
#' @param tmax,tmin Daily maximum and minimum temperatures.
#' @param base,upper Base and upper thresholds.
#' @param hourly_temp Hourly temperatures.
#' @param savi Soil-adjusted vegetation index values.
#' @param cap Maximum LAI.
#' @param precip,et0 Precipitation and reference evapotranspiration.
#' @param kc Crop coefficient(s).
#' @param tmean Daily mean temperatures for one year.
#' @param threshold Temperature threshold (scalar or per day).
#' @param span Minimum run length.
#' @param midyear 0-based day index after which the season may end.
#' @return Numeric or list.
#' @references McMaster, G. S. and Wilhelm, W. W. (1997). Growing
#'   degree-days: one equation, two interpretations. Agricultural and Forest
#'   Meteorology 87, 291-300.
#'
#'   Richardson, E. A., Seeley, S. D. and Walker, D. R. (1974). A model for
#'   estimating the completion of rest for Redhaven and Elberta peach trees.
#'   HortScience 9, 331-332.
#'
#'   Zhang, X. et al. (2011). Indices for monitoring changes in extremes
#'   based on daily temperature and precipitation data. WIREs Climate Change
#'   2, 851-870.
#' @examples
#' GrowingDegreeDays(c(25, 32, 12), c(11, 20, 4), base = 10, upper = 30)$total
#' ChillAccumulation(c(-1, 2, 5, 8, 10, 14, 17, 20))$utah_units
#' GrowingSeasonLength(c(rep(0, 100), rep(10, 150), rep(0, 115)))
#' @export
GrowingDegreeDays <- function(tmax, tmin, base = 10, upper = NULL) {
  if (!is.null(upper)) {
    tmax <- pmin(tmax, upper)
    tmin <- pmax(tmin, base)
  }
  d <- pmax(0, (tmax + tmin) / 2 - base)
  list(daily = d, cumulative = cumsum(d), total = sum(d))
}

#' @rdname GrowingDegreeDays
#' @export
ChillAccumulation <- function(hourly_temp) {
  t <- hourly_temp
  w <- ifelse(t <= 1.4, 0, ifelse(t <= 2.4, 0.5, ifelse(t <= 9.1, 1, ifelse(t <= 12.4, 0.5,
         ifelse(t <= 15.9, 0, ifelse(t <= 18, -0.5, -1))))))
  list(chill_hours = sum(t >= 0 & t <= 7.2), utah_units = sum(w))
}

#' @rdname GrowingDegreeDays
#' @export
LaiFromSavi <- function(savi, cap = 6) {
  ifelse(savi <= 0.1, 0, ifelse(savi >= 0.687, cap, pmin(cap, -log(pmax(0.69 - savi, 1e-300) / 0.59) / 0.91)))
}

#' @rdname GrowingDegreeDays
#' @export
RainfallAdequacy <- function(precip, et0, kc = 1) {
  etc <- kc * et0
  list(etc = etc, ratio = sum(precip) / sum(etc), deficit = pmax(0, etc - precip))
}

.ac_first_run <- function(flags, len, start = 1) {
  run <- 0
  if (start > length(flags)) return(NA_integer_)
  for (i in start:length(flags)) {
    run <- if (flags[i]) run + 1 else 0
    if (run == len) return(i - len + 1)
  }
  NA_integer_
}

#' @rdname GrowingDegreeDays
#' @export
GrowingSeasonLength <- function(tmean, threshold = 5, span = 6, midyear = 181) {
  s <- .ac_first_run(tmean > threshold, span)
  if (is.na(s)) return(0)
  e <- .ac_first_run(tmean < threshold, span, max(midyear + 1, s + 1))
  (if (is.na(e)) length(tmean) + 1 else e) - s
}

#' @rdname GrowingDegreeDays
#' @export
ColdSpellDuration <- function(tmin, threshold, span = 6) {
  r <- rle(tmin < threshold)
  cold <- r$values & r$lengths >= span
  list(csdi = sum(r$lengths[cold]), spells = sum(cold))
}

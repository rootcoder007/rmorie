# SPDX-License-Identifier: AGPL-3.0-or-later
#' GISS surface temperature anomaly
#'
#' A record is expressed as an anomaly against its own base-period mean
#' (1951-1980 in the GISS analysis); the anomaly at a location is the weighted
#' mean of station anomalies within 1200 km, the weight falling linearly from
#' one at the point to zero at 1200 km.  Source consulted: Hansen, Ruedy,
#' Glascoe and Sato (1999), GISS analysis of surface temperature change,
#' Journal of Geophysical Research 104(D24), 30997-31022.
#'
#' @param T numeric vector, or matrix with one station per row.
#' @param years optional numeric year of each column.
#' @param base length-2 inclusive first and last year of the climatology.
#' @param dist optional distance in km from the target to each station.
#' @param radius influence radius in km, 1200 in the GISS analysis.
#' @return list: estimate, anomaly, baseline, trend, intercept, nbase,
#'   nstation, n, method.
#' @keywords internal
#' @examples
#' giss(c(1, 2, 3))
#' @export
giss <- function(T, years = NULL, base = c(1951, 1980), dist = NULL, radius = 1200) {
  arr <- if (is.matrix(T)) T else matrix(as.numeric(T), nrow = 1)
  nst <- nrow(arr)
  m <- ncol(arr)
  if (is.null(years)) { yr <- seq_len(m) - 1
  inbase <- rep(TRUE, m) }
  else { yr <- as.numeric(years)
  inbase <- yr >= base[1] & yr <= base[2] }
  if (!any(inbase)) inbase <- rep(TRUE, m)
  nbase <- sum(inbase)
  baselines <- numeric(nst)
  anom <- matrix(0, nst, m)
  for (i in seq_len(nst)) {
    bm <- sum(arr[i, inbase]) / nbase
    baselines[i] <- bm
    anom[i, ] <- arr[i, ] - bm
  }
  if (is.null(dist)) w <- rep(1 / nst, nst)
  else {
    raw <- pmax(0, 1 - as.numeric(dist) / radius)
    w <- if (sum(raw) > 0) raw / sum(raw) else rep(1 / nst, nst)
  }
  ser <- as.numeric(t(anom) %*% w)
  beta <- t3ols(cbind(1, yr), ser)
  list(estimate = mean(ser), anomaly = ser,
       baseline = as.numeric(sum(w * baselines)),
       trend = as.numeric(beta[2]), intercept = as.numeric(beta[1]),
       nbase = as.integer(nbase), nstation = as.integer(nst),
       n = as.integer(m),
       method = "GISS base-period temperature anomaly (Hansen et al. 1999)")
}

# CANONICAL TEST
# r <- giss(c(1, 2, 3))
# stopifnot(abs(r$estimate) < 1e-12, abs(r$baseline - 2) < 1e-12, abs(r$trend - 1) < 1e-12)

#' @rdname giss
#' @keywords internal
#' @export
morie_giss_anomaly <- giss

# Canadian Forest Fire Weather Index System (Van Wagner & Pickett 1985).
# Source: Van Wagner & Pickett (1985), Forestry Technical Report 33
# (fetched-wave3/vanwagner-pickett-1985-ftr33.pdf) — implemented from
# the standard FORTRAN program statement by statement (FFMC 110-165,
# DMC 165-210, DC 215-235, ISI/BUI/FWI/DSR 235-280, DATA EL/FL).
# Anchored on the report's printed 49-day SAMPLE OF OUTPUT (p.17):
# all 49 x 7 values reproduced to print rounding.
# Note: later implementations (NOR-X-424; CRAN cffdrs) revise the
# FFMC constant 147.2 to 147.27723; we keep 147.2 to match the
# FTR-33 standard program and its printed sample output.
# Mirrors Python morie.fn.fwxF exactly.

.fwx_el <- c(6.5, 7.5, 9.0, 12.8, 13.9, 13.9, 12.4, 10.9, 9.4, 8.0, 7.0, 6.0)
.fwx_fl <- c(-1.6, -1.6, -1.6, 0.9, 3.8, 5.8, 6.4, 5.0, 2.4, 0.4, -1.6, -1.6)

.fwx_ffmc_day <- function(f0, t, h, w, r) {
  wmo <- 147.2 * (101 - f0) / (59.5 + f0)
  if (r > 0.5) {
    ra <- r - 0.5
    dm <- 42.5 * ra * exp(-100 / (251 - wmo)) * (1 - exp(-6.93 / ra))
    if (wmo > 150) {
      wmo <- wmo + dm + 0.0015 * (wmo - 150)^2 * sqrt(ra)
    } else {
      wmo <- wmo + dm
    }
    if (wmo > 250) wmo <- 250
  }
  ed <- 0.942 * h^0.679 + 11 * exp((h - 100) / 10) +
    0.18 * (21.1 - t) * (1 - 1 / exp(0.115 * h))
  if (wmo < ed) {
    ew <- 0.618 * h^0.753 + 10 * exp((h - 100) / 10) +
      0.18 * (21.1 - t) * (1 - 1 / exp(0.115 * h))
    if (wmo < ew) {
      z <- 0.424 * (1 - ((100 - h) / 100)^1.7) +
        0.0694 * sqrt(w) * (1 - ((100 - h) / 100)^8)
      x <- z * 0.581 * exp(0.0365 * t)
      wm <- ew - (ew - wmo) / 10^x
    } else {
      wm <- wmo
    }
  } else if (wmo == ed) {
    wm <- wmo
  } else {
    z <- 0.424 * (1 - (h / 100)^1.7) +
      0.0694 * sqrt(w) * (1 - (h / 100)^8)
    x <- z * 0.581 * exp(0.0365 * t)
    wm <- ed + (wmo - ed) / 10^x
  }
  ffm <- 59.5 * (250 - wm) / (147.2 + wm)
  if (ffm > 101) ffm <- 101
  if (ffm < 0) ffm <- 0
  ffm
}

.fwx_dmc_day <- function(p0, t, h, r, month) {
  if (t < -1.1) t <- -1.1
  rk <- 1.894 * (t + 1.1) * (100 - h) * .fwx_el[month] * 1e-4
  if (r > 1.5) {
    rw <- 0.92 * r - 1.27
    wmi <- 20 + 280 / exp(0.023 * p0)
    if (p0 <= 33) {
      b <- 100 / (0.5 + 0.3 * p0)
    } else if (p0 <= 65) {
      b <- 14 - 1.3 * log(p0)
    } else {
      b <- 6.2 * log(p0) - 17.2
    }
    wmr <- wmi + 1000 * rw / (48.77 + b * rw)
    pr <- 43.43 * (5.6348 - log(wmr - 20))
    if (pr < 0) pr <- 0
  } else {
    pr <- p0
  }
  pr + rk
}

.fwx_dc_day <- function(d0, t, r, month) {
  if (t < -2.8) t <- -2.8
  pe <- (0.36 * (t + 2.8) + .fwx_fl[month]) / 2
  if (r > 2.8) {
    rw <- 0.83 * r - 1.27
    smi <- 800 * exp(-d0 / 400)
    dr <- d0 - 400 * log(1 + 3.937 * rw / smi)
    if (dr < 0) dr <- 0
  } else {
    dr <- d0
  }
  dc <- dr + pe
  if (dc < 0) dc <- 0
  dc
}

.fwx_isi_bui_fwi <- function(ffm, dmc, dc, w) {
  fm <- 147.2 * (101 - ffm) / (59.5 + ffm)
  sf <- 19.115 * exp(fm * -0.1386) * (1 + fm^5.31 / 4.93e7)
  isi <- sf * exp(0.05039 * w)
  if (dmc == 0 && dc == 0) {
    bui <- 0
  } else {
    bui <- 0.8 * dc * dmc / (dmc + 0.4 * dc)
    if (bui < dmc) {
      p <- (dmc - bui) / dmc
      cc <- 0.92 + (0.0114 * dmc)^1.7
      bui <- dmc - cc * p
      if (bui < 0) bui <- 0
    }
  }
  if (bui > 80) {
    bb <- 0.1 * isi * (1000 / (25 + 108.64 / exp(0.023 * bui)))
  } else {
    bb <- 0.1 * isi * (0.626 * bui^0.809 + 2)
  }
  fwi <- if (bb <= 1) bb else exp(2.72 * (0.434 * log(bb))^0.647)
  dsr <- 0.0272 * fwi^1.77
  list(isi = isi, bui = bui, fwi = fwi, dsr = dsr)
}

#' Canadian Forest Fire Weather Index System (daily codes)
#'
#' Runs the six standard FWI System components day by day from noon
#' weather observations: FFMC, DMC, DC, ISI, BUI, FWI and the daily
#' severity rating DSR = 0.0272 FWI^1.77, following the standard
#' FORTRAN program of Van Wagner and Pickett (1985) statement by
#' statement.  Verified against the report's printed 49-day sample
#' output (all values to print rounding).
#'
#' @param temp Numeric vector of daily noon temperatures (deg C).
#' @param rh Numeric vector of relative humidities (percent).
#' @param wind Numeric vector of wind speeds (km/h).
#' @param rain Numeric vector of 24-h rainfalls (mm).
#' @param month Integer month (1-12), scalar or per-day vector.
#' @param ffmc_init,dmc_init,dc_init Starting code values
#'   (defaults 85, 6, 15 as in the report).
#' @return A list with daily numeric vectors \code{ffmc}, \code{dmc},
#'   \code{dc}, \code{isi}, \code{bui}, \code{fwi}, \code{dsr}, plus
#'   \code{n_days} and \code{method}.
#' @references Van Wagner, C. E. and Pickett, T. L. (1985).
#'   Equations and FORTRAN program for the Canadian Forest Fire
#'   Weather Index System. Canadian Forestry Service, Forestry
#'   Technical Report 33.  Van Wagner, C. E. (1987). Development and
#'   structure of the Canadian Forest Fire Weather Index System.
#'   Forestry Technical Report 35.
#' @export
morie_fwxf <- function(temp, rh, wind, rain, month, ffmc_init = 85,
                       dmc_init = 6, dc_init = 15) {
  t <- as.numeric(temp); h <- as.numeric(rh)
  w <- as.numeric(wind); r <- as.numeric(rain)
  n <- length(t)
  if (length(h) != n || length(w) != n || length(r) != n) {
    stop("temp, rh, wind, rain must have equal length")
  }
  mo <- as.integer(month)
  if (length(mo) == 1L) mo <- rep(mo, n)
  if (length(mo) != n) stop("month must be scalar or match n_days")
  if (any(mo < 1L | mo > 12L)) stop("month entries must be in 1..12")
  if (any(h < 0 | h > 100)) stop("rh must be in [0, 100]")
  f0 <- as.numeric(ffmc_init); p0 <- as.numeric(dmc_init)
  d0 <- as.numeric(dc_init)
  out <- list(ffmc = numeric(n), dmc = numeric(n), dc = numeric(n),
              isi = numeric(n), bui = numeric(n), fwi = numeric(n),
              dsr = numeric(n))
  for (i in seq_len(n)) {
    f0 <- .fwx_ffmc_day(f0, t[i], h[i], w[i], r[i])
    p0 <- .fwx_dmc_day(p0, t[i], h[i], r[i], mo[i])
    d0 <- .fwx_dc_day(d0, t[i], r[i], mo[i])
    ib <- .fwx_isi_bui_fwi(f0, p0, d0, w[i])
    out$ffmc[i] <- f0; out$dmc[i] <- p0; out$dc[i] <- d0
    out$isi[i] <- ib$isi; out$bui[i] <- ib$bui
    out$fwi[i] <- ib$fwi; out$dsr[i] <- ib$dsr
  }
  out$n_days <- n
  out$method <- "Canadian FWI System (Van Wagner & Pickett 1985)"
  out
}

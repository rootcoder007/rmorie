# Canadian Forest Fire Weather Index System (Van Wagner & Pickett 1985).
# Native R translation of the Python fwxF module.

# Effective day lengths (DMC, EL) and day-length factors (DC, FL) by
# month, transcribed from the DATA statements of the standard FORTRAN
# program in Van Wagner & Pickett (1985), Forestry Technical Report 33
# (fetched-wave3/vanwagner-pickett-1985-ftr33.pdf, program listing):
#   DATA EL /6.5,7.5,9.0,12.8,13.9,13.9,12.4,10.9,9.4,8.0,7.0,6.0/
#   DATA FL /-1.6,-1.6,-1.6,0.9,3.8,5.8,6.4,5.0,2.4,0.4,-1.6,-1.6/
.fwxF_EL <- c(6.5, 7.5, 9.0, 12.8, 13.9, 13.9, 12.4, 10.9, 9.4, 8.0, 7.0, 6.0)
.fwxF_FL <- c(-1.6, -1.6, -1.6, 0.9, 3.8, 5.8, 6.4, 5.0, 2.4, 0.4, -1.6, -1.6)


.fwxF_ffmc_day <- function(f0, t, h, w, r) {
  # Fine Fuel Moisture Code, statements 110-165 of the FTR-33 program.
  wmo <- 147.2 * (101.0 - f0) / (59.5 + f0)
  if (r > 0.5) {
    ra <- r - 0.5
    dm <- 42.5 * ra * exp(-100.0 / (251.0 - wmo)) *
      (1.0 - exp(-6.93 / ra))
    if (wmo > 150.0) {
      wmo <- wmo + dm + 0.0015 * (wmo - 150.0)^2 * sqrt(ra)
    } else {
      wmo <- wmo + dm
    }
    if (wmo > 250.0) {
      wmo <- 250.0
    }
  }
  ed <- 0.942 * h^0.679 + 11.0 * exp((h - 100.0) / 10.0) +
    0.18 * (21.1 - t) * (1.0 - 1.0 / exp(0.115 * h))
  if (wmo < ed) {
    ew <- 0.618 * h^0.753 + 10.0 * exp((h - 100.0) / 10.0) +
      0.18 * (21.1 - t) * (1.0 - 1.0 / exp(0.115 * h))
    if (wmo < ew) {
      z <- 0.424 * (1.0 - ((100.0 - h) / 100.0)^1.7) +
        0.0694 * sqrt(w) * (1.0 - ((100.0 - h) / 100.0)^8)
      x <- z * 0.581 * exp(0.0365 * t)
      wm <- ew - (ew - wmo) / 10.0^x
    } else {
      wm <- wmo
    }
  } else if (wmo == ed) {
    wm <- wmo
  } else {
    z <- 0.424 * (1.0 - (h / 100.0)^1.7) +
      0.0694 * sqrt(w) * (1.0 - (h / 100.0)^8)
    x <- z * 0.581 * exp(0.0365 * t)
    wm <- ed + (wmo - ed) / 10.0^x
  }
  ffm <- 59.5 * (250.0 - wm) / (147.2 + wm)
  if (ffm > 101.0) {
    ffm <- 101.0
  }
  if (ffm < 0.0) {
    ffm <- 0.0
  }
  ffm
}


.fwxF_dmc_day <- function(p0, t, h, r, month) {
  # Duff Moisture Code, statements 165-210.
  if (t < -1.1) {
    t <- -1.1
  }
  rk <- 1.894 * (t + 1.1) * (100.0 - h) * .fwxF_EL[month] * 1e-4
  if (r > 1.5) {
    rw <- 0.92 * r - 1.27
    wmi <- 20.0 + 280.0 / exp(0.023 * p0)
    if (p0 <= 33.0) {
      b <- 100.0 / (0.5 + 0.3 * p0)
    } else if (p0 <= 65.0) {
      b <- 14.0 - 1.3 * log(p0)
    } else {
      b <- 6.2 * log(p0) - 17.2
    }
    wmr <- wmi + 1000.0 * rw / (48.77 + b * rw)
    pr <- 43.43 * (5.6348 - log(wmr - 20.0))
    if (pr < 0.0) {
      pr <- 0.0
    }
  } else {
    pr <- p0
  }
  pr + rk
}


.fwxF_dc_day <- function(d0, t, r, month) {
  # Drought Code, statements 215-235.
  if (t < -2.8) {
    t <- -2.8
  }
  pe <- (0.36 * (t + 2.8) + .fwxF_FL[month]) / 2.0
  if (r > 2.8) {
    rw <- 0.83 * r - 1.27
    smi <- 800.0 * exp(-d0 / 400.0)
    dr <- d0 - 400.0 * log(1.0 + 3.937 * rw / smi)
    if (dr < 0.0) {
      dr <- 0.0
    }
  } else {
    dr <- d0
  }
  dc <- dr + pe
  if (dc < 0.0) {
    dc <- 0.0
  }
  dc
}


.fwxF_isi_bui_fwi <- function(ffm, dmc, dc, w) {
  # ISI, BUI, FWI, DSR, statements 235-280.
  fm <- 147.2 * (101.0 - ffm) / (59.5 + ffm)
  sf <- 19.115 * exp(fm * -0.1386) * (1.0 + fm^5.31 / 4.93e7)
  isi <- sf * exp(0.05039 * w)
  if (dmc == 0.0 && dc == 0.0) {
    bui <- 0.0
  } else {
    bui <- 0.8 * dc * dmc / (dmc + 0.4 * dc)
    if (bui < dmc) {
      p <- (dmc - bui) / dmc
      cc <- 0.92 + (0.0114 * dmc)^1.7
      bui <- dmc - cc * p
      if (bui < 0.0) {
        bui <- 0.0
      }
    }
  }
  if (bui > 80.0) {
    bb <- 0.1 * isi * (1000.0 / (25.0 + 108.64 / exp(0.023 * bui)))
  } else {
    bb <- 0.1 * isi * (0.626 * bui^0.809 + 2.0)
  }
  if (bb <= 1.0) {
    fwi <- bb
  } else {
    fwi <- exp(2.72 * (0.434 * log(bb))^0.647)
  }
  dsr <- 0.0272 * fwi^1.77
  c(isi = isi, bui = bui, fwi = fwi, dsr = dsr)
}


morie_fwxF <- function(temp, rh, wind, rain, month,
                       ffmc_init = 85.0, dmc_init = 6.0, dc_init = 15.0) {
  # Canadian Forest Fire Weather Index (FWI) System, daily codes.
  # Runs the six standard components day by day from noon weather
  # observations: the three moisture codes FFMC (fine fuel), DMC (duff)
  # and DC (drought), then ISI (initial spread), BUI (buildup), FWI
  # and the daily severity rating DSR = 0.0272 FWI^1.77.  Follows the
  # standard FORTRAN program of Van Wagner & Pickett (1985).
  t <- as.numeric(temp)
  h <- as.numeric(rh)
  w <- as.numeric(wind)
  r <- as.numeric(rain)
  n <- length(t)
  if (!(length(h) == n && length(w) == n && length(r) == n)) {
    stop("temp, rh, wind, rain must have equal length")
  }
  if (length(month) == 1L) {
    mo <- rep(as.integer(month), n)
  } else {
    mo <- as.integer(month)
    if (length(mo) != n) {
      stop("month must be scalar or match n_days")
    }
  }
  if (any(mo < 1L | mo > 12L)) {
    stop("month entries must be in 1..12")
  }
  if (any(h < 0.0 | h > 100.0)) {
    stop("rh must be in [0, 100]")
  }
  f0 <- as.numeric(ffmc_init)
  p0 <- as.numeric(dmc_init)
  d0 <- as.numeric(dc_init)
  out <- list(ffmc = numeric(n), dmc = numeric(n), dc = numeric(n),
              isi = numeric(n), bui = numeric(n), fwi = numeric(n),
              dsr = numeric(n))
  for (i in seq_len(n)) {
    f0 <- .fwxF_ffmc_day(f0, t[i], h[i], w[i], r[i])
    p0 <- .fwxF_dmc_day(p0, t[i], h[i], r[i], mo[i])
    d0 <- .fwxF_dc_day(d0, t[i], r[i], mo[i])
    res <- .fwxF_isi_bui_fwi(f0, p0, d0, w[i])
    out$ffmc[i] <- f0
    out$dmc[i] <- p0
    out$dc[i] <- d0
    out$isi[i] <- res[["isi"]]
    out$bui[i] <- res[["bui"]]
    out$fwi[i] <- res[["fwi"]]
    out$dsr[i] <- res[["dsr"]]
  }
  out$n_days <- n
  out$method <- "Canadian FWI System (Van Wagner & Pickett 1985)"
  out
}

fire_weather_index <- morie_fwxF

.fwxF_cheatsheet <- function() {
  "fwxF: Canadian FWI System daily FFMC/DMC/DC/ISI/BUI/FWI/DSR (FTR-33)"
}

.fwxF_cheatsheet <- .fwxF_cheatsheet

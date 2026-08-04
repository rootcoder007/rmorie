# R arm of morie/fn/krigsv.py -- weighted least squares variogram fit.
#
# The Python body was a placeholder: it averaged `coords` and never looked
# at `values` or `model`. There was no R arm at all.
#
# Empirical semivariogram: Matheron's method of moments,
#   gamma_hat(h) = 1/(2|N(h)|) sum_{N(h)} (Z(s_i) - Z(s_j))^2
# binned into n_bins lag classes (shared core .sp_empirical_variogram).
#
# Model: gamma(h) = c0 + c (1 - rho(h/a)) with rho the exponential
# exp(-u), gaussian exp(-u^2) or spherical 1 - 1.5u + 0.5u^3 correlogram,
# in the range-PARAMETER convention of expvar.
#
# Fitting is deterministic by construction: for fixed a the model is
# linear in (c0, c), so those come from a closed-form weighted least
# squares solve with the pair counts |N(h)| as weights, and a is profiled
# over a fixed logarithmic grid of 200 values between hmax/20 and 2 hmax.
# No iteration, no starting value, no restart, so both arms return the
# same numbers bit for bit.
#
# The weights are |N(h)|, NOT Cressie's |N(h)|/gamma(h;theta)^2, which
# would make the objective depend on the parameters and require
# iteration. Stated rather than hidden, because it changes the answer.
# Non-negativity is imposed by projection. The range is confined to the
# profiling grid [hmax/20, 2 hmax]; a weakly structured sample can push the
# optimum onto an endpoint, in which case the fitted range means only "at
# least this large" (or "at most"). range_at_bound flags exactly that and
# grid_lo / grid_hi say where the wall is. Do not read a boundary range as
# an estimate.
#
# Cressie (1993), Statistics for Spatial Data, rev. edn., secs. 2.4 and
# 2.6.2. Schabenberger & Gotway (2005) eq. (4.1) and eqs. (4.10)-(4.13).

#' @noRd
.krigsv_rho <- function(u, model) {
  if (identical(model, "exponential")) return(exp(-u))
  if (identical(model, "gaussian")) return(exp(-(u * u)))
  if (identical(model, "spherical")) {
    return(ifelse(u >= 1, 0, 1 - 1.5 * u + 0.5 * u^3))
  }
  stop(sprintf("unknown model '%s'; expected exponential, gaussian or spherical",
               model), call. = FALSE)
}

#' @noRd
.krigsv_wls2 <- function(x, y, w) {
  sw <- sum(w); sx <- sum(w * x); sxx <- sum(w * x * x)
  sy <- sum(w * y); sxy <- sum(w * x * y)
  det <- sw * sxx - sx * sx
  if (abs(det) > 1e-300) {
    c0 <- (sxx * sy - sx * sxy) / det
    cc <- (sw * sxy - sx * sy) / det
  } else {
    c0 <- -1; cc <- -1
  }
  if (c0 < 0 || cc < 0) {
    if (c0 < 0 && cc < 0) {
      c0 <- 0; cc <- 0
    } else if (c0 < 0) {
      c0 <- 0
      cc <- if (sxx > 0) sxy / sxx else 0
      if (cc < 0) cc <- 0
    } else {
      cc <- 0
      c0 <- sy / sw
      if (c0 < 0) c0 <- 0
    }
  }
  c(c0 = c0, c = cc, wss = sum(w * (y - c0 - cc * x)^2))
}

#' @noRd
morie_variogram_fit <- function(coords, values, model = "exponential",
                                n_bins = 15, max_dist = NULL) {
  ev <- .sp_empirical_variogram(coords, values, n_bins, max_dist)
  lag <- as.numeric(ev$lag); gam <- as.numeric(ev$gamma)
  cnt <- as.integer(ev$n_pairs)
  use <- which(cnt > 0 & !is.na(lag))
  if (length(use) < 3L) {
    stop("fewer than three non-empty lag classes; cannot fit three parameters",
         call. = FALSE)
  }
  hs <- lag[use]; gs <- gam[use]; ws <- as.numeric(cnt[use])

  NGRID <- 200L
  hmax <- max(hs)
  lo <- log(hmax / 20); hi <- log(2 * hmax)
  best <- NULL
  for (k in seq_len(NGRID) - 1L) {
    a <- exp(lo + (hi - lo) * k / (NGRID - 1L))
    x <- 1 - .krigsv_rho(hs / a, model)
    fit <- .krigsv_wls2(x, gs, ws)
    if (is.null(best) || fit[["wss"]] < best[["wss"]] - 1e-15) {
      best <- c(a = a, fit)
    }
  }
  a <- best[["a"]]; c0 <- best[["c0"]]; cc <- best[["c"]]
  lo_a <- exp(lo); hi_a <- exp(hi)
  range_at_bound <- (abs(a - lo_a) <= 1e-12 * lo_a) || (abs(a - hi_a) <= 1e-12 * hi_a)

  fitted <- c0 + cc * (1 - .krigsv_rho(hs / a, model))
  list(c0 = c0, c = cc, a = a, sill = c0 + cc, model = model,
       lag = hs, gamma_hat = gs, counts = as.integer(ws), fitted = fitted,
       wss = best[["wss"]], range_at_bound = range_at_bound,
       grid_lo = lo_a, grid_hi = hi_a, n_bins_used = length(use),
       method = paste("Weighted least squares variogram fit, |N(h)| weights,",
                      "range profiled on a fixed grid"))
}

#' @noRd
Krigsv <- morie_variogram_fit

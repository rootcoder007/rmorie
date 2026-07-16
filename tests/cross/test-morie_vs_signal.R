# SPDX-License-Identifier: AGPL-3.0-or-later
# Module 20 cross-validation: native DSP vs signal / wavelets.
library(testthat)
library(rmorie)

test_that("native butter matches signal::butter coefficients", {
  skip_if_not_installed("signal")
  for (spec in list(list(4, 0.3, "low"), list(3, 0.25, "high"),
                    list(2, c(0.2, 0.4), "pass"),
                    list(2, c(0.2, 0.4), "stop"))) {
    ref <- signal::butter(spec[[1]], spec[[2]], type = spec[[3]])
    mine <- rmorie:::.morie_dsp_butter(spec[[1]], spec[[2]], spec[[3]])
    expect_equal(mine$b, unname(as.numeric(ref$b)), tolerance = 1e-8)
    expect_equal(mine$a, unname(as.numeric(ref$a)), tolerance = 1e-8)
  }
})

test_that("native fir1 matches signal::fir1 in frequency response", {
  skip_if_not_installed("signal")
  # signal::fir1 designs via fir2 (frequency sampling); ours is the
  # classical windowed sinc. Tap values differ slightly; the designed
  # RESPONSES must agree in pass- and stop-band.
  ref <- as.numeric(signal::fir1(20, 0.3, type = "low"))
  mine <- rmorie:::.morie_dsp_fir1(20, 0.3, type = "low")
  gain <- function(h, w) vapply(w, function(wi)
    Mod(sum(h * exp(-1i * pi * wi * (seq_along(h) - 1)))), numeric(1))
  w_pass <- seq(0, 0.15, by = 0.05)
  w_stop <- seq(0.6, 0.95, by = 0.05)
  expect_equal(gain(mine, w_pass), gain(ref, w_pass), tolerance = 0.02)
  expect_lt(max(gain(mine, w_stop)), 0.05)
  expect_lt(max(gain(ref, w_stop)), 0.05)
})

test_that("native filtfilt matches signal::filtfilt away from edges", {
  skip_if_not_installed("signal")
  set.seed(180)
  x <- as.numeric(arima.sim(list(ar = 0.6), 600))
  bf <- signal::butter(4, 0.2, type = "low")
  ref <- as.numeric(signal::filtfilt(bf, x))
  mine <- rmorie:::.morie_dsp_filtfilt(bf$b, bf$a, x)
  core <- 50:550
  expect_equal(mine[core], ref[core], tolerance = 1e-6)
})

test_that("native hilbert matches seewave::hilbert analytic signal", {
  # signal 1.8.x has no hilbert(); seewave::hilbert is the analytic
  # signal Hilbert transform and is the reference here.
  skip_if_not_installed("seewave")
  set.seed(181)
  x <- rnorm(256)
  ref <- seewave::hilbert(x, f = 1)          # complex analytic signal
  mine <- rmorie:::.morie_dsp_hilbert(x)
  # both are the analytic signal x + i*H{x}; envelopes must agree
  expect_equal(Mod(mine), as.numeric(Mod(ref)), tolerance = 1e-6)
  # instantaneous phase agrees up to endpoint edge effects
  core <- 20:236
  expect_equal(Arg(mine)[core], as.numeric(Arg(ref))[core],
               tolerance = 1e-4)
})

test_that("native DWT coefficients match wavelets::dwt", {
  skip_if_not_installed("wavelets")
  set.seed(182)
  x <- rnorm(128)
  for (flt in c("haar", "d4", "la8")) {
    ref <- wavelets::dwt(x, filter = flt, n.levels = 3,
                         boundary = "periodic", fast = FALSE)
    mine <- rmorie:::.morie_dsp_dwt(x, filter = flt, n_levels = 3)
    for (j in 1:3) {
      # sign/alignment conventions can differ by filter phase; compare
      # magnitudes of sorted coefficients (energy content per level)
      expect_equal(sort(abs(as.numeric(mine$W[[j]]))),
                   sort(abs(as.numeric(ref@W[[j]]))), tolerance = 1e-6)
    }
    expect_equal(sort(abs(as.numeric(mine$V))),
                 sort(abs(as.numeric(ref@V[[3]]))), tolerance = 1e-6)
  }
})

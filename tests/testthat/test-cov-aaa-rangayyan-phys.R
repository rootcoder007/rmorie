# Coverage tests for R/aaa_rangayyan_phys.R (Rangayyan 2024, ch. 1, 5-10).
# Closed-form models are recomputed from their equations; spectral
# features are recomputed from a Hann periodogram built on stats::fft.
# The package uses its own radix-2 FFT, so spectral comparisons carry
# 1e-9 relative tolerance for the different rounding order.

phys_psd <- function(x, fs) {
  x <- x - mean(x)
  n <- length(x)
  w <- 0.5 - 0.5 * cos(2 * pi * (0:(n - 1)) / (n - 1))
  nfft <- 2^ceiling(log2(n))
  X <- fft(c(x * w, rep(0, nfft - n)))
  m <- nfft / 2 + 1
  list(freqs = (0:(m - 1)) * fs / nfft, power = Mod(X[1:m])^2 / sum(w^2))
}

phys_band <- function(sp, lo, hi) sum(sp$power[sp$freqs >= lo & sp$freqs < hi])

phys_yw <- function(x, p) {
  x <- x - mean(x)
  n <- length(x)
  r <- vapply(0:p, function(k) sum(x[1:(n - k)] * x[(1 + k):n]) / n, 0)
  a <- solve(toeplitz(r[1:p]), -r[2:(p + 1)])
  list(a = a, e = r[1] + sum(a * r[2:(p + 1)]))
}

test_that("action potential waveform is the ramp-and-decay formula", {
  t <- seq(-1, 6, by = 0.25)
  r <- ApWave(t, v_rest = -70, v_peak = 30, t_rise = 0.5, t_fall = 1, t_onset = 0)
  ref <- ifelse(t < 0, -70, ifelse(t < 0.5, -70 + 100 * t / 0.5, -70 + 100 * exp(-(t - 0.5))))
  expect_equal(r$V_mV, ref, tolerance = 1e-12)
  above <- t[ref >= -20]
  expect_equal(r$width_half_ms, max(above) - min(above))
  u <- ApWave(t, v_undershoot = -80, t_recover = 2)
  ref2 <- ifelse(t < 0, -70, ifelse(t < 0.5, -70 + 100 * t / 0.5,
    -80 + 110 * exp(-(t - 0.5)) + 10 * (1 - exp(-(t - 0.5) / 2))))
  expect_equal(u$V_mV, ref2, tolerance = 1e-12)
  expect_error(ApWave(t, v_undershoot = -60, t_recover = 1), "below v_rest")
  expect_error(ApWave(t, v_peak = -80), "exceed")
})

test_that("Nernst and GHK potentials follow RT/zF log ratios", {
  RT_F <- 8.314462618 * 310.15 / 96485.33212
  expect_equal(Nernst()$potential_mV, 1000 * RT_F * log(5 / 140), tolerance = 1e-12)
  ca <- Nernst(T = 300, z = 2, conc_out = 2, conc_in = 1e-4, ion = "Ca2+")
  expect_equal(ca$potential_V, 8.314462618 * 300 / (2 * 96485.33212) * log(2 / 1e-4), tolerance = 1e-12)
  expect_error(Nernst(z = 0), "non-zero")
  ic <- list(K_in = 140, K_out = 5, Na_in = 12, Na_out = 145, Cl_in = 4, Cl_out = 110)
  g <- Ghk(ic, P_K = 1, P_Na = 0.05, P_Cl = 0.4)
  num <- 5 + 0.05 * 145 + 0.4 * 4
  den <- 140 + 0.05 * 12 + 0.4 * 110
  expect_equal(g$potential_mV, 1000 * RT_F * log(num / den), tolerance = 1e-12)
  # with only K permeable GHK reduces to the K Nernst potential
  expect_equal(Ghk(ic, 1, 0, 0)$potential_mV, Nernst()$potential_mV, tolerance = 1e-12)
  expect_error(Ghk(ic[-1]), "missing keys")
  expect_error(Ghk(ic, P_K = -1), "non-negative")
})

test_that("Hodgkin-Huxley gates: steady state, time constant, relaxation", {
  V <- -30
  am <- 0.1 * (V + 40) / (1 - exp(-(V + 40) / 10))
  bm <- 4 * exp(-(V + 65) / 18)
  ah <- 0.07 * exp(-(V + 65) / 20)
  bh <- 1 / (1 + exp(-(V + 35) / 10))
  an <- 0.01 * (V + 55) / (1 - exp(-(V + 55) / 10))
  bn <- 0.125 * exp(-(V + 65) / 80)
  g <- HhGate(V)
  expect_equal(c(g$m_inf, g$h_inf, g$n_inf), c(am / (am + bm), ah / (ah + bh), an / (an + bn)), tolerance = 1e-12)
  expect_equal(g$tau_h_ms, 1 / (ah + bh), tolerance = 1e-12)
  g2 <- HhGate(V, dt = 0.05, m = 0.1, h = 0.9, n = 0.2, steps = 7)
  expect_equal(g2$m, g$m_inf + (0.1 - g$m_inf) * exp(-0.35 / g$tau_m_ms), tolerance = 1e-12)
  expect_equal(g2$n, g$n_inf + (0.2 - g$n_inf) * exp(-0.35 / g$tau_n_ms), tolerance = 1e-12)
  expect_equal(HhGate(-55)$alpha_per_ms$n, 0.1)
  expect_equal(HhGate(-40)$alpha_per_ms$m, 1)
  expect_error(HhGate(V, m = 2), "\\[0, 1\\]")
  expect_error(HhGate(V, dt = 0), "dt")
})

test_that("HH model is RK4 on the four HH equations", {
  rates <- function(v) {
    c(0.1 * (v + 40) / (1 - exp(-(v + 40) / 10)), 4 * exp(-(v + 65) / 18),
      0.07 * exp(-(v + 65) / 20), 1 / (1 + exp(-(v + 35) / 10)),
      0.01 * (v + 55) / (1 - exp(-(v + 55) / 10)), 0.125 * exp(-(v + 65) / 80))
  }
  f <- function(t, y) {
    r <- rates(y[1])
    I <- if (t >= 1 && t < 2) 10 else 0
    c(I - 120 * y[2]^3 * y[3] * (y[1] - 50) - 36 * y[4]^4 * (y[1] + 77) - 0.3 * (y[1] + 54.387),
      r[1] * (1 - y[2]) - r[2] * y[2], r[3] * (1 - y[3]) - r[4] * y[3], r[5] * (1 - y[4]) - r[6] * y[4])
  }
  r0 <- rates(-65)
  y <- c(-65, r0[1] / (r0[1] + r0[2]), r0[3] / (r0[3] + r0[4]), r0[5] / (r0[5] + r0[6]))
  dt <- 0.05
  Vs <- y[1]
  for (i in 1:60) {
    t <- (i - 1) * dt
    k1 <- f(t, y)
    k2 <- f(t + dt / 2, y + dt / 2 * k1)
    k3 <- f(t + dt / 2, y + dt / 2 * k2)
    k4 <- f(t + dt, y + dt * k3)
    y <- y + dt / 6 * (k1 + 2 * k2 + 2 * k3 + k4)
    y[2:4] <- pmin(1, pmax(0, y[2:4]))
    Vs <- c(Vs, y[1])
  }
  r <- HhModel(duration = 3, dt = dt, stim_start = 1, stim_stop = 2)
  expect_equal(r$V_mV, Vs, tolerance = 1e-12)
  expect_equal(r$I_K_uA_cm2, 36 * r$n^4 * (r$V_mV + 77), tolerance = 1e-12)
  full <- HhModel()
  expect_true(full$spiked)
  expect_equal(full$n_spikes, 1L)
  expect_false(HhModel(I_ext = 0)$spiked)
  expect_error(HhModel(dt = 2), "dt")
})

test_that("FitzHugh-Nagumo is RK4 on the two FHN equations", {
  # the stimulus is on for stim_start <= t < stim_stop (default: duration)
  f <- function(t, v, w) c(v - v^3 / 3 - w + if (t < 10) 0.5 else 0, 0.08 * (v + 0.7 - 0.8 * w))
  v <- -1.2
  w <- -0.6
  vs <- v
  for (i in 1:100) {
    t <- (i - 1) * 0.1
    k1 <- f(t, v, w)
    k2 <- f(t + 0.05, v + 0.05 * k1[1], w + 0.05 * k1[2])
    k3 <- f(t + 0.05, v + 0.05 * k2[1], w + 0.05 * k2[2])
    k4 <- f(t + 0.1, v + 0.1 * k3[1], w + 0.1 * k3[2])
    v <- v + 0.1 / 6 * (k1[1] + 2 * k2[1] + 2 * k3[1] + k4[1])
    w <- w + 0.1 / 6 * (k1[2] + 2 * k2[2] + 2 * k3[2] + k4[2])
    vs <- c(vs, v)
  }
  r <- Fhn(duration = 10, dt = 0.1)
  expect_equal(r$v, vs, tolerance = 1e-12)
  long <- Fhn(duration = 200, dt = 0.05)
  expect_gt(long$n_spikes, 2L)
  st <- long$spike_times
  expect_equal(long$period, (st[length(st)] - st[1]) / (length(st) - 1), tolerance = 1e-12)
  expect_error(Fhn(eps = 0), "eps")
  expect_error(Fhn(stim_start = 5, stim_stop = 1), "precede")
})

test_that("passive RC membrane follows the exact exponential step", {
  t <- c(0, 0.5, 1.5, 3, 7)
  r <- RcMemb(t, I_inj = 0.1, C_m = 0.2, R_m = 100, V_rest = -65)
  expect_equal(r$tau_ms, 20)
  expect_equal(r$V_mV, -65 + 10 * (1 - exp(-t / 20)), tolerance = 1e-12)
  cur <- c(0.1, 0.1, 0, 0, 0)
  r2 <- RcMemb(t, I_inj = cur)
  v <- -65
  ref <- v
  for (i in 2:5) {
    vinf <- -65 + cur[i - 1] * 100
    v <- vinf + (v - vinf) * exp(-(t[i] - t[i - 1]) / 20)
    ref <- c(ref, v)
  }
  expect_equal(r2$V_mV, ref, tolerance = 1e-12)
  expect_error(RcMemb(c(1, 0)), "non-decreasing")
  expect_error(RcMemb(t, I_inj = 1:2), "scalar")
})

test_that("bidomain cable: diffusion constant, propagation, extracellular Poisson solve", {
  r <- BiDomain(n_nodes = 30, duration_ms = 20)
  D <- (1 * 2 / 3) / 1000
  expect_equal(r$D_cm2_per_ms, D, tolerance = 1e-12)
  expect_equal(r$stability_limit_ms, 0.02^2 / (2 * D), tolerance = 1e-12)
  V <- r$Vm_mV
  n <- 30
  lap <- function(u) (c(u[2], u[-n]) - 2 * u + c(u[-1], u[n - 1])) / 0.02^2
  res <- (1 + 2) * lap(r$phi_e_mV) + 1 * lap(V)
  expect_equal(res[2:(n - 1)], rep(0, n - 2), tolerance = 1e-8 * max(abs(lap(V))))
  expect_equal(mean(r$phi_e_mV), 0, tolerance = 1e-12)
  expect_equal(r$phi_i_mV, V + r$phi_e_mV, tolerance = 1e-12)
  act <- r$activation_ms[!is.na(r$activation_ms)]
  expect_gt(length(act), 2L)
  expect_false(is.unsorted(act))
  expect_gt(r$cv_cm_per_ms, 0)
  expect_error(BiDomain(dt_ms = 1), "stability limit")
  expect_error(BiDomain(threshold_frac = 0.5), "threshold_frac")
})

test_that("coronary AR spectrum uses the Yule-Walker solution", {
  fs <- 2000
  tv <- (0:999) / fs
  x <- sin(2 * pi * 400 * tv) * exp(-3 * tv) + 0.3 * sin(2 * pi * 120 * tv) + 0.05 * cos(2 * pi * 713 * tv)
  r <- CadAcou(x, fs, order = 6)
  yw <- phys_yw(x, 6)
  expect_equal(r$ar_coeffs, yw$a, tolerance = 1e-9)
  expect_equal(r$prediction_error, yw$e, tolerance = 1e-9)
  f <- r$freq_hz
  H <- vapply(f, function(ff) 1 / Mod(1 + sum(yw$a * exp(-1i * 2 * pi * ff / fs * (1:6))))^2, 0)
  psd <- yw$e * H
  expect_equal(r$ar_psd, psd, tolerance = 1e-9)
  expect_equal(r$power_ratio, sum(psd[f >= 300 & f < 900]) / sum(psd[f >= 50 & f < 300]), tolerance = 1e-9)
  expect_equal(r$mean_freq_hz, sum(f * psd) / sum(psd), tolerance = 1e-9)
  expect_error(CadAcou(x, fs, hf_band = c(300, 1200)), "Nyquist")
  expect_error(CadAcou(x[1:10], fs, order = 6), "4\\*order")
})

test_that("coronary sound turbulence spectrum, eqs 7.135-7.136", {
  r <- CorSound(0.004, 0.3, stenosis_pct = 75, p2max = 2, freqs = c(0, 10, 100, 500))
  d <- 0.004 * sqrt(0.25)
  tau <- d / 0.3
  expect_equal(r$d_stenotic_m, d, tolerance = 1e-12)
  expect_equal(r$u_stenotic_m_s, 1.2, tolerance = 1e-12)
  expect_equal(r$psd_Pa2_per_Hz, 0.7 * tau * 2 / (1 + 0.5 * c(0, 10, 100, 500) * tau)^(10 / 3), tolerance = 1e-12)
  expect_equal(r$corner_freq_hz, 2 / tau, tolerance = 1e-12)
  expect_equal(r$reynolds_number, 1.2 * d / 3.5e-6, tolerance = 1e-12)
  expect_equal(r$reynolds_param_x, 1e-3 * 1.2 * d / 3.5e-6 * (0.004 / d)^0.75, tolerance = 1e-12)
  expect_equal(r$total_power_Pa2, sum(r$psd_Pa2_per_Hz) * 10, tolerance = 1e-12)
  expect_error(CorSound(0.004, 0.3, stenosis_pct = 100), "\\[0, 100\\)")
})

test_that("infant cry F0 track from the autocorrelation peak", {
  fs <- 8000
  tv <- (0:3999) / fs
  cry <- sin(2 * pi * 500 * tv)
  r <- InfantCry(cry, fs)
  seg <- cry[1:320]
  seg <- seg - mean(seg)
  acf <- vapply(0:40, function(k) sum(seg[1:(320 - k)] * seg[(1 + k):320]) / 320, 0)
  k <- 8 + which.max(acf[9:41]) - 1
  expect_equal(unlist(r$f0_track_hz)[1], fs / k)
  expect_equal(r$n_windows, 12L)
  expect_equal(r$voiced_fraction, 1)
  expect_equal(r$mean_f0_hz, mean(unlist(r$f0_track_hz)), tolerance = 1e-12)
  expect_true(all(r$melody == 0L))
  expect_equal(r$in_common_band_fraction, mean(unlist(r$f0_track_hz) >= 300 & unlist(r$f0_track_hz) <= 600))
  expect_error(InfantCry(cry, 1500), "twice")
  expect_error(InfantCry(cry, fs, window_ms = 5), "two periods")
})

test_that("electrogastrogram dominant frequency and band fractions", {
  fs <- 1
  x <- sin(2 * pi * 0.05 * (1:600)) + 0.3 * sin(2 * pi * 0.02 * (1:600))
  r <- EggFeat(x, fs)
  sp <- phys_psd(x, fs)
  tot <- sum(sp$power)
  expect_equal(r$normal_fraction, phys_band(sp, 0.0333, 0.0667) / tot, tolerance = 1e-9)
  expect_equal(r$brady_fraction, phys_band(sp, 0, 0.0333) / tot, tolerance = 1e-9)
  sel <- sp$freqs > 0 & sp$freqs <= 0.5
  expect_equal(r$dominant_freq_hz, sp$freqs[sel][which.max(sp$power[sel])])
  expect_identical(r$rhythm, "normogastria")
  expect_equal(r$dominant_freq_cpm, 60 * r$dominant_freq_hz)
  expect_identical(EggFeat(sin(2 * pi * 0.1 * (1:600)), fs)$rhythm, "tachygastria")
  expect_error(EggFeat(x[1:30], fs), "too short")
})

test_that("ENG compound action potential sums fibre waveforms", {
  t <- seq(0, 4, by = 0.01)
  r <- EngCap(t, distance_m = 0.1, n_fibers = 5, cv_range = c(50, 70), amp_range = c(1, 2), width_ms = 0.3)
  cv <- seq(50, 70, length.out = 5)
  amp <- seq(1, 2, length.out = 5)
  lat <- 100 / cv
  cap <- vapply(t, function(ti) {
    u <- (ti - lat) / 0.3
    k <- u >= 0 & u <= 30
    sum(amp[k] * u[k] * (2 - u[k]) * exp(-u[k]))
  }, 0)
  expect_equal(r$cap_uV, cap, tolerance = 1e-12)
  expect_equal(r$latencies_ms, lat, tolerance = 1e-12)
  expect_equal(r$peak_latency_ms, t[which.max(abs(cap))])
  expect_equal(r$cv_from_peak_m_s, 100 / r$peak_latency_ms, tolerance = 1e-12)
  expect_error(EngCap(seq(0, 0.5, by = 0.1)), "identically zero")
  expect_error(EngCap(t, cv_range = c(70, 50)), "cv_range")
})

test_that("seizure detector flags slow-band epochs against baseline", {
  fs <- 100
  tv <- (0:99) / fs
  alpha <- sin(2 * pi * 10 * tv) + 0.5 * sin(2 * pi * 20 * tv)
  slow <- 3 * sin(2 * pi * 3 * tv) + 0.2 * sin(2 * pi * 20 * tv)
  eeg <- c(rep(alpha, 4), rep(slow, 2), rep(alpha, 2))
  r <- SeizDet(eeg, fs)
  sf <- vapply(1:8, function(e) {
    sp <- phys_psd(eeg[((e - 1) * 100 + 1):(e * 100)], fs)
    (phys_band(sp, 0.5, 4) + phys_band(sp, 4, 8)) / sum(sp$power)
  }, 0)
  expect_equal(vapply(r$epochs, function(e) e$slow_fraction, 0), sf, tolerance = 1e-9)
  expect_equal(r$baseline_slow_fraction, mean(sf[1:2]), tolerance = 1e-9)
  expect_equal(vapply(r$epochs, function(e) e$flagged, TRUE), sf > 2 * mean(sf[1:2]))
  expect_equal(r$seizure_intervals_s, list(c(4, 6)))
  expect_error(SeizDet(eeg, 20), "30 Hz")
  expect_error(SeizDet(eeg, fs, ratio_threshold = 1), "exceed 1")
})

test_that("ERP components: baseline-corrected extrema in their windows", {
  fs <- 1000
  t <- seq(-100, 599) / 1000
  erp <- 2 + (-4) * exp(-((t - 0.1) / 0.02)^2) + 6 * exp(-((t - 0.32) / 0.04)^2)
  r <- ErpFeat(erp, fs, t0 = 100)
  expect_equal(r$baseline_uV, mean(erp[1:100]), tolerance = 1e-12)
  ts <- (0:699) - 100
  y <- erp - mean(erp[1:100])
  w <- which(ts >= 250 & ts <= 500)
  expect_equal(r$components$P300$latency_ms, ts[w[which.max(y[w])]])
  expect_equal(r$components$P300$amplitude_uV, max(y[w]), tolerance = 1e-12)
  w1 <- which(ts >= 50 & ts <= 150)
  expect_equal(r$components$N100$amplitude_uV, min(y[w1]), tolerance = 1e-12)
  expect_equal(r$peak_to_peak_uV, max(y) - min(y), tolerance = 1e-12)
  cc <- ErpFeat(erp, fs, t0 = 100, components = list(X = list(900, 950, 1L)))
  expect_false(cc$components$X$found)
  expect_error(ErpFeat(erp, fs, components = list(X = list(10, 5, 1L))), "empty window")
})

test_that("ERD/ERS compares per-sample band power", {
  fs <- 100
  tv <- (0:599) / fs
  eeg <- ifelse(tv < 3, 2, 0.5) * sin(2 * pi * 10 * tv) + 0.1 * sin(2 * pi * 30 * tv)
  r <- ErdErs(eeg, fs, c(0, 2), c(3.5, 5.5))
  R <- phys_band(phys_psd(eeg[1:200], fs), 8, 13) / 200
  A <- phys_band(phys_psd(eeg[351:550], fs), 8, 13) / 200
  expect_equal(r$ref_power, R, tolerance = 1e-9)
  expect_equal(r$erd_percent, 100 * (A - R) / R, tolerance = 1e-9)
  expect_identical(r$event, "ERD")
  expect_error(ErdErs(eeg, fs, c(0, 2), c(5, 7)), "outside")
  expect_error(ErdErs(eeg, fs, c(2, 1), c(3, 4)), "end > start")
})

test_that("CAD spectral features, murmur analysis and murmur detection", {
  fs <- 2000
  tv <- (0:1999) / fs
  x <- sin(2 * pi * 100 * tv) + 0.4 * sin(2 * pi * 350 * tv)
  sp <- phys_psd(x, fs)
  tot <- sum(sp$power)
  r <- CadSpec(x, fs)
  expect_equal(r$total_power, tot, tolerance = 1e-9)
  expect_equal(r$dominant_freq_hz, sp$freqs[which.max(sp$power)])
  fr <- vapply(r$band_power_fraction, `[`, 0, 3)
  expect_equal(fr, c(phys_band(sp, 0, 100), phys_band(sp, 100, 300), phys_band(sp, 300, 600), phys_band(sp, 600, Inf)) / tot, tolerance = 1e-9)
  expect_error(CadSpec(x, fs, bands = list(c(10, 5))), "hi > lo")
  m <- MurmSpec(x, fs)
  mag <- sqrt(sp$power)
  expect_equal(m$pa_over_ca, sum(mag[sp$freqs >= 75 & sp$freqs < 150]) / sum(mag[sp$freqs >= 25 & sp$freqs < 75]), tolerance = 1e-9)
  expect_error(MurmSpec(x, fs, 50, 40, 100), "f1 < f2 < f3")
  d <- MurmDet(x, fs, threshold = 0.1)
  frac <- phys_band(sp, 150, 600) / tot
  expect_equal(d$hf_power_fraction, frac, tolerance = 1e-9)
  expect_equal(d$murmur_present, frac >= 0.1)
  expect_equal(d$margin, frac - 0.1, tolerance = 1e-9)
  expect_error(MurmDet(x, fs, threshold = 1), "threshold")
})

test_that("VAG muscle artifact cancellation is the documented LMS", {
  n <- 200
  ref <- sin(2 * pi * (1:n) / 13) + 0.5 * cos(2 * pi * (1:n) / 7)
  vag <- 0.6 * ref + 0.2 * sin(2 * pi * (1:n) / 29)
  lms <- function(adaptive, mu) {
    w <- numeric(4)
    xb <- mean(ref^2)
    out <- numeric(n)
    for (i in 1:n) {
      r <- vapply(0:3, function(k) if (i - k >= 1) ref[i - k] else 0, 0)
      e <- vag[i] - sum(w * r)
      if (adaptive) {
        xb <- 0.02 * r[1]^2 + 0.98 * xb
        st <- mu / (4 * xb)
      } else {
        st <- mu
      }
      w <- w + 2 * st * e * r
      out[i] <- e
    }
    list(out = out, w = w)
  }
  a <- VagClean(vag, ref, 500, n_taps = 4, mu = 0.05)
  ra <- lms(TRUE, 0.05)
  expect_equal(a$cleaned, ra$out, tolerance = 1e-12)
  expect_equal(a$weights, ra$w, tolerance = 1e-12)
  expect_equal(a$artifact_reduction_db, 20 * log10(sqrt(mean(vag^2)) / sqrt(mean(ra$out^2))), tolerance = 1e-12)
  b <- VagClean(vag, ref, 500, n_taps = 4, mu = 0.01, adaptive_mu = FALSE)
  expect_equal(b$cleaned, lms(FALSE, 0.01)$out, tolerance = 1e-12)
  expect_error(VagClean(vag, ref, 500, n_taps = 4, mu = 1, adaptive_mu = FALSE), "stability limit")
  expect_error(VagClean(vag, ref[-1], 500), "same length")
})

test_that("MUAP sums delayed Hermite-type fibre potentials", {
  t <- seq(0, 20, by = 0.1)
  r <- MuapModel(t, n_fibers = 5, conduction_vel = 4, spread_mm = 2, amp_uV = 8, width_ms = 1)
  d <- -0.25 + 0.5 * (0:4) / 4
  ref <- vapply(t, function(ti) {
    u <- (ti - 10 - d)
    k <- abs(u) <= 8
    8 * sum((u[k]^2 - 1) * exp(-u[k]^2 / 2))
  }, 0)
  expect_equal(r$muap_uV, ref, tolerance = 1e-12)
  expect_equal(r$delays_ms, d, tolerance = 1e-12)
  expect_equal(r$n_phases_observed, 3L)
  b <- MuapModel(t, n_fibers = 1, phases = 2)
  u <- t - 10
  expect_equal(b$muap_uV, ifelse(abs(u) <= 8, -8 * u * exp(-u^2 / 2), 0), tolerance = 1e-12)
  expect_equal(b$n_phases_observed, 2L)
  expect_error(MuapModel(t, phases = 4), "phases")
})

test_that("OAE band analysis with and without a noise floor", {
  fs <- 16000
  tv <- (0:2047) / fs
  x <- sin(2 * pi * 2000 * tv) * exp(-5 * tv)
  nz <- 0.01 * sin(2 * pi * 1100 * tv) + 0.01 * cos(2 * pi * 3700 * tv) + 0.01 * sin(2 * pi * 2050 * tv)
  r <- OaeFeat(x, fs, noise_floor = nz)
  sp <- phys_psd(x, fs)
  np <- phys_psd(nz, fs)
  b3 <- r$band_analysis[[3]]
  expect_equal(c(b3$lo_hz, b3$hi_hz), 2000 * c(2^-0.25, 2^0.25), tolerance = 1e-12)
  expect_equal(b3$power, phys_band(sp, b3$lo_hz, b3$hi_hz), tolerance = 1e-9)
  expect_equal(b3$snr_db, 10 * log10(b3$power / phys_band(np, b3$lo_hz, b3$hi_hz)), tolerance = 1e-9)
  expect_true(r$emission_detected)
  expect_equal(r$rms, sqrt(mean(x^2)), tolerance = 1e-12)
  expect_null(OaeFeat(x, fs)$emission_detected)
  expect_error(OaeFeat(x, 1000), "too low")
  expect_error(OaeFeat(x, fs, noise_floor = nz[-1]), "same length")
})

test_that("Parkinson monitor: tremor fractions, turns, gait rate", {
  fs <- 100
  tv <- (0:999) / fs
  eeg <- sin(2 * pi * 20 * tv) + 0.5 * sin(2 * pi * 6 * tv)
  emg <- sin(2 * pi * 5 * tv) + 0.2 * sin(2 * pi * 23 * tv)
  gait <- sin(2 * pi * 1 * tv) + 0.1 * sin(2 * pi * 5 * tv)
  r <- PdMonitor(eeg, emg, gait, fs)
  me <- phys_psd(emg, fs)
  expect_equal(r$emg_tremor_fraction, phys_band(me, 3, 7) / sum(me$power), tolerance = 1e-9)
  inb <- me$freqs >= 3 & me$freqs <= 7
  expect_equal(r$emg_tremor_freq_hz, me$freqs[inb][which.max(me$power[inb])])
  turns <- sum(diff(emg)[-999] * diff(emg)[-1] < 0)
  expect_equal(r$emg_turns_per_second, turns * fs / 1000, tolerance = 1e-12)
  ee <- phys_psd(eeg, fs)
  expect_equal(r$eeg_beta_fraction, phys_band(ee, 13.0001, 30) / sum(ee$power), tolerance = 1e-9)
  expect_true(r$tremor_present)
  expect_equal(r$gait_rate_hz, 1, tolerance = fs / 1024)
  expect_error(PdMonitor(eeg, emg, gait, 50), "60 Hz")
})

test_that("PCG-EEG coherence is segment-averaged magnitude coherence", {
  fs <- 200
  n <- 1024
  k <- 1:n
  pcg <- sin(2 * pi * 25 * k / fs) + 0.5 * sin(2 * pi * 40 * k / fs + (k %/% 128))
  eeg <- 0.8 * sin(2 * pi * 25 * k / fs - 0.4) + cos(2 * pi * 40 * k / fs + 2 * (k %/% 128))
  r <- PcgEeg(pcg, eeg, fs, n_segments = 8)
  w <- 128
  han <- 0.5 - 0.5 * cos(2 * pi * (0:(w - 1)) / (w - 1))
  Sxx <- Syy <- 0
  Sxy <- 0i
  for (s in 1:8) {
    a <- pcg[((s - 1) * w + 1):(s * w)]
    b <- eeg[((s - 1) * w + 1):(s * w)]
    A <- fft((a - mean(a)) * han)[1:65]
    B <- fft((b - mean(b)) * han)[1:65]
    Sxx <- Sxx + Mod(A)^2
    Syy <- Syy + Mod(B)^2
    Sxy <- Sxy + A * Conj(B)
  }
  c2 <- pmin(1, Mod(Sxy)^2 / (Sxx * Syy))
  sel <- 2:65
  expect_equal(r$coherence_sq[sel], c2[sel], tolerance = 1e-9)
  expect_equal(r$significance_level, 1 - 0.05^(1 / 7), tolerance = 1e-12)
  expect_equal(r$peak_freq_hz, 25, tolerance = 1e-12)
  expect_error(PcgEeg(pcg, eeg, fs, n_segments = 1), "at least 2")
})

test_that("PSG staging applies the documented band rules", {
  fs <- 100
  tv <- (0:2999) / fs
  deltaep <- 3 * sin(2 * pi * 2 * tv) + 0.2 * sin(2 * pi * 20 * tv)
  thetaep <- sin(2 * pi * 6 * tv) + 0.3 * sin(2 * pi * 10 * tv)
  eeg <- c(deltaep, thetaep, thetaep)
  eog <- rep(0.1 * sin(2 * pi * 1 * tv), 3)
  emg <- rep(0.3 * sin(2 * pi * 25 * tv), 3)
  r <- PsgStage(eeg, eog, emg, fs)
  sp1 <- phys_psd(deltaep, fs)
  expect_equal(r$epochs[[1]]$delta_fraction, phys_band(sp1, 0.5, 4) / sum(sp1$power), tolerance = 1e-9)
  expect_equal(r$stage_sequence, c("N3", "N2", "N2"))
  expect_equal(r$total_sleep_time_min, 1.5, tolerance = 1e-12)
  expect_equal(r$sleep_efficiency, 1)
  expect_error(PsgStage(eeg, eog[-1], emg, fs), "same length")
  expect_error(PsgStage(eeg[1:100], eog[1:100], emg[1:100], fs), "shorter than one epoch")
})

test_that("inter-event interval statistics", {
  ts <- cumsum(c(0, 0.5, 0.52, 0.48, 0.55, 0.5, 0.47, 0.53))
  r <- IeiStats(ts, n_bins = 4)
  ipi <- diff(ts)
  expect_equal(r$mean_ipi_s, mean(ipi), tolerance = 1e-12)
  expect_equal(r$sd_ipi_s, sd(ipi), tolerance = 1e-12)
  expect_equal(r$cv_rate, sd(1 / ipi) / mean(1 / ipi), tolerance = 1e-12)
  expect_equal(r$median_ipi_s, median(ipi), tolerance = 1e-12)
  expect_equal(r$event_rate_pps, 8 / (ts[8] - ts[1]), tolerance = 1e-12)
  w <- (max(ipi) - min(ipi)) / 4
  cnt <- tabulate(pmin(floor((ipi - min(ipi)) / w), 3) + 1, 4)
  expect_equal(vapply(r$ipi_histogram, `[`, 0, 2), cnt)
  expect_identical(r$regularity, "near-periodic")
  expect_error(IeiStats(c(0, 1, 1)), "strictly increasing")
  expect_error(IeiStats(1:2), "at least 3")
})

test_that("prosthetic valve AR peaks and speech features", {
  fs <- 2000
  tv <- (0:999) / fs
  pcg <- sin(2 * pi * 80 * tv) + 0.5 * sin(2 * pi * 220 * tv) + 0.05 * cos(2 * pi * 613 * tv)
  r <- ValvePcg(pcg, fs, n_peaks = 2)
  yw <- phys_yw(pcg, 8)
  expect_equal(r$ar_coeffs, yw$a, tolerance = 1e-9)
  expect_equal(r$order, 8L)
  pf <- vapply(r$peaks, function(p) p$freq_hz, 0)
  expect_true(all(abs(sort(pf) - c(80, 220)) < 10))
  expect_error(ValvePcg(pcg[1:20], fs), "at least 32")
  fs2 <- 8000
  tv2 <- (0:2399) / fs2
  sp <- sin(2 * pi * 125 * tv2) + 0.5 * sin(2 * pi * 750 * tv2)
  s <- SpeechFeat(sp, fs2)
  expect_true(s$voiced)
  expect_equal(s$f0_hz, 125, tolerance = 1e-12)
  expect_equal(s$zero_crossing_rate, sum(diff(sp < 0) != 0) * fs2 / 2400, tolerance = 1e-12)
  expect_equal(s$ar_coeffs, phys_yw(sp, 10)$a, tolerance = 1e-9)
  expect_error(SpeechFeat(sp, fs2, order = 3), "at least 4")
})

test_that("respiration features: rate, Ti/Te, depth, both signal types", {
  fs <- 25
  tv <- (0:749) / fs
  flow <- sin(2 * pi * 0.25 * tv + 0.3)
  r <- RespFeat(flow, fs)
  expect_equal(r$rate_breaths_per_min, 15, tolerance = 0.02)
  expect_equal(r$mean_ti_s, r$mean_te_s, tolerance = 2 / fs)
  b1 <- r$breaths[[1]]
  i0 <- b1$t_start_s * fs + 1
  iend <- i0 + b1$ti_s * fs
  y <- flow - mean(flow)
  expect_equal(b1$depth, sum(y[i0:(iend - 1)]) / fs, tolerance = 1e-12)
  v <- RespFeat(-cos(2 * pi * 0.25 * tv), fs, signal_type = "volume")
  expect_equal(v$rate_breaths_per_min, 15, tolerance = 0.02)
  expect_equal(v$depth, 2, tolerance = 0.01)
  expect_error(RespFeat(flow, fs, signal_type = "pressure"), "flow")
})

test_that("respiratory sound airway model, eqs 7.122-7.129", {
  r <- RespSound(length_m = 0.12, radius_m = 0.008, freqs = c(50, 200, 800))
  A <- pi * 0.008^2
  S <- 2 * pi * 0.008
  La <- 1.2 * 0.12 / A
  Ca <- A * 0.12 / (101325 * 1.4)
  w <- 2 * pi * c(50, 200, 800)
  Ra <- 0.12 * S / A^2 * sqrt(w * 1.2 * 1.8e-5 / 2)
  Ga <- S * 0.12 / (1.2 * 343^2) * 0.4 * sqrt(0.026 * w / (2 * 1005 * 1.2))
  H <- 1 / (1 + complex(real = Ra, imaginary = w * La) * complex(real = Ga, imaginary = w * Ca))
  expect_equal(r$transfer_mag, Mod(H), tolerance = 1e-12)
  expect_equal(r$resonance_hz, 1 / (2 * pi * sqrt(La * Ca)), tolerance = 1e-12)
  expect_equal(r$Ra_Pa_s_per_m3, Ra, tolerance = 1e-12)
  expect_error(RespSound(eta = 1), "eta")
})

test_that("sleep apnea epochs: desaturation, snoring and the index", {
  fs <- 100
  n <- fs * 180
  ecg <- numeric(n)
  for (b in seq(0.4, 179, by = 0.8)) ecg[round(b * fs) + 0:4] <- c(0.3, 1.2, -0.4, 0.1, 0)
  spo2 <- rep(97, n)
  spo2[(60 * fs):(75 * fs)] <- 91
  snore <- rep(0.1, n) * sin(1:n)
  snore[(60 * fs):(119 * fs)] <- 5 * sin((60 * fs):(119 * fs))
  r <- ApneaDet(ecg, spo2, snore, fs)
  expect_equal(r$n_epochs, 3L)
  expect_equal(vapply(r$epochs, function(e) e$desat_depth_pct, 0), c(6, 6, 0))
  hr <- r$epochs[[1]]$mean_hr_bpm
  expect_equal(hr, 75, tolerance = 0.02)
  expect_equal(r$epochs[[2]]$score >= 2, r$epochs[[2]]$epoch_flagged)
  expect_equal(r$events_per_hour, r$n_flagged / (180 / 3600), tolerance = 1e-12)
  expect_error(ApneaDet(ecg, spo2, snore, 50), "100 Hz")
  expect_error(ApneaDet(ecg, spo2 + 10, snore, fs), "0-100")
})

test_that("VAG statistics and the linear discriminant", {
  fs <- 1000
  k <- 1:1000
  vag <- sin(k / 7) * (1 + k / 1000) + 0.3 * cos(k / 3)
  r <- VagFeat(vag, fs)
  m <- mean(vag)
  s2 <- mean((vag - m)^2)
  expect_equal(r$variance, s2, tolerance = 1e-12)
  expect_equal(r$kurtosis, mean((vag - m)^4) / s2^2, tolerance = 1e-12)
  expect_equal(r$skewness, mean((vag - m)^3) / s2^1.5, tolerance = 1e-12)
  d1 <- diff(vag)
  d2 <- diff(d1)
  vr <- function(v) mean((v - mean(v))^2)
  expect_equal(r$mobility, sqrt(vr(d1) / vr(vag)), tolerance = 1e-12)
  expect_equal(r$form_factor, sqrt(vr(d2) / vr(d1)) / sqrt(vr(d1) / vr(vag)), tolerance = 1e-12)
  expect_equal(r$turns_count, sum(d1[-999] * d1[-1] < 0))
  bins <- pmin(floor((vag - min(vag)) / (max(vag) - min(vag)) * 64), 63)
  p <- tabulate(bins + 1, 64) / 1000
  expect_equal(r$entropy_bits, -sum(p[p > 0] * log2(p[p > 0])), tolerance = 1e-12)
  ms <- vapply(1:8, function(i) mean(vag[((i - 1) * 125 + 1):(i * 125)]^2), 0)
  expect_equal(r$var_of_segment_ms, vr(ms), tolerance = 1e-12)
  expect_error(VagFeat(vag, fs, n_segments = 1), "at least 2")
  w <- c(0.5, -0.2, 0.1, 0.03, -0.4)
  kn <- VagKnee(vag, fs, weights = w, bias = 0.7)
  f <- c(r$form_factor, r$kurtosis - 3, log(r$var_of_segment_ms), r$turns_per_second, r$entropy_bits)
  expect_equal(kn$discriminant, 0.7 + sum(w * f), tolerance = 1e-12)
  expect_true(kn$trained)
  d0 <- VagKnee(vag, fs)
  b0 <- -(1.2 + 0.2 * log(1e-4) + 0.01 * 200 + 0.5 * 5)
  expect_equal(d0$discriminant, b0 + sum(c(1, 0.5, 0.2, 0.01, 0.5) * f), tolerance = 1e-12)
  expect_error(VagKnee(vag, fs, bias = 1), "together with weights")
})

test_that("complex log of a product and of a rational X(z)", {
  X <- complex(real = c(1, -2, 0.5, 3), imaginary = c(0.5, 1, -1.5, -0.2))
  H <- complex(real = c(-1, 0.3, 2, 1), imaginary = c(1, -0.7, 0.1, 2))
  r <- CLogProd(X, H)
  expect_equal(r$log_Y_real, log(Mod(X)) + log(Mod(H)), tolerance = 1e-12)
  expect_equal(r$Y_real + 1i * r$Y_imag, X * H, tolerance = 1e-12)
  expect_lt(r$max_abs_error, 1e-12)
  expect_equal(r$omega, 0:3)
  expect_error(CLogProd(c(1, 0), c(1, 1)), "non-zero")
  z <- exp(1i * c(0.3, 1.1, 2.5))
  a <- 0.4 + 0.2i
  b <- 0.3 - 0.1i
  cc <- -0.5
  d <- 0.2i
  p <- CLogPz(z, A = 2, r = 1, a_k = a, b_k = b, c_k = cc, d_k = d)
  Xz <- 2 * z * (1 - a / z) * (1 - b * z) / ((1 - cc / z) * (1 - d * z))
  expect_equal(p$X_real + 1i * p$X_imag, Xz, tolerance = 1e-12)
  expect_equal(p$xhat_real, log(Mod(Xz)), tolerance = 1e-12)
  expect_lt(p$max_abs_error, 1e-12)
  expect_error(CLogPz(z, a_k = 1.5), "modulus < 1")
  expect_error(CLogPz(z, a_k = a, M_I = 2), "declared count")
})

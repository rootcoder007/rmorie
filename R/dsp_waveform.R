# SPDX-License-Identifier: AGPL-3.0-or-later

# Waveform analysis primitives from Rangayyan & Krishnan (2015),
# Biomedical Signal Analysis, 2nd ed., Chapter 5.
#
# Ports src/morie/_waveform.py. Public functions carry the
# `morie_dsp_` prefix.

#' Root mean square
#'
#' `sqrt(mean(x^2))`.
#'
#' @param x Numeric vector.
#' @return Scalar RMS.
#' @references Rangayyan & Krishnan (2015), Ch. 5, sec. 5.2.
#' @export
morie_dsp_rms <- function(x) {
  sqrt(mean(x^2))
}

#' Form factor (RMS / mean-absolute)
#'
#' `1.11` for a pure sine; deviations diagnose waveshape changes.
#'
#' @param x Numeric vector.
#' @return Scalar.
#' @references Rangayyan & Krishnan (2015), Ch. 5, sec. 5.2.
#' @export
morie_dsp_form_factor <- function(x) {
  m <- mean(abs(x))
  if (m == 0) return(0)
  morie_dsp_rms(x) / m
}

#' Crest factor (peak / RMS)
#'
#' `sqrt(2)` for a pure sine; large values indicate spiky waveforms.
#'
#' @param x Numeric vector.
#' @return Scalar.
#' @references Rangayyan & Krishnan (2015), Ch. 5, sec. 5.2.
#' @export
morie_dsp_crest_factor <- function(x) {
  r <- morie_dsp_rms(x)
  if (r == 0) return(0)
  max(abs(x)) / r
}

#' Shape factor (mean-absolute / mean-sqrt-absolute-squared)
#'
#' Dimensionless waveshape descriptor: `E|x| / (E sqrt|x|)^2`.
#'
#' @param x Numeric vector.
#' @return Scalar.
#' @references Rangayyan & Krishnan (2015), Ch. 5, sec. 5.2.
#' @export
morie_dsp_shape_factor <- function(x) {
  num <- mean(abs(x))
  den <- mean(sqrt(abs(x)))^2
  if (den == 0) return(0)
  num / den
}

#' Waveform length (total variation)
#'
#' Sum of absolute first differences. Standard sEMG descriptor.
#'
#' @param x Numeric vector.
#' @return Scalar.
#' @references Rangayyan & Krishnan (2015), Ch. 5, sec. 5.3;
#'   Hudgins et al. (1993).
#' @export
morie_dsp_waveform_length <- function(x) {
  sum(abs(diff(x)))
}

#' Per-sample (normalised) waveform length
#'
#' `morie_dsp_waveform_length(x) / length(x)`.
#'
#' @param x Numeric vector.
#' @return Scalar.
#' @references Rangayyan & Krishnan (2015), Ch. 5.
#' @export
morie_dsp_waveform_length_norm <- function(x) {
  morie_dsp_waveform_length(x) / length(x)
}

#' Willison turns count
#'
#' Counts adjacent sign changes in `diff(x)` whose absolute slope
#' difference exceeds `threshold`. Used as a fatigue/load proxy in
#' sEMG analysis.
#'
#' @param x Numeric vector.
#' @param threshold Slope-difference threshold. Default 0.
#' @return Integer count.
#' @references Rangayyan & Krishnan (2015), Ch. 5, sec. 5.4;
#'   Willison (1964).
#' @export
morie_dsp_turns_count <- function(x, threshold = 0) {
  d <- diff(x)
  if (length(d) < 2L) return(0L)
  prev <- d[-length(d)]
  nxt <- d[-1L]
  as.integer(sum(prev * nxt < 0 & abs(prev - nxt) > threshold))
}

#' Slope sign changes
#'
#' Sign changes in `diff(x)` where the absolute next slope exceeds
#' `threshold`. Hudgins TD feature.
#'
#' @param x Numeric vector.
#' @param threshold Magnitude threshold. Default 0.
#' @return Integer count.
#' @references Rangayyan & Krishnan (2015), Ch. 5, sec. 5.4.
#' @export
morie_dsp_slope_sign_changes <- function(x, threshold = 0) {
  d <- diff(x)
  if (length(d) < 2L) return(0L)
  prev <- d[-length(d)]
  nxt <- d[-1L]
  as.integer(sum(prev * nxt < 0 & abs(nxt) > threshold))
}

#' Willison amplitude
#'
#' Count of `|diff(x)| > threshold` (defaults to `sd(x)`).
#'
#' @param x Numeric vector.
#' @param threshold Optional threshold (default `sd(x)`).
#' @return Integer count.
#' @references Rangayyan & Krishnan (2015), Ch. 5, sec. 5.4.
#' @export
morie_dsp_willison_amplitude <- function(x, threshold = NULL) {
  if (is.null(threshold)) threshold <- stats::sd(x)
  as.integer(sum(abs(diff(x)) > threshold))
}

#' Myopulse percentage rate
#'
#' Fraction of samples with `|x| > threshold` (defaults to `2 * sd(x)`).
#'
#' @param x Numeric vector.
#' @param threshold Optional threshold.
#' @return Scalar in `[0, 1]`.
#' @references Rangayyan & Krishnan (2015), Ch. 5, sec. 5.4.
#' @export
morie_dsp_myopulse_rate <- function(x, threshold = NULL) {
  if (is.null(threshold)) threshold <- 2 * stats::sd(x)
  mean(abs(x) > threshold)
}

#' Hjorth activity (variance)
#'
#' First Hjorth parameter: signal variance.
#'
#' @param x Numeric vector.
#' @return Scalar.
#' @references Rangayyan & Krishnan (2015), Ch. 5, sec. 5.5;
#'   Hjorth (1970).
#' @export
morie_dsp_hjorth_activity <- function(x) {
  stats::var(x)
}

#' Hjorth mobility
#'
#' `sqrt(var(diff(x)) / var(x))`; proportional to mean frequency.
#'
#' @param x Numeric vector.
#' @return Scalar.
#' @references Rangayyan & Krishnan (2015), Ch. 5, sec. 5.5.
#' @export
morie_dsp_hjorth_mobility <- function(x) {
  a <- stats::var(x)
  if (a == 0) return(0)
  sqrt(stats::var(diff(x)) / a)
}

#' Hjorth complexity
#'
#' `mobility(diff(x)) / mobility(x)`; bandwidth-like descriptor (1
#' for a pure sinusoid).
#'
#' @param x Numeric vector.
#' @return Scalar.
#' @references Rangayyan & Krishnan (2015), Ch. 5, sec. 5.5.
#' @export
morie_dsp_hjorth_complexity <- function(x) {
  m <- morie_dsp_hjorth_mobility(x)
  if (m == 0) return(0)
  morie_dsp_hjorth_mobility(diff(x)) / m
}

#' All three Hjorth parameters
#'
#' @param x Numeric vector.
#' @return Named list: `activity`, `mobility`, `complexity`.
#' @references Rangayyan & Krishnan (2015), Ch. 5, sec. 5.5;
#'   Hjorth (1970).
#' @export
morie_dsp_hjorth <- function(x) {
  list(
    activity   = morie_dsp_hjorth_activity(x),
    mobility   = morie_dsp_hjorth_mobility(x),
    complexity = morie_dsp_hjorth_complexity(x)
  )
}

#' Integrated EMG (sum of absolute values)
#'
#' @param x Numeric vector.
#' @return Scalar.
#' @references Rangayyan & Krishnan (2015), Ch. 5, sec. 5.3.
#' @export
morie_dsp_integrated_emg <- function(x) {
  sum(abs(x))
}

#' Mean absolute value
#'
#' @param x Numeric vector.
#' @return Scalar.
#' @references Rangayyan & Krishnan (2015), Ch. 5, sec. 5.3.
#' @export
morie_dsp_mean_abs <- function(x) {
  mean(abs(x))
}

#' Variance ratio (x vs. y)
#'
#' `var(x) / var(y)`; `Inf` if `var(y) == 0`.
#'
#' @param x Numeric vector.
#' @param y Numeric vector.
#' @return Scalar.
#' @references Rangayyan & Krishnan (2015), Ch. 5.
#' @export
morie_dsp_variance_ratio <- function(x, y) {
  v <- stats::var(y)
  if (v == 0) return(Inf)
  stats::var(x) / v
}

#' Signal arc length
#'
#' Polygonal length: `sum(sqrt(1 + diff(x)^2))`. Curve length under
#' unit time-step.
#'
#' @param x Numeric vector.
#' @return Scalar.
#' @references Rangayyan & Krishnan (2015), Ch. 5, sec. 5.3.
#' @export
morie_dsp_arc_length <- function(x) {
  sum(sqrt(1 + diff(x)^2))
}

#' Centroidal time
#'
#' Time-domain centre of energy: `sum(t * x^2) / sum(x^2)`.
#'
#' @param x Numeric vector.
#' @param fs Sampling frequency (Hz). Default 1.
#' @return Scalar (seconds).
#' @references Rangayyan & Krishnan (2015), Ch. 5.
#' @export
morie_dsp_centroidal_time <- function(x, fs = 1) {
  t <- (seq_along(x) - 1L) / fs
  e <- x^2
  total <- sum(e)
  if (total == 0) return(0)
  sum(t * e) / total
}

#' Amplitude histogram features
#'
#' Equal-width histogram of `x` with `n_bins` bins. Returns counts,
#' bin centres, probabilities, and edges (parallel to numpy.histogram).
#'
#' @param x Numeric vector.
#' @param n_bins Number of bins. Default 50.
#' @return List with `counts`, `centers`, `probabilities`, `edges`.
#' @references Rangayyan & Krishnan (2015), Ch. 5, sec. 5.6.
#' @export
morie_dsp_amplitude_histogram <- function(x, n_bins = 50L) {
  rng <- range(x)
  edges <- seq(rng[1], rng[2], length.out = n_bins + 1L)
  h <- graphics::hist(x, breaks = edges, plot = FALSE,
                      include.lowest = TRUE, right = TRUE)
  counts <- as.integer(h$counts)
  centers <- (edges[-1L] + edges[-(n_bins + 1L)]) / 2
  total <- sum(counts)
  probs <- if (total > 0) counts / total else rep(0, n_bins)
  list(counts = counts, centers = centers,
       probabilities = probs, edges = edges)
}

#' Shannon entropy from amplitude histogram
#'
#' `-sum(p log2 p)` over the histogram probabilities.
#'
#' @inheritParams morie_dsp_amplitude_histogram
#' @return Scalar (bits).
#' @references Rangayyan & Krishnan (2015), Ch. 5, sec. 5.6.
#' @export
morie_dsp_entropy_histogram <- function(x, n_bins = 50L) {
  p <- morie_dsp_amplitude_histogram(x, n_bins)$probabilities
  p <- p[p > 0]
  -sum(p * log2(p))
}

#' Higuchi fractal dimension
#'
#' Slope of `log(L(k))` vs. `log(1/k)` over `k = 1..kmax` curve-length
#' scales. Returns a value in approximately ``[1, 2]`` for real signals.
#'
#' @param x Numeric vector.
#' @param kmax Maximum scale. Default 10.
#' @return Scalar fractal dimension.
#' @references Rangayyan & Krishnan (2015), Ch. 5, sec. 5.7;
#'   Higuchi (1988).
#' @export
morie_dsp_higuchi_fd <- function(x, kmax = 10L) {
  x <- as.numeric(x)
  n <- length(x)
  lk <- numeric(kmax)
  for (k in seq_len(kmax)) {
    lm_sum <- 0
    for (m in seq_len(k)) {
      n_pts <- (n - m) %/% k  # Higuchi (1988) M = floor((N-m)/k)
      if (n_pts < 2L) next
      # Need M+1 indices to get M differences (matches Higuchi eq 1 exactly).
      # Earlier `seq(0, n_pts-1)` gave only M-1 differences.
      idx <- seq.int(0L, n_pts) * k + m
      idx <- idx[idx <= n]
      seg <- x[idx]
      lm_sum <- lm_sum + sum(abs(diff(seg))) * (n - 1) / (n_pts * k)
    }
    lk[k] <- lm_sum / k
  }
  valid <- lk > 0
  if (sum(valid) < 2L) return(1)
  log_k <- log(1 / seq_len(kmax))
  log_l <- log(lk + 1e-15)
  fit <- stats::lm(log_l[valid] ~ log_k[valid])
  unname(stats::coef(fit)[2L])
}

#' Katz / box-counting fractal dimension
#'
#' Coarse box-counting on the amplitude-normalised signal across
#' `n_scales` log-spaced box widths; returns the slope of
#' `log N(s)` vs. `log(1/s)`.
#'
#' @param x Numeric vector.
#' @param n_scales Number of box sizes. Default 10.
#' @return Scalar fractal dimension.
#' @references Rangayyan & Krishnan (2015), Ch. 5, sec. 5.7;
#'   Katz (1988).
#' @export
morie_dsp_katz_fd <- function(x, n_scales = 10L) {
  x_norm <- x - min(x)
  rng <- max(x_norm)
  if (rng == 0) return(0)
  x_norm <- x_norm / rng
  n <- length(x_norm)
  scales <- unique(pmin(pmax(
    as.integer(10^seq(0, log10(n / 2), length.out = n_scales)),
    1L), n))
  counts <- integer(length(scales))
  for (j in seq_along(scales)) {
    s <- scales[j]
    n_boxes <- 0L
    starts <- seq.int(1L, n, by = s)
    for (i in starts) {
      seg <- x_norm[i:min(i + s - 1L, n)]
      n_boxes <- n_boxes +
        as.integer(ceiling((max(seg) - min(seg)) * n / s)) + 1L
    }
    counts[j] <- n_boxes
  }
  log_s <- log(1 / scales)
  log_n <- log(counts + 1)
  unname(stats::coef(stats::lm(log_n ~ log_s))[2L])
}

#' Ruler / divider fractal dimension
#'
#' Slope-based estimator from polygonal lengths at log-spaced ruler
#' sizes. Equivalent up to sign convention to Higuchi.
#'
#' @param x Numeric vector.
#' @param n_rulers Number of ruler sizes. Default 10.
#' @return Scalar fractal dimension.
#' @references Rangayyan & Krishnan (2015), Ch. 5, sec. 5.7.
#' @export
morie_dsp_ruler_fd <- function(x, n_rulers = 10L) {
  n <- length(x)
  rulers <- unique(pmin(pmax(
    as.integer(10^seq(0, log10(n / 2), length.out = n_rulers)),
    1L), n %/% 2L))
  lengths <- numeric(length(rulers))
  for (j in seq_along(rulers)) {
    r <- rulers[j]
    total <- 0
    pos <- 1L
    while (pos + r <= n) {
      total <- total + sqrt(r^2 + (x[pos + r] - x[pos])^2)
      pos <- pos + r
    }
    lengths[j] <- total
  }
  if (length(lengths) < 2L) return(1)
  log_r <- log(as.numeric(rulers))
  log_l <- log(lengths + 1e-15)
  slope <- unname(stats::coef(stats::lm(log_l ~ log_r))[2L])
  1 - slope
}

#' Parzen kernel density estimate
#'
#' Gaussian-kernel KDE on a uniform grid. Bandwidth defaults to
#' Silverman's `1.06 * sd(x) * n^{-1/5}`.
#'
#' @param x Numeric vector.
#' @param bandwidth Optional kernel bandwidth.
#' @param n_points Grid size. Default 100.
#' @return List with `grid` and `density`.
#' @references Rangayyan & Krishnan (2015), Ch. 5, sec. 5.6;
#'   Parzen (1962); Silverman (1986).
#' @export
morie_dsp_parzen_pdf <- function(x, bandwidth = NULL, n_points = 100L) {
  if (is.null(bandwidth)) {
    bandwidth <- 1.06 * stats::sd(x) * length(x)^(-0.2)
  }
  if (bandwidth <= 0) bandwidth <- 0.1
  grid <- seq(min(x) - 3 * bandwidth, max(x) + 3 * bandwidth,
              length.out = n_points)
  d <- numeric(n_points)
  for (xi in x) {
    d <- d + exp(-0.5 * ((grid - xi) / bandwidth)^2)
  }
  d <- d / (length(x) * bandwidth * sqrt(2 * pi))
  list(grid = grid, density = d)
}

#' Complex demodulation around a carrier
#'
#' Multiplies `x` by `exp(-j 2 pi fc t)`, low-passes via Butterworth
#' (requires `signal`), and returns envelope and unwrapped phase.
#'
#' @param x Numeric vector.
#' @param fc Carrier frequency (Hz).
#' @param fs Sampling frequency (Hz). Default 1.
#' @return List with `envelope` and `phase`, both length(x).
#' @references Rangayyan & Krishnan (2015), Ch. 5, sec. 5.8.
#' @export
morie_dsp_complex_demodulation <- function(x, fc, fs = 1) {
  if (!requireNamespace("signal", quietly = TRUE)) {
    stop("morie_dsp_complex_demodulation requires the 'signal' package")
  }
  t <- (seq_along(x) - 1L) / fs
  analytic <- x * exp(-1i * 2 * pi * fc * t)
  nyq <- fs / 2
  cutoff <- min(fc * 0.5, nyq * 0.9) / nyq
  if (cutoff <= 0 || cutoff >= 1) cutoff <- 0.1
  ba <- signal::butter(4, cutoff, type = "low")
  envelope <- as.numeric(signal::filtfilt(ba$b, ba$a, Mod(analytic)))
  phase <- .unwrap(Arg(analytic))
  list(envelope = envelope, phase = phase)
}

#' Minimum-phase correspondent via cepstral folding
#'
#' Folds the real cepstrum to the causal half then re-exponentiates
#' to produce a minimum-phase sequence with the same magnitude
#' spectrum as `x` (approximate).
#'
#' @param x Numeric vector.
#' @return Numeric vector, length(x).
#' @references Rangayyan & Krishnan (2015), Ch. 5;
#'   Oppenheim & Schafer (2010).
#' @export
morie_dsp_min_phase <- function(x) {
  X <- stats::fft(x)
  log_mag <- log(Mod(X) + 1e-10)
  cep <- Re(stats::fft(log_mag, inverse = TRUE) / length(log_mag))
  n <- length(cep)
  win <- numeric(n)
  win[1L] <- 1
  if (n >= 4L) win[2:(n %/% 2L)] <- 2
  if (n %% 2L == 0L) win[n %/% 2L + 1L] <- 1
  min_cep <- cep * win
  min_spec <- exp(stats::fft(min_cep))
  Re(stats::fft(min_spec, inverse = TRUE) / n)
}

#' QRS-style waveform descriptors
#'
#' Returns peak amplitude, duration (samples), absolute area
#' (trapezoid), up/down slopes, and peak index for a single beat.
#'
#' @param beat Numeric vector covering one beat.
#' @return Named list of features.
#' @references Rangayyan & Krishnan (2015), Ch. 5, sec. 5.9.
#' @export
morie_dsp_qrs_features <- function(beat) {
  peak <- which.max(abs(beat))
  amplitude <- beat[peak]
  duration <- length(beat)
  # Trapezoid integration of |beat| on unit spacing.
  ab <- abs(beat)
  area <- sum((ab[-1L] + ab[-length(ab)]) / 2)
  slope_up <- if (peak > 1L) max(diff(beat[1:peak])) else 0
  slope_down <- if (peak < length(beat)) min(diff(beat[peak:length(beat)])) else 0
  list(amplitude = amplitude, duration = duration, area = area,
       slope_up = slope_up, slope_down = slope_down,
       peak_index = as.integer(peak))
}

#' Baseline-corrected Pearson correlation
#'
#' `cor(x - mean(x), y - mean(y))` with explicit zero-norm guard.
#'
#' @param x Numeric vector.
#' @param y Numeric vector.
#' @return Scalar in `[-1, 1]`.
#' @references Rangayyan & Krishnan (2015), Ch. 5.
#' @export
morie_dsp_baseline_correlation <- function(x, y) {
  xc <- x - mean(x)
  yc <- y - mean(y)
  den <- sqrt(sum(xc^2) * sum(yc^2))
  if (den == 0) return(0)
  sum(xc * yc) / den
}

# ---- internal helpers -------------------------------------------------

# numpy.unwrap port: shift phase jumps > pi by 2*pi.
.unwrap <- function(p, tol = pi) {
  d <- diff(p)
  adj <- ifelse(d >  tol, d - 2 * pi,
         ifelse(d < -tol, d + 2 * pi, d))
  c(p[1L], p[1L] + cumsum(adj))
}

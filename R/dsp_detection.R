# SPDX-License-Identifier: AGPL-3.0-or-later

# Event-detection primitives from Rangayyan & Krishnan (2015),
# Biomedical Signal Analysis, 2nd ed., Chapter 4.
#
# Ports src/morie/_detection.py. Public functions carry the
# `morie_dsp_` prefix.

#' Threshold-based event detection
#'
#' Returns sample indices (1-based) where `x` crosses `threshold` in
#' the chosen direction. `min_distance` enforces a minimum gap (in
#' samples) between successive events.
#'
#' @param x Numeric vector.
#' @param threshold Scalar threshold.
#' @param min_distance Minimum gap between events. Default 1.
#' @param direction One of "above", "below", "either". Default "above".
#' @return Integer vector of detected indices.
#' @references Rangayyan & Krishnan (2015), Ch. 4, sec. 4.2.
#' @export
morie_dsp_threshold_detect <- function(x, threshold, min_distance = 1L,
                                       direction = "above") {
  x <- as.numeric(x)
  mask <- switch(direction,
    "above"  = x > threshold,
    "below"  = x < threshold,
    "either" = abs(x) > threshold,
    stop("direction must be one of 'above', 'below', 'either'")
  )
  idx <- which(mask)
  if (length(idx) == 0L || min_distance <= 1L) return(idx)
  kept <- idx[1L]
  for (i in idx[-1L]) if (i - kept[length(kept)] >= min_distance) {
    kept <- c(kept, i)
  }
  kept
}

#' Derivative-based peak detection
#'
#' Locates samples where the first derivative crosses zero from
#' positive to non-positive AND the prior slope magnitude exceeds
#' `threshold_factor * max(|dx|)`.
#'
#' @param x Numeric vector.
#' @param fs Sampling frequency (Hz). Default 1.
#' @param threshold_factor Slope threshold fraction. Default 0.5.
#' @return Integer vector of peak indices.
#' @references Rangayyan & Krishnan (2015), Ch. 4, sec. 4.3.
#' @export
morie_dsp_derivative_detect <- function(x, fs = 1,
                                        threshold_factor = 0.5) {
  dx <- diff(x) * fs
  if (length(dx) < 3L) return(integer(0))
  thr <- threshold_factor * max(abs(dx))
  peaks <- integer(0)
  for (i in 2L:(length(dx) - 1L)) {
    if (dx[i - 1L] > 0 && dx[i] <= 0 && abs(dx[i - 1L]) > thr) {
      peaks <- c(peaks, i)
    }
  }
  peaks
}

#' Zero-crossing rate
#'
#' Whole-signal ZCR if `frame_length = NULL`; otherwise per-frame
#' ZCR over consecutive non-overlapping frames.
#'
#' @param x Numeric vector.
#' @param frame_length Optional frame length.
#' @return Scalar (whole signal) or numeric vector (per frame).
#' @references Rangayyan & Krishnan (2015), Ch. 4, sec. 4.3.
#' @export
morie_dsp_zero_crossing <- function(x, frame_length = NULL) {
  if (is.null(frame_length)) {
    s <- sign(x)
    return(sum(abs(diff(s)) > 0) / (length(x) - 1L))
  }
  n_frames <- length(x) %/% frame_length
  zcr <- numeric(n_frames)
  for (i in seq_len(n_frames)) {
    f <- x[((i - 1L) * frame_length + 1L):(i * frame_length)]
    zcr[i] <- sum(abs(diff(sign(f))) > 0) / (frame_length - 1L)
  }
  zcr
}

#' Normalised template matching
#'
#' Slides `template` across `x`, computing the centred Pearson-style
#' correlation per offset. Returns indices and correlation values
#' meeting `threshold`.
#'
#' @param x Numeric vector.
#' @param template Numeric vector (shorter than `x`).
#' @param threshold Minimum correlation. Default 0.7.
#' @return List with `indices` (1-based) and `correlations`.
#' @references Rangayyan & Krishnan (2015), Ch. 4, sec. 4.4.
#' @export
morie_dsp_template_match <- function(x, template, threshold = 0.7) {
  x <- as.numeric(x)
  template <- as.numeric(template)
  n <- length(template)
  if (length(x) < n) return(list(indices = integer(0),
                                 correlations = numeric(0)))
  t_norm <- template - mean(template)
  t_std <- stats::sd(template) * sqrt((n - 1) / n)  # match np.std (population)
  if (t_std == 0) return(list(indices = integer(0),
                              correlations = numeric(0)))
  m <- length(x) - n + 1L
  cors <- numeric(m)
  for (i in seq_len(m)) {
    seg <- x[i:(i + n - 1L)]
    s_centered <- seg - mean(seg)
    s_std <- sqrt(mean(s_centered^2))
    if (s_std == 0) next
    cors[i] <- sum(s_centered * t_norm) / (n * s_std * t_std)
  }
  idx <- which(cors >= threshold)
  list(indices = idx, correlations = cors[idx])
}

#' Energy-onset detection
#'
#' Smooths `x^2` with a `energy_window_ms` boxcar and flags samples
#' where the smoothed energy crosses `threshold_factor * median(energy)`.
#' Hysteresis: returns to "off" only when energy drops below baseline.
#'
#' @param x Numeric vector.
#' @param fs Sampling frequency (Hz).
#' @param energy_window_ms Smoothing window (ms). Default 20.
#' @param threshold_factor Multiplier on baseline median. Default 3.
#' @return Integer vector of onset indices.
#' @references Rangayyan & Krishnan (2015), Ch. 4, sec. 4.5.
#' @export
morie_dsp_onset_detect <- function(x, fs, energy_window_ms = 20,
                                   threshold_factor = 3) {
  win <- max(1L, as.integer(energy_window_ms * fs / 1000))
  kernel <- rep(1 / win, win)
  energy <- .same_convolve(x^2, kernel)
  # Robust noise floor: 10th percentile (was: median, which sits inside
  # the burst on a flat-then-loud signal and yields a too-high threshold).
  baseline <- max(stats::quantile(energy, 0.1, na.rm = TRUE,
                                   names = FALSE),
                   .Machine$double.eps)
  thr <- baseline * threshold_factor
  onsets <- integer(0)
  above <- FALSE
  for (i in seq_along(energy)) {
    if (!above && energy[i] > thr) { onsets <- c(onsets, i)
    above <- TRUE }
    else if (above && energy[i] < baseline) above <- FALSE
  }
  onsets
}

#' Shannon-energy envelope
#'
#' `-x^2 * log(x^2)` with a small floor to guard log(0). Amplifies
#' moderate-energy components, useful as a preprocessor for heart-sound
#' segmentation.
#'
#' @param x Numeric vector.
#' @return Numeric vector, length(x).
#' @references Rangayyan & Krishnan (2015), Ch. 4, sec. 4.5;
#'   Liang et al. (1997).
#' @export
morie_dsp_shannon_energy <- function(x) {
  # Normalise to max amplitude so x_sq lies in [0, 1] and the entropy
  # form -p log(p) stays non-negative even when |x| > 1.
  m <- max(abs(x))
  if (m > 0) x <- x / m
  x_sq <- pmax(x^2, 1e-12)
  -x_sq * log(x_sq)
}

#' Teager-Kaiser energy operator
#'
#' `psi[n] = x[n]^2 - x[n-1] * x[n+1]`; sensitive to instantaneous
#' amplitude AND frequency.
#'
#' @param x Numeric vector.
#' @return Numeric vector, length(x); ends are zero.
#' @references Rangayyan & Krishnan (2015), Ch. 4, sec. 4.5;
#'   Kaiser (1990).
#' @export
morie_dsp_teager_energy <- function(x) {
  n <- length(x)
  psi <- numeric(n)
  if (n < 3L) return(psi)
  psi[2L:(n - 1L)] <- x[2L:(n - 1L)]^2 - x[1L:(n - 2L)] * x[3L:n]
  psi
}

#' Hilbert envelope
#'
#' Analytic-signal magnitude via the Hilbert transform. Delegates to
#' `signal::hilbert` when available.
#'
#' @param x Numeric vector.
#' @return Numeric vector, length(x).
#' @references Rangayyan & Krishnan (2015), Ch. 4, sec. 4.6.
#' @export
morie_dsp_hilbert_envelope <- function(x) {
  if (requireNamespace("signal", quietly = TRUE) &&
      exists("hilbert", where = asNamespace("signal"))) {
    return(Mod(get("hilbert", envir = asNamespace("signal"))(x)))
  }
  # Pure-R fallback: build analytic signal in the FFT domain.
  n <- length(x)
  X <- stats::fft(x)
  h <- numeric(n)
  if (n %% 2L == 0L) {
    h[1L] <- 1
    h[n / 2L + 1L] <- 1
    h[2L:(n / 2L)] <- 2
  } else {
    h[1L] <- 1
    h[2L:((n + 1L) / 2L)] <- 2
  }
  analytic <- stats::fft(X * h, inverse = TRUE) / n
  Mod(analytic)
}

#' Pan-Tompkins QRS detector
#'
#' Bandpass (5-15 Hz) -> differentiate -> square -> moving-window
#' integrate -> adaptive threshold with refractory period. Requires
#' `signal` for the Butterworth bandpass.
#'
#' @param ecg ECG vector.
#' @param fs Sampling frequency (Hz). Default 360.
#' @return Integer vector of QRS sample indices.
#' @references Rangayyan & Krishnan (2015), Ch. 4, sec. 4.7;
#'   Pan & Tompkins (1985).
#' @export
morie_dsp_pan_tompkins <- function(ecg, fs = 360) {
  if (!requireNamespace("signal", quietly = TRUE)) {
    stop("morie_dsp_pan_tompkins requires the 'signal' package")
  }
  nyq <- fs / 2
  low <- 5 / nyq
  high <- min(15 / nyq, 0.99)
  ba <- signal::butter(1, c(low, high), type = "pass")
  filtered <- as.numeric(signal::filtfilt(ba$b, ba$a, ecg))
  diffs <- diff(filtered)
  squared <- diffs^2
  win <- max(1L, as.integer(0.15 * fs))
  kernel <- rep(1 / win, win)
  integrated <- .same_convolve(squared, kernel)
  thr <- 0.5 * max(integrated)
  cand <- which(integrated > thr)
  if (length(cand) == 0L) return(integer(0))
  refract <- as.integer(0.2 * fs)
  qrs <- cand[1L]
  for (c in cand[-1L]) {
    if (c - qrs[length(qrs)] > refract) qrs <- c(qrs, c)
  }
  qrs
}

#' Dicrotic-notch detection in pulse waves
#'
#' Finds prominent minima in the second derivative outside the
#' systolic onset window. Requires `signal::findpeaks`.
#'
#' @param pulse Numeric pulse vector.
#' @param fs Sampling frequency (Hz). Default 125.
#' @return Integer vector of notch indices.
#' @references Rangayyan & Krishnan (2015), Ch. 4, sec. 4.8.
#' @export
morie_dsp_dicrotic_notch <- function(pulse, fs = 125) {
  if (!requireNamespace("signal", quietly = TRUE) ||
      !exists("findpeaks", where = asNamespace("signal"))) {
    stop("morie_dsp_dicrotic_notch requires signal::findpeaks")
  }
  d2 <- diff(pulse, differences = 2L)
  pk <- get("findpeaks", envir = asNamespace("signal"))(-d2, MinPeakDistance = as.integer(0.1 * fs))
  if (is.null(pk) || length(pk) == 0L) return(integer(0))
  # signal::findpeaks returns a matrix with peak positions in column 2.
  positions <- if (is.matrix(pk)) pk[, 2L] else pk
  systolic_end <- as.integer(0.3 * fs)
  positions[positions > systolic_end]
}

#' T-wave detection by post-QRS argmax search
#'
#' For each QRS index, searches `[loc + 0.2 * fs, loc + 0.5 * fs]` for
#' the absolute maximum and records its global index.
#'
#' @param ecg ECG vector.
#' @param qrs_locs QRS indices (1-based) from a detector like
#'   `morie_dsp_pan_tompkins`.
#' @param fs Sampling frequency (Hz). Default 360.
#' @return Integer vector of T-peak indices.
#' @references Rangayyan & Krishnan (2015), Ch. 4, sec. 4.8.
#' @export
morie_dsp_t_wave <- function(ecg, qrs_locs, fs = 360) {
  search_start <- as.integer(0.2 * fs)
  search_end <- as.integer(0.5 * fs)
  out <- integer(0)
  for (loc in qrs_locs) {
    s <- loc + search_start
    e <- loc + search_end
    if (e > length(ecg)) break
    seg <- ecg[s:e]
    out <- c(out, s + which.max(seg) - 1L)
  }
  out
}

#' Homomorphic high-pass filter
#'
#' Log -> rFFT high-pass at `cutoff` Hz -> exp. Reduces multiplicative
#' baseline drift in non-negative envelopes.
#'
#' @param x Numeric vector.
#' @param cutoff Cutoff frequency (Hz). Default 0.1.
#' @param fs Sampling frequency (Hz). Default 1.
#' @return Numeric vector, length(x).
#' @references Rangayyan & Krishnan (2015), Ch. 4, sec. 4.9;
#'   Oppenheim & Schafer (2010).
#' @export
morie_dsp_homomorphic <- function(x, cutoff = 0.1, fs = 1) {
  log_x <- log(abs(x) + 1e-10)
  n <- length(log_x)
  X <- stats::fft(log_x)
  half <- n %/% 2L + 1L
  freqs <- seq.int(0, half - 1L) * fs / n
  H <- rep(1, half)
  H[freqs < cutoff] <- 0
  Y_half <- X[seq_len(half)] * H
  if (n %% 2L == 0L) {
    full <- c(Y_half, Conj(rev(Y_half[2:(half - 1L)])))
  } else {
    full <- c(Y_half, Conj(rev(Y_half[2:half])))
  }
  filtered <- Re(stats::fft(full, inverse = TRUE) / n)
  exp(filtered)
}

#' Complex cepstrum
#'
#' `IFFT(log|X| + j * unwrap(angle(X)))`. Returns cepstrum and
#' quefrency indices.
#'
#' @param x Numeric vector.
#' @return List with `cepstrum` and `quefrency`.
#' @references Rangayyan & Krishnan (2015), Ch. 4, sec. 4.10;
#'   Oppenheim & Schafer (2010).
#' @export
morie_dsp_complex_cepstrum <- function(x) {
  X <- stats::fft(x)
  log_X <- log(Mod(X) + 1e-10) + 1i * .unwrap_d(Arg(X))
  cep <- Re(stats::fft(log_X, inverse = TRUE) / length(log_X))
  list(cepstrum = cep, quefrency = seq.int(0L, length(cep) - 1L))
}

#' Heart rate from RR intervals (BPM)
#'
#' `60 / rr`, with zero-RR turned into `NA`.
#'
#' @param rr_intervals Numeric vector (seconds).
#' @return Numeric vector of BPM values.
#' @references Rangayyan & Krishnan (2015), Ch. 4, sec. 4.8.
#' @export
morie_dsp_hr_from_rr <- function(rr_intervals) {
  rr <- as.numeric(rr_intervals)
  rr[rr == 0] <- NA_real_
  60 / rr
}

#' Coherence spectrum (alias to morie_dsp_coherence)
#'
#' Convenience pass-through so detection-side callers can stay within
#' the `dsp_detection` namespace.
#'
#' @inheritParams morie_dsp_coherence
#' @return Same as `morie_dsp_coherence`.
#' @references Rangayyan & Krishnan (2015), Ch. 4 & Ch. 6.
#' @export
morie_dsp_coherence_spectrum <- function(x, y, fs = 1, nperseg = 256L) {
  morie_dsp_coherence(x, y, fs = fs, nperseg = nperseg)
}

#' Cross-spectral density (Welch)
#'
#' Hamming-windowed averaged CSD. Delegates to `signal::cpsd` if
#' present; otherwise computed from the same FFT loop as the coherence
#' fallback.
#'
#' @param x Numeric vector.
#' @param y Numeric vector.
#' @param fs Sampling frequency (Hz). Default 1.
#' @param nperseg Segment length. Default 256.
#' @return List with `freqs` (Hz) and `csd` (complex).
#' @references Rangayyan & Krishnan (2015), Ch. 4 & Ch. 6.
#' @export
morie_dsp_csd <- function(x, y, fs = 1, nperseg = 256L) {
  nperseg <- as.integer(min(nperseg, length(x)))
  noverlap <- nperseg %/% 2L
  step <- nperseg - noverlap
  starts <- seq.int(1L, length(x) - nperseg + 1L, by = step)
  if (length(starts) == 0L) starts <- 1L
  win <- morie_dsp_window(nperseg, "hamming")
  half <- nperseg %/% 2L + 1L
  Pxy <- complex(half)
  scale <- sum(win^2) * fs
  for (s in starts) {
    sx <- x[s:(s + nperseg - 1L)] * win
    sy <- y[s:(s + nperseg - 1L)] * win
    X <- stats::fft(sx)[seq_len(half)]
    Y <- stats::fft(sy)[seq_len(half)]
    Pxy <- Pxy + X * Conj(Y) / scale
  }
  Pxy <- Pxy / length(starts)
  freqs <- seq.int(0, half - 1L) * fs / nperseg
  list(freqs = freqs, csd = Pxy)
}

# ---- internal helpers -------------------------------------------------

# Local unwrap (numeric vector).
.unwrap_d <- function(p, tol = pi) {
  d <- diff(p)
  adj <- ifelse(d >  tol, d - 2 * pi,
         ifelse(d < -tol, d + 2 * pi, d))
  c(p[1L], p[1L] + cumsum(adj))
}

# SPDX-License-Identifier: AGPL-3.0-or-later

# Spectral analysis primitives from Rangayyan & Krishnan (2015),
# Biomedical Signal Analysis, 2nd ed., Chapter 6.
#
# Ports src/morie/_spectral.py. Every public function carries the
# `morie_dsp_` prefix; PSD estimators use the `psd_` sub-prefix.

#' Periodogram PSD estimate
#'
#' One-sided rFFT-based periodogram. Inner bins are doubled to fold
#' negative-frequency power into the one-sided spectrum.
#'
#' @param x Numeric vector.
#' @param fs Sampling frequency (Hz). Default 1.
#' @return List with `freqs` and `psd`, both length `floor(N/2)+1`.
#' @references Rangayyan & Krishnan (2015), Ch. 6, sec. 6.4.
#' @export
morie_dsp_psd_periodogram <- function(x, fs = 1) {
  x <- as.numeric(x)
  N <- length(x)
  X <- stats::fft(x)
  half <- N %/% 2L + 1L
  X_half <- X[seq_len(half)]
  psd <- (Mod(X_half)^2) / (N * fs)
  if (half >= 3L) psd[2:(half - 1L)] <- psd[2:(half - 1L)] * 2
  freqs <- seq.int(0, half - 1L) * fs / N
  list(freqs = freqs, psd = psd)
}

#' Bartlett PSD estimate
#'
#' Splits `x` into `n_segments` non-overlapping equal segments,
#' periodograms each, and averages the result. Variance scales as
#' `1 / n_segments` at the cost of frequency resolution.
#'
#' @param x Numeric vector.
#' @param fs Sampling frequency (Hz). Default 1.
#' @param n_segments Number of equal segments. Default 8.
#' @return List with `freqs` and `psd`.
#' @references Rangayyan & Krishnan (2015), Ch. 6, sec. 6.4;
#'   Bartlett (1948).
#' @export
morie_dsp_psd_bartlett <- function(x, fs = 1, n_segments = 8L) {
  x <- as.numeric(x)
  seg_len <- length(x) %/% n_segments
  psd_sum <- NULL
  freqs <- NULL
  for (i in seq_len(n_segments)) {
    seg <- x[((i - 1L) * seg_len + 1L):(i * seg_len)]
    out <- morie_dsp_psd_periodogram(seg, fs)
    if (is.null(psd_sum)) {
      psd_sum <- out$psd
      freqs <- out$freqs
    } else {
      psd_sum <- psd_sum + out$psd
    }
  }
  list(freqs = freqs, psd = psd_sum / n_segments)
}

#' Welch PSD estimate (delegated to signal::pwelch / specgram)
#'
#' Thin wrapper that prefers the `signal` package's `pwelch`-style
#' routine for the production estimator; falls back to a pure-R
#' Hamming-windowed averaged periodogram when `signal` is unavailable.
#'
#' @param x Numeric vector.
#' @param fs Sampling frequency (Hz). Default 1.
#' @param nperseg Segment length. Default 256.
#' @param noverlap Overlap in samples. Default `nperseg %/% 2`.
#' @return List with `freqs` and `psd`.
#' @references Rangayyan & Krishnan (2015), Ch. 6, sec. 6.4;
#'   Welch (1967).
#' @export
morie_dsp_psd_welch <- function(x, fs = 1, nperseg = 256L,
                                noverlap = NULL) {
  x <- as.numeric(x)
  nperseg <- as.integer(min(nperseg, length(x)))
  if (is.null(noverlap)) noverlap <- nperseg %/% 2L
  if (requireNamespace("signal", quietly = TRUE) &&
      exists("pwelch", where = asNamespace("signal"))) {
    out <- get("pwelch", envir = asNamespace("signal"))(x, window = nperseg, overlap = noverlap,
                          fs = fs, plot = FALSE)
    return(list(freqs = as.numeric(out$freq),
                psd = as.numeric(out$spec)))
  }
  # Pure-R fallback: Hamming-windowed averaged periodograms.
  step <- nperseg - noverlap
  starts <- seq.int(1L, length(x) - nperseg + 1L, by = step)
  if (length(starts) == 0L) starts <- 1L
  win <- morie_dsp_window(nperseg, "hamming")
  scale <- sum(win^2) * fs
  half <- nperseg %/% 2L + 1L
  psd_sum <- numeric(half)
  for (s in starts) {
    seg <- x[s:(s + nperseg - 1L)] * win
    X <- stats::fft(seg)[seq_len(half)]
    p <- (Mod(X)^2) / scale
    if (half >= 3L) p[2:(half - 1L)] <- p[2:(half - 1L)] * 2
    psd_sum <- psd_sum + p
  }
  freqs <- seq.int(0, half - 1L) * fs / nperseg
  list(freqs = freqs, psd = psd_sum / length(starts))
}

#' k-th spectral moment
#'
#' `m_k = sum(freqs^k * psd) * df`. Used to derive mean, median, edge,
#' and higher-order frequency descriptors.
#'
#' @param psd PSD vector.
#' @param freqs Matching frequency vector.
#' @param order Moment order. Default 0.
#' @return Scalar moment.
#' @references Rangayyan & Krishnan (2015), Ch. 6, sec. 6.6.
#' @export
morie_dsp_spectral_moment <- function(psd, freqs, order = 0L) {
  df <- if (length(freqs) > 1L) freqs[2] - freqs[1] else 1
  sum((freqs^order) * psd) * df
}

#' Mean frequency from PSD
#'
#' `m1 / m0`; equals the first moment normalised by total power.
#'
#' @inheritParams morie_dsp_spectral_moment
#' @return Scalar mean frequency (Hz).
#' @references Rangayyan & Krishnan (2015), Ch. 6, sec. 6.6.
#' @export
morie_dsp_mean_frequency <- function(psd, freqs) {
  m0 <- morie_dsp_spectral_moment(psd, freqs, 0L)
  if (m0 == 0) return(0)
  morie_dsp_spectral_moment(psd, freqs, 1L) / m0
}

#' Median frequency from PSD
#'
#' Frequency at which the cumulative spectrum reaches half the total
#' power. Robust to high-frequency outliers vs. the mean frequency.
#'
#' @inheritParams morie_dsp_spectral_moment
#' @return Scalar (Hz).
#' @references Rangayyan & Krishnan (2015), Ch. 6, sec. 6.6.
#' @export
morie_dsp_median_frequency <- function(psd, freqs) {
  df <- if (length(freqs) > 1L) freqs[2] - freqs[1] else 1
  cum <- cumsum(psd * df)
  total <- cum[length(cum)]
  if (total == 0) return(0)
  idx <- which(cum >= total / 2)[1L]
  if (is.na(idx)) idx <- length(freqs)
  freqs[idx]
}

#' Spectral edge frequency (e.g. SEF95)
#'
#' Frequency below which `pct` fraction of total power lies. SEF95
#' (`pct = 0.95`) is a classical EEG depth-of-anaesthesia marker.
#'
#' @inheritParams morie_dsp_spectral_moment
#' @param pct Cumulative fraction in (0, 1]. Default 0.95.
#' @return Scalar (Hz).
#' @references Rangayyan & Krishnan (2015), Ch. 6, sec. 6.6.
#' @export
morie_dsp_spectral_edge <- function(psd, freqs, pct = 0.95) {
  df <- if (length(freqs) > 1L) freqs[2] - freqs[1] else 1
  cum <- cumsum(psd * df)
  total <- cum[length(cum)]
  if (total == 0) return(0)
  idx <- which(cum >= pct * total)[1L]
  if (is.na(idx)) idx <- length(freqs)
  freqs[idx]
}

#' Spectral power ratio between two bands
#'
#' Returns `bandpower(psd, band1) / bandpower(psd, band2)`.
#'
#' @param psd PSD vector.
#' @param freqs Matching frequency vector.
#' @param band1 Length-2 numeric (low, high) Hz.
#' @param band2 Length-2 numeric (low, high) Hz.
#' @return Scalar ratio.
#' @references Rangayyan & Krishnan (2015), Ch. 6, sec. 6.6.
#' @export
morie_dsp_spectral_ratio <- function(psd, freqs, band1, band2) {
  df <- if (length(freqs) > 1L) freqs[2] - freqs[1] else 1
  p1 <- sum(psd[freqs >= band1[1] & freqs <= band1[2]]) * df
  p2 <- sum(psd[freqs >= band2[1] & freqs <= band2[2]]) * df
  if (p2 == 0) return(Inf)
  p1 / p2
}

#' Spectral flatness (Wiener entropy)
#'
#' Geometric-to-arithmetic mean ratio of the positive PSD bins;
#' values near 1 indicate a white spectrum, near 0 indicate tonal
#' concentration.
#'
#' @param psd PSD vector.
#' @return Scalar in `[0, 1]`.
#' @references Rangayyan & Krishnan (2015), Ch. 6, sec. 6.7.
#' @export
morie_dsp_spectral_flatness <- function(psd) {
  p <- psd[psd > 0]
  if (length(p) == 0L) return(0)
  am <- mean(p)
  if (am == 0) return(0)
  exp(mean(log(p))) / am
}

#' Spectral entropy (Shannon, base 2)
#'
#' Normalises PSD to a probability mass function and returns its
#' Shannon entropy in bits.
#'
#' @param psd PSD vector.
#' @return Scalar in `[0, log2(length(psd))]`.
#' @references Rangayyan & Krishnan (2015), Ch. 6, sec. 6.7;
#'   Inouye et al. (1991).
#' @export
morie_dsp_spectral_entropy <- function(psd) {
  total <- sum(psd)
  if (total == 0) return(0)
  p <- psd / total
  p <- p[p > 0]
  -sum(p * log2(p))
}

#' Spectral kurtosis from PSD
#'
#' Standardised fourth central moment of frequency under the PSD
#' treated as a probability mass.
#'
#' @inheritParams morie_dsp_spectral_moment
#' @return Scalar.
#' @references Rangayyan & Krishnan (2015), Ch. 6.
#' @export
morie_dsp_spectral_kurtosis <- function(psd, freqs) {
  total <- sum(psd)
  if (total == 0) return(0)
  p <- psd / total
  fm <- sum(freqs * p)
  m2 <- sum((freqs - fm)^2 * p)
  m4 <- sum((freqs - fm)^4 * p)
  if (m2 == 0) return(0)
  m4 / m2^2
}

#' Convert PSD to decibels
#'
#' `10 * log10(max(psd, 1e-20))`.
#'
#' @param psd PSD vector.
#' @return PSD in dB.
#' @references Rangayyan & Krishnan (2015), Ch. 6.
#' @export
morie_dsp_psd_to_db <- function(psd) {
  10 * log10(pmax(psd, 1e-20))
}

#' Autocorrelation from PSD (Wiener-Khinchin)
#'
#' Inverse-rFFT of the PSD recovers the (biased) autocorrelation.
#'
#' @param psd PSD vector (one-sided).
#' @return Numeric vector (autocorrelation).
#' @references Rangayyan & Krishnan (2015), Ch. 6, sec. 6.3.
#' @export
morie_dsp_acf_from_psd <- function(psd) {
  half <- length(psd)
  N <- 2L * (half - 1L)
  if (N < 2L) N <- 2L * half
  full <- c(psd, Conj(rev(psd[2:(half - 1L)])))
  if (length(full) != N) {
    # Odd-length case
    full <- c(psd, Conj(rev(psd[2:half])))
    N <- length(full)
  }
  Re(stats::fft(full, inverse = TRUE) / N)
}

#' Bandpower over `[f_low, f_high]`
#'
#' Trapezoid-equivalent rectangular integration of PSD over a band.
#'
#' @inheritParams morie_dsp_spectral_moment
#' @param f_low Lower edge (Hz).
#' @param f_high Upper edge (Hz).
#' @return Band power (units of PSD x Hz).
#' @references Rangayyan & Krishnan (2015), Ch. 6.
#' @export
morie_dsp_band_power <- function(psd, freqs, f_low, f_high) {
  df <- if (length(freqs) > 1L) freqs[2] - freqs[1] else 1
  sum(psd[freqs >= f_low & freqs <= f_high]) * df
}

#' Fractal dimension from log-log PSD slope
#'
#' Fits `log10(psd) ~ log10(f)` on positive bins; with slope `-beta`,
#' returns the 1/f fractal dimension `(5 - beta) / 2`. Falls back to
#' 1.5 (Brownian) when fewer than two valid bins exist.
#'
#' @inheritParams morie_dsp_spectral_moment
#' @return Scalar fractal dimension.
#' @references Rangayyan & Krishnan (2015), Ch. 6, sec. 6.8;
#'   Eke et al. (2002).
#' @export
morie_dsp_fractal_dim_psd <- function(psd, freqs) {
  valid <- (freqs > 0) & (psd > 0)
  if (sum(valid) < 2L) return(1.5)
  fit <- stats::lm(log10(psd[valid]) ~ log10(freqs[valid]))
  beta <- -unname(stats::coef(fit)[2L])
  (5 - beta) / 2
}

#' Coherence-squared spectrum
#'
#' Delegates to `signal::coherence` when present; otherwise builds a
#' Welch-style estimator from `morie_dsp_psd_welch` and a parallel CSD.
#'
#' @param x Numeric vector.
#' @param y Numeric vector.
#' @param fs Sampling frequency (Hz). Default 1.
#' @param nperseg Segment length. Default 256.
#' @return List with `freqs` and `coh` (magnitude squared coherence).
#' @references Rangayyan & Krishnan (2015), Ch. 6, sec. 6.9.
#' @export
morie_dsp_coherence <- function(x, y, fs = 1, nperseg = 256L) {
  if (requireNamespace("signal", quietly = TRUE) &&
      exists("coherence", where = asNamespace("signal"))) {
    out <- get("coherence", envir = asNamespace("signal"))(x, y, fs = fs, nperseg = nperseg)
    return(list(freqs = as.numeric(out$freq),
                coh = as.numeric(out$coh)))
  }
  # Pure-R Welch coherence fallback.
  nperseg <- as.integer(min(nperseg, length(x)))
  noverlap <- nperseg %/% 2L
  step <- nperseg - noverlap
  starts <- seq.int(1L, length(x) - nperseg + 1L, by = step)
  if (length(starts) == 0L) starts <- 1L
  win <- morie_dsp_window(nperseg, "hamming")
  half <- nperseg %/% 2L + 1L
  Pxx <- numeric(half)
  Pyy <- numeric(half)
  Pxy <- complex(half)
  for (s in starts) {
    sx <- x[s:(s + nperseg - 1L)] * win
    sy <- y[s:(s + nperseg - 1L)] * win
    X <- stats::fft(sx)[seq_len(half)]
    Y <- stats::fft(sy)[seq_len(half)]
    Pxx <- Pxx + Mod(X)^2
    Pyy <- Pyy + Mod(Y)^2
    Pxy <- Pxy + X * Conj(Y)
  }
  coh <- Mod(Pxy)^2 / (Pxx * Pyy)
  freqs <- seq.int(0, half - 1L) * fs / nperseg
  list(freqs = freqs, coh = coh)
}

#' Window function generator
#'
#' Returns a length-`N` window vector of the requested type. Supports
#' hamming, hann/hanning, blackman, bartlett/triangular, kaiser
#' (beta = 14), and rectangular/boxcar. Unknown types default to hamming.
#'
#' @param N Window length.
#' @param wtype Type string. Default "hamming".
#' @return Numeric vector of length `N`.
#' @references Rangayyan & Krishnan (2015), Ch. 6, sec. 6.5.
#' @export
morie_dsp_window <- function(N, wtype = "hamming") {
  N <- as.integer(N)
  if (N < 1L) return(numeric(0))
  n <- seq.int(0, N - 1L)
  wt <- tolower(wtype)
  switch(wt,
    "hamming"    = 0.54 - 0.46 * cos(2 * pi * n / (N - 1L)),
    "hann"       = 0.5 - 0.5 * cos(2 * pi * n / (N - 1L)),
    "hanning"    = 0.5 - 0.5 * cos(2 * pi * n / (N - 1L)),
    "blackman"   = 0.42 - 0.5 * cos(2 * pi * n / (N - 1L)) +
                   0.08 * cos(4 * pi * n / (N - 1L)),
    "bartlett"   = pmin(n, N - 1L - n) * (2 / (N - 1L)),
    "triangular" = pmin(n, N - 1L - n) * (2 / (N - 1L)),
    "kaiser"     = .kaiser_window(N, beta = 14),
    "rectangular" = rep(1, N),
    "boxcar"     = rep(1, N),
    0.54 - 0.46 * cos(2 * pi * n / (N - 1L))
  )
}

#' Fractional Brownian motion synthesis (1/f^beta)
#'
#' Generates a length-`N` fBm via spectral shaping of white noise
#' with `|f|^{-(H + 0.5)}`, then cumulative integration. `H = 0.5`
#' recovers ordinary Brownian motion.
#'
#' @param N Length.
#' @param H Hurst exponent in (0, 1). Default 0.5.
#' @return Numeric vector, length `N`.
#' @references Rangayyan & Krishnan (2015), Ch. 6, sec. 6.8;
#'   Mandelbrot & Van Ness (1968).
#' @export
morie_dsp_fbm_synthesis <- function(N, H = 0.5) {
  N <- as.integer(N)
  white <- stats::rnorm(N)
  half <- N %/% 2L + 1L
  freqs <- seq.int(0, half - 1L) / N
  freqs[1] <- 1
  pf <- freqs^(-(H + 0.5))
  pf[1] <- 0
  X <- stats::fft(white)[seq_len(half)] * pf
  # Reconstitute Hermitian.
  if (N %% 2L == 0L) {
    full <- c(X, Conj(rev(X[2:(half - 1L)])))
  } else {
    full <- c(X, Conj(rev(X[2:half])))
  }
  fbm <- Re(stats::fft(full, inverse = TRUE) / N)
  cumsum(fbm)
}

# ---- internal helpers -------------------------------------------------

# Kaiser window with shape `beta` using the modified Bessel function
# I0. Matches numpy.kaiser(N, beta).
.kaiser_window <- function(N, beta = 14) {
  if (N == 1L) return(1)
  n <- seq.int(0, N - 1L)
  alpha <- (N - 1) / 2
  num <- .bessel_i0(beta * sqrt(1 - ((n - alpha) / alpha)^2))
  num / .bessel_i0(beta)
}

# Polynomial expansion of I0 valid for beta up to ~16.
.bessel_i0 <- function(x) {
  ax <- abs(x)
  out <- numeric(length(ax))
  small <- ax < 3.75
  y_small <- (ax[small] / 3.75)^2
  out[small] <- 1 + y_small *
    (3.5156229 + y_small *
    (3.0899424 + y_small *
    (1.2067492 + y_small *
    (0.2659732 + y_small *
    (0.0360768 + y_small * 0.0045813)))))
  y_big <- 3.75 / ax[!small]
  out[!small] <- (exp(ax[!small]) / sqrt(ax[!small])) *
    (0.39894228 + y_big *
    (0.01328592 + y_big *
    (0.00225319 + y_big *
    (-0.00157565 + y_big *
    (0.00916281 + y_big *
    (-0.02057706 + y_big *
    (0.02635537 + y_big *
    (-0.01647633 + y_big * 0.00392377))))))))
  out
}

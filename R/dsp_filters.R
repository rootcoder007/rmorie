# SPDX-License-Identifier: AGPL-3.0-or-later

# Signal filtering primitives from Rangayyan & Krishnan (2015),
# Biomedical Signal Analysis, 2nd ed., Chapter 3.
#
# Ports src/morie/_filters.py. All exported functions carry the
# `morie_dsp_` prefix to keep this foundation layer namespaced;
# `_adaptive`, `_armodel`, `_biomodel`, `_classify`, `_decompose`
# sit on top.

#' Moving-average filter (boxcar)
#'
#' Length-`window` boxcar convolution in "same" mode (output length
#' equals input length, edges biased by zero-padding).
#'
#' @param x Numeric vector.
#' @param window Positive integer kernel length. Default 5.
#' @return Numeric vector, length(x).
#' @references Rangayyan & Krishnan (2015), Ch. 3, sec. 3.3.
#' @export
morie_dsp_moving_average <- function(x, window = 5L) {
  x <- as.numeric(x)
  window <- as.integer(window)
  if (window < 1L) stop("window must be >= 1")
  kernel <- rep(1 / window, window)
  .same_convolve(x, kernel)
}

#' Hann-windowed smoothing filter
#'
#' Convolves `x` with a normalised Hann (raised-cosine) window of
#' length `window`. Less ringing than the boxcar.
#'
#' @param x Numeric vector.
#' @param window Window length. Default 5.
#' @return Smoothed vector, length(x).
#' @references Rangayyan & Krishnan (2015), Ch. 3.
#' @export
morie_dsp_hann_filter <- function(x, window = 5L) {
  x <- as.numeric(x)
  window <- as.integer(window)
  if (window < 1L) stop("window must be >= 1")
  if (window == 1L) return(x)
  # numpy.hanning is N-point symmetric, zero at the ends.
  w <- 0.5 - 0.5 * cos(2 * pi * seq.int(0, window - 1L) / (window - 1L))
  w <- w / sum(w)
  .same_convolve(x, w)
}

#' Alpha-trimmed mean filter
#'
#' Sliding-window mean after trimming `alpha` fraction of values from
#' each tail of the sorted window. Robust to both Gaussian and impulsive
#' noise; reduces to the mean filter when `alpha = 0` and to the median
#' filter as `alpha -> 0.5`.
#'
#' @param x Numeric vector.
#' @param window Window length. Default 5.
#' @param alpha Trim fraction (0 <= alpha < 0.5). Default 0.2.
#' @return Filtered vector, length(x).
#' @references Rangayyan & Krishnan (2015), Ch. 3, sec. 3.4.
#' @export
morie_dsp_alpha_trimmed_mean <- function(x, window = 5L, alpha = 0.2) {
  x <- as.numeric(x)
  window <- as.integer(window)
  if (window < 1L) stop("window must be >= 1")
  if (alpha < 0 || alpha >= 0.5) stop("alpha must be in [0, 0.5)")
  n <- length(x)
  if (n == 0L) return(numeric(0))
  half <- window %/% 2L
  trim <- as.integer(alpha * window)
  y <- numeric(n)
  for (i in seq_len(n)) {
    s <- max(1L, i - half)
    e <- min(n, i + half)
    seg <- sort(x[s:e])
    if (trim > 0L && length(seg) > 2L * trim) {
      seg <- seg[(trim + 1L):(length(seg) - trim)]
    }
    y[i] <- mean(seg)
  }
  y
}

#' @noRd
.morie_dsp_cpp_ok <- function(name) {
  exists(name, envir = asNamespace("morie"), inherits = FALSE)
}

#' Median filter
#'
#' Sliding-window median. Robust to impulsive (salt-and-pepper) noise.
#'
#' @param x Numeric vector.
#' @param kernel_size Odd positive integer kernel length. Default 5.
#' @return Filtered vector, length(x).
#' @references Rangayyan & Krishnan (2015), Ch. 3, sec. 3.4.
#' @export
morie_dsp_median_filter <- function(x, kernel_size = 5L) {
  x <- as.numeric(x)
  k <- as.integer(kernel_size)
  if (k < 1L) stop("kernel_size must be >= 1")
  if (k %% 2L == 0L) k <- k + 1L
  half <- k %/% 2L
  n <- length(x)
  if (n == 0L) return(numeric(0))
  if (.morie_dsp_cpp_ok("morie_dsp_median_filter_cpp")) {
    return(morie_dsp_median_filter_cpp(x, k))
  }
  # Zero-pad to mimic scipy.signal.medfilt's default behaviour.
  padded <- c(rep(0, half), x, rep(0, half))
  vapply(seq_len(n), function(i) stats::median(padded[i:(i + k - 1L)]),
         numeric(1))
}

#' Wiener filter (frequency domain)
#'
#' Classical scalar Wiener gain in the rFFT domain:
#' `H(f) = Pxx(f) / (Pxx(f) + Pnn(f))`. With `noise_psd = NULL` the
#' noise PSD is assumed flat at `noise_fraction * mean(Pxx)`.
#'
#' @param x Numeric vector (signal + noise).
#' @param noise_psd Optional noise PSD, length `floor(N/2)+1`.
#' @param noise_fraction Fallback flat-noise scale. Default 0.1.
#' @return Filtered vector, length(x).
#' @references Rangayyan & Krishnan (2015), Ch. 3, sec. 3.5.
#' @export
morie_dsp_wiener_filter <- function(x, noise_psd = NULL,
                                    noise_fraction = 0.1) {
  x <- as.numeric(x)
  n <- length(x)
  X <- stats::fft(x)
  half <- n %/% 2L + 1L
  X_half <- X[seq_len(half)]
  Pxx <- Mod(X_half)^2
  if (is.null(noise_psd)) {
    noise_psd <- rep(noise_fraction * mean(Pxx), length(Pxx))
  }
  H <- Pxx / (Pxx + noise_psd)
  Y_half <- H * X_half
  # Reconstitute full Hermitian spectrum then inverse FFT.
  if (n %% 2L == 0L) {
    Y_full <- c(Y_half, Conj(rev(Y_half[2:(half - 1L)])))
  } else {
    Y_full <- c(Y_half, Conj(rev(Y_half[2:half])))
  }
  Re(stats::fft(Y_full, inverse = TRUE) / n)
}

#' LMS adaptive filter (Widrow-Hoff)
#'
#' Least-mean-squares adaptive transversal filter. Returns the filter
#' output `y` and instantaneous error `e = d - y`. Coefficient update:
#' `w <- w + 2 * mu * e[i] * x_seg`.
#'
#' @param x Input (reference) vector.
#' @param d Desired vector, same length as `x`.
#' @param order Filter order (taps). Default 16.
#' @param mu Step size. Default 0.01.
#' @return List with elements `y` and `e`, both length(x).
#' @references Rangayyan & Krishnan (2015), Ch. 3, sec. 3.6;
#'   Widrow & Stearns (1985).
#' @export
morie_dsp_lms <- function(x, d, order = 16L, mu = 0.01) {
  x <- as.numeric(x)
  d <- as.numeric(d)
  if (length(x) != length(d)) stop("x and d must be the same length")
  order <- as.integer(order)
  if (.morie_dsp_cpp_ok("morie_dsp_lms_cpp")) {
    return(morie_dsp_lms_cpp(x, d, order, as.numeric(mu)))
  }
  n <- length(x)
  w <- numeric(order)
  y <- numeric(n)
  e <- numeric(n)
  for (i in seq.int(order + 1L, n)) {
    seg <- rev(x[(i - order):(i - 1L)])
    y[i] <- sum(w * seg)
    e[i] <- d[i] - y[i]
    w <- w + 2 * mu * e[i] * seg
  }
  list(y = y, e = e)
}

#' NLMS adaptive filter
#'
#' Normalised LMS: divides the step by the instantaneous input power
#' `x_seg' x_seg + eps`, giving robust convergence over a wider range
#' of input scales.
#'
#' @inheritParams morie_dsp_lms
#' @param mu Normalised step size (typically 0 < mu < 2). Default 0.5.
#' @param eps Power-floor for division. Default 1e-8.
#' @return List with `y`, `e`.
#' @references Rangayyan & Krishnan (2015), Ch. 3, sec. 3.6.
#' @export
morie_dsp_nlms <- function(x, d, order = 16L, mu = 0.5, eps = 1e-8) {
  x <- as.numeric(x)
  d <- as.numeric(d)
  if (length(x) != length(d)) stop("x and d must be the same length")
  order <- as.integer(order)
  if (.morie_dsp_cpp_ok("morie_dsp_nlms_cpp")) {
    return(morie_dsp_nlms_cpp(x, d, order, as.numeric(mu), as.numeric(eps)))
  }
  n <- length(x)
  w <- numeric(order)
  y <- numeric(n)
  e <- numeric(n)
  for (i in seq.int(order + 1L, n)) {
    seg <- rev(x[(i - order):(i - 1L)])
    nrm <- sum(seg * seg) + eps
    y[i] <- sum(w * seg)
    e[i] <- d[i] - y[i]
    w <- w + (mu / nrm) * e[i] * seg
  }
  list(y = y, e = e)
}

#' RLS adaptive filter
#'
#' Recursive least-squares with forgetting factor `lam` and initial
#' inverse-correlation `P0 = delta * I`. Faster convergence than LMS
#' at the cost of O(order^2) per sample.
#'
#' @inheritParams morie_dsp_lms
#' @param lam Forgetting factor in (0, 1]. Default 0.99.
#' @param delta Initial P diagonal. Default 100.
#' @return List with `y`, `e`.
#' @references Rangayyan & Krishnan (2015), Ch. 3, sec. 3.6;
#'   Haykin (2002).
#' @export
morie_dsp_rls <- function(x, d, order = 16L, lam = 0.99, delta = 100) {
  x <- as.numeric(x)
  d <- as.numeric(d)
  if (length(x) != length(d)) stop("x and d must be the same length")
  order <- as.integer(order)
  if (.morie_dsp_cpp_ok("morie_dsp_rls_cpp")) {
    return(morie_dsp_rls_cpp(x, d, order, as.numeric(lam), as.numeric(delta)))
  }
  n <- length(x)
  w <- numeric(order)
  P <- delta * diag(order)
  y <- numeric(n)
  e <- numeric(n)
  for (i in seq.int(order + 1L, n)) {
    seg <- rev(x[(i - order):(i - 1L)])
    y[i] <- sum(w * seg)
    e[i] <- d[i] - y[i]
    Pseg <- as.numeric(P %*% seg)
    denom <- lam + sum(seg * Pseg)
    k <- Pseg / denom
    w <- w + k * e[i]
    P <- (P - outer(k, as.numeric(seg %*% P))) / lam
  }
  list(y = y, e = e)
}

#' IIR notch filter (single frequency)
#'
#' Wraps `signal::butter` style IIR notch via the `signal` package's
#' `iirnotch` / `filtfilt`. Falls back to a stop with `NotYetPorted` if
#' `signal` is unavailable.
#'
#' @param x Numeric vector.
#' @param freq Notch centre frequency (Hz).
#' @param fs Sampling frequency (Hz).
#' @param q Quality factor. Default 30.
#' @return Filtered vector, length(x).
#' @references Rangayyan & Krishnan (2015), Ch. 3, sec. 3.7.
#' @export
morie_dsp_notch <- function(x, freq, fs, q = 30) {
  if (!requireNamespace("signal", quietly = TRUE)) {
    stop("morie_dsp_notch requires the 'signal' package")
  }
  if (!exists("iirnotch", where = asNamespace("signal"))) {
    # signal lacks iirnotch on some CRAN builds; construct by hand.
    w0 <- freq / (fs / 2)
    bw <- w0 / q
    r <- 1 - 3 * bw
    cosw0 <- cos(pi * w0)
    b <- c(1, -2 * cosw0, 1) * (1 + r^2) / 2
    a <- c(1, -2 * r * cosw0, r^2)
    return(as.numeric(signal::filtfilt(b, a, x)))
  }
  ba <- get("iirnotch", envir = asNamespace("signal"))(freq / (fs / 2), q)
  as.numeric(signal::filtfilt(ba$b, ba$a, x))
}

#' Comb filter built from cascaded notches
#'
#' Successively notches the fundamental and its first `n_harmonics`
#' harmonics. Each notch below Nyquist is applied; aliased harmonics
#' are skipped.
#'
#' @param x Numeric vector.
#' @param fundamental Fundamental frequency (Hz), e.g. 60 for North
#'   American mains.
#' @param fs Sampling frequency (Hz).
#' @param n_harmonics Number of harmonics to cancel. Default 5.
#' @param q Quality factor per notch. Default 30.
#' @return Filtered vector, length(x).
#' @references Rangayyan & Krishnan (2015), Ch. 3, sec. 3.7.
#' @export
morie_dsp_comb <- function(x, fundamental, fs, n_harmonics = 5L, q = 30) {
  y <- as.numeric(x)
  for (h in seq_len(n_harmonics)) {
    f <- fundamental * h
    if (f < fs / 2) y <- morie_dsp_notch(y, f, fs, q)
  }
  y
}

#' Matched filter (time-reversed template correlator)
#'
#' Optimal linear filter against white Gaussian noise: convolves `x`
#' with the time-reversed template, normalised by `||template||`.
#'
#' @param x Numeric vector.
#' @param template Reference waveform.
#' @return Matched-filter output, length(x).
#' @references Rangayyan & Krishnan (2015), Ch. 3, sec. 3.5.
#' @export
morie_dsp_matched <- function(x, template) {
  x <- as.numeric(x)
  template <- as.numeric(template)
  nrm <- sqrt(sum(template^2))
  if (nrm == 0) stop("template has zero norm")
  # correlate(x, template, "same") = convolve(x, rev(template), "same")
  .same_convolve(x, rev(template)) / nrm
}

#' Ensemble average over fixed-length segments
#'
#' Averages a `(n_segments, n_samples)` matrix down its rows. Standard
#' synchronous-averaging recipe for repeated stimulus responses (e.g.
#' evoked potentials).
#'
#' @param segments Numeric matrix (rows = trials, cols = samples).
#' @return Row mean as a length-`ncol` vector.
#' @references Rangayyan & Krishnan (2015), Ch. 3, sec. 3.3.
#' @export
morie_dsp_ensemble_average <- function(segments) {
  segments <- as.matrix(segments)
  colMeans(segments)
}

#' Synchronized average around trigger indices
#'
#' Extracts length-`window` epochs centred on each `trigger_indices`
#' value and averages them. Out-of-bounds triggers are dropped.
#'
#' @param x Numeric vector.
#' @param trigger_indices Integer vector of 1-based event indices.
#' @param window Epoch length (centred). Default 100.
#' @return Mean epoch, length `window`.
#' @references Rangayyan & Krishnan (2015), Ch. 3, sec. 3.3.
#' @export
morie_dsp_synchronized_average <- function(x, trigger_indices,
                                           window = 100L) {
  x <- as.numeric(x)
  window <- as.integer(window)
  half <- window %/% 2L
  segs <- list()
  for (idx in trigger_indices) {
    s <- idx - half
    e <- idx + half - 1L
    if (s >= 1L && e <= length(x)) {
      segs[[length(segs) + 1L]] <- x[s:e]
    }
  }
  if (length(segs) == 0L) return(numeric(window))
  rowMeans(do.call(cbind, segs))
}

#' Signal-to-noise ratio estimate (dB)
#'
#' `10 * log10(mean(signal^2) / mean(noise^2))`. Returns `Inf` when
#' noise power is zero.
#'
#' @param signal Numeric vector.
#' @param noise Numeric vector.
#' @return SNR in dB.
#' @references Rangayyan & Krishnan (2015), Ch. 3, sec. 3.2.
#' @export
morie_dsp_snr <- function(signal, noise) {
  ps <- mean(signal^2)
  pn <- mean(noise^2)
  if (pn == 0) return(Inf)
  10 * log10(ps / pn)
}

#' SNR improvement attributable to a filter (dB)
#'
#' Compares SNR of (clean vs. clean-noisy) before filtering with SNR
#' of (clean vs. clean-filtered) after; positive values mean the
#' filter reduced noise relative to clean.
#'
#' @param x_noisy Observed noisy vector.
#' @param x_clean Clean reference.
#' @param x_filtered Filter output.
#' @return Delta SNR in dB.
#' @references Rangayyan & Krishnan (2015), Ch. 3, sec. 3.2.
#' @export
morie_dsp_snr_improvement <- function(x_noisy, x_clean, x_filtered) {
  morie_dsp_snr(x_clean, x_filtered - x_clean) -
    morie_dsp_snr(x_clean, x_noisy - x_clean)
}

#' Turning-points stationarity test
#'
#' Counts strict turning points and z-scores against the i.i.d.
#' expectation `2(n-2)/3`. `|z| < 1.96` is consistent with weak
#' stationarity at the 5 percent level.
#'
#' @param x Numeric vector.
#' @return List with `turning_points`, `expected`, `z_statistic`,
#'   `stationary`.
#' @references Rangayyan & Krishnan (2015), Ch. 3, sec. 3.2.
#' @export
morie_dsp_turning_points <- function(x) {
  x <- as.numeric(x)
  d <- diff(x)
  turns <- sum(d[-length(d)] * d[-1L] < 0)
  n <- length(x)
  expected <- 2 * (n - 2) / 3
  variance <- (16 * n - 29) / 90
  z <- (turns - expected) / sqrt(variance)
  list(turning_points = as.integer(turns), expected = expected,
       z_statistic = z, stationary = abs(z) < 1.96)
}

#' Coefficient of variation
#'
#' `sd(x) / |mean(x)|`. `Inf` when the mean is zero.
#'
#' @param x Numeric vector.
#' @return Scalar.
#' @references Rangayyan & Krishnan (2015), Ch. 3.
#' @export
morie_dsp_cv <- function(x) {
  m <- mean(x)
  if (m == 0) return(Inf)
  stats::sd(x) / abs(m)
}

#' Solve the Wiener-Hopf normal equations
#'
#' Returns `w = solve(Rxx, rxd)`. Used as the optimal-FIR Wiener
#' solution; equivalent to `lm.fit` on Toeplitz inputs.
#'
#' @param Rxx Symmetric autocorrelation matrix (order x order).
#' @param rxd Cross-correlation vector (length order).
#' @return Optimal tap-weight vector.
#' @references Rangayyan & Krishnan (2015), Ch. 3, sec. 3.5.
#' @export
morie_dsp_wiener_hopf <- function(Rxx, rxd) {
  solve(Rxx, rxd)
}

#' Normalised cross-correlation up to a maximum lag
#'
#' Centres both inputs (subtracts the mean) and divides by the
#' geometric mean of their L2 norms. Returns lags `-max_lag .. +max_lag`.
#'
#' @param x Numeric vector.
#' @param y Numeric vector, same length as `x`.
#' @param max_lag Maximum lag (defaults to `length(x) - 1`).
#' @return Numeric vector of length `2 * max_lag + 1`.
#' @references Rangayyan & Krishnan (2015), Ch. 3, sec. 3.4.
#' @export
morie_dsp_cross_correlation <- function(x, y, max_lag = NULL) {
  x <- as.numeric(x)
  y <- as.numeric(y)
  if (length(x) != length(y)) stop("x and y must be the same length")
  n <- length(x)
  if (is.null(max_lag)) max_lag <- n - 1L
  max_lag <- as.integer(max_lag)
  if (.morie_dsp_cpp_ok("morie_dsp_cross_correlation_cpp")) {
    return(morie_dsp_cross_correlation_cpp(x, y, max_lag))
  }
  xc <- x - mean(x)
  yc <- y - mean(y)
  nrm <- sqrt(sum(xc^2) * sum(yc^2))
  # r(tau) = sum_t xc(t) * yc(t + tau) over valid overlap;
  # returns 2*max_lag + 1 values, lag 0 sits at the centre index.
  lags <- seq.int(-max_lag, max_lag)
  out <- vapply(lags, function(tau) {
    if (tau >= 0L) {
      i <- seq.int(1L, n - tau)
      sum(xc[i] * yc[i + tau])
    } else {
      i <- seq.int(1L - tau, n)
      sum(xc[i] * yc[i + tau])
    }
  }, numeric(1L))
  if (nrm > 0) out <- out / nrm
  out
}

#' Even-odd decomposition of a finite signal
#'
#' Splits `x` into its even (symmetric) and odd (anti-symmetric)
#' components about the centre: `x_even = (x + rev(x))/2`,
#' `x_odd = (x - rev(x))/2`.
#'
#' @param x Numeric vector.
#' @return List with `even` and `odd`.
#' @references Rangayyan & Krishnan (2015), Ch. 3.
#' @export
morie_dsp_even_odd <- function(x) {
  x <- as.numeric(x)
  xr <- rev(x)
  list(even = (x + xr) / 2, odd = (x - xr) / 2)
}

# ---- internal helpers -------------------------------------------------

# "same"-mode convolution matching numpy.convolve(x, k, "same").
.same_convolve <- function(x, k) {
  n <- length(x)
  m <- length(k)
  full <- stats::convolve(x, rev(k), type = "open")  # numpy "full"
  out_len <- n
  start <- (length(full) - out_len) %/% 2L + 1L
  full[start:(start + out_len - 1L)]
}

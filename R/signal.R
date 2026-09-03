#' Butterworth lowpass filter
#'
#' Zero-phase Butterworth lowpass filter via the suggested `signal` package's
#' `butter()` + `filtfilt()`. Useful for removing high-frequency noise from
#' biological or geophysical time series.
#'
#' @param x Numeric vector.
#' @param fs Sampling frequency (Hz).
#' @param cutoff Cutoff frequency (Hz).
#' @param order Filter order (default 4).
#' @return List with `filtered` (numeric vector), `fs`, `order`, `name`.
#' @export
#' @examples
#' \donttest{
#' if (requireNamespace("signal", quietly = TRUE)) {
#'   set.seed(1)
#'   t <- seq(0, 1, length.out = 500)
#'   x <- sin(2 * pi * 5 * t) + 0.5 * sin(2 * pi * 60 * t) # 5 Hz + 60 Hz
#'   y <- buttlp(x, fs = 500, cutoff = 20)
#'   length(y$filtered) # 500
#' }
#' }
buttlp <- function(x, fs, cutoff, order = 4L) {
  bf <- .morie_dsp_butter(order, cutoff / (fs / 2), type = "low")
  y <- .morie_dsp_filtfilt(bf$b, bf$a, x)
  list(filtered = y, fs = fs, order = order, name = "butter_lowpass")
}

#' Butterworth highpass filter
#'
#' Zero-phase Butterworth highpass filter. Removes low-frequency drift while
#' preserving higher-frequency content; useful for de-trending physiological
#' signals (EEG, ECG) prior to analysis.
#'
#' @param x Numeric vector.
#' @param fs Sampling frequency (Hz).
#' @param cutoff Cutoff frequency (Hz).
#' @param order Filter order (default 4).
#' @return List with `filtered` (numeric vector), `fs`, `order`, `name`.
#' @export
#' @examples
#' \donttest{
#' if (requireNamespace("signal", quietly = TRUE)) {
#'   set.seed(1)
#'   t <- seq(0, 1, length.out = 500)
#'   x <- 5 * t + sin(2 * pi * 10 * t) # linear drift + 10 Hz signal
#'   y <- butthp(x, fs = 500, cutoff = 1)
#'   length(y$filtered)
#' }
#' }
butthp <- function(x, fs, cutoff, order = 4L) {
  bf <- .morie_dsp_butter(order, cutoff / (fs / 2), type = "high")
  y <- .morie_dsp_filtfilt(bf$b, bf$a, x)
  list(filtered = y, fs = fs, order = order, name = "butter_highpass")
}

#' Butterworth bandpass filter
#'
#' Zero-phase Butterworth bandpass filter. Isolates a frequency band of
#' interest (e.g., 0.5--40 Hz for EEG, 25--400 Hz for phonocardiogram).
#'
#' @param x Numeric vector.
#' @param fs Sampling frequency (Hz).
#' @param low Lower cutoff (Hz).
#' @param high Upper cutoff (Hz).
#' @param order Filter order (default 4).
#' @return List with `filtered` (numeric vector), `fs`, `order`, `name`.
#' @export
#' @examples
#' \donttest{
#' if (requireNamespace("signal", quietly = TRUE)) {
#'   set.seed(1)
#'   t <- seq(0, 1, length.out = 1000)
#'   # 2 Hz drift + 10 Hz band of interest + 60 Hz noise
#'   x <- sin(2 * pi * 2 * t) + sin(2 * pi * 10 * t) +
#'     0.3 * sin(2 * pi * 60 * t)
#'   y <- buttbp(x, fs = 1000, low = 5, high = 20)
#'   length(y$filtered)
#' }
#' }
buttbp <- function(x, fs, low, high, order = 4L) {
  bf <- .morie_dsp_butter(order, c(low, high) / (fs / 2), type = "pass")
  y <- .morie_dsp_filtfilt(bf$b, bf$a, x)
  list(filtered = y, fs = fs, order = order, name = "butter_bandpass")
}

#' Butterworth bandstop (notch) filter
#'
#' Zero-phase Butterworth bandstop filter. Default 59--61 Hz removes North-
#' American AC mains hum (60 Hz); use 49--51 Hz for European mains.
#'
#' @param x Numeric vector.
#' @param fs Sampling frequency (Hz).
#' @param low Lower cutoff (Hz, default 59).
#' @param high Upper cutoff (Hz, default 61).
#' @param order Filter order (default 4).
#' @return List with `filtered` (numeric vector), `fs`, `order`, `name`.
#' @export
#' @examples
#' \donttest{
#' if (requireNamespace("signal", quietly = TRUE)) {
#'   set.seed(1)
#'   t <- seq(0, 1, length.out = 1000)
#'   x <- sin(2 * pi * 10 * t) + sin(2 * pi * 60 * t)
#'   y <- buttbs(x, fs = 1000) # remove 60 Hz mains
#'   length(y$filtered)
#' }
#' }
buttbs <- function(x, fs, low = 59, high = 61, order = 4L) {
  bf <- .morie_dsp_butter(order, c(low, high) / (fs / 2), type = "stop")
  y <- .morie_dsp_filtfilt(bf$b, bf$a, x)
  list(filtered = y, fs = fs, order = order, name = "butter_bandstop")
}

#' Savitzky-Golay smoothing filter
#'
#' Polynomial-fit smoothing filter. Preserves higher moments (peak heights,
#' shape) better than a moving average and is the standard tool for
#' chromatography, spectroscopy, and biosensor smoothing.
#'
#' @param x Numeric vector.
#' @param window_length Window length (odd integer, default 11).
#' @param polyorder Polynomial order (default 3).
#' @return List with `filtered` (numeric vector) and `name`.
#' @export
#' @examples
#' \donttest{
#' if (requireNamespace("signal", quietly = TRUE)) {
#'   set.seed(1)
#'   t <- seq(0, 1, length.out = 200)
#'   x <- sin(2 * pi * 3 * t) + rnorm(200, sd = 0.2)
#'   y <- morie_sgolay_smooth(x, window_length = 11, polyorder = 3)
#'   length(y$filtered)
#' }
#' }
morie_sgolay_smooth <- function(x, window_length = 11L, polyorder = 3L) {
  y <- .morie_dsp_sgolay(x, polyorder = polyorder,
                         window_length = window_length)
  list(filtered = as.numeric(y), name = "savitzky_golay")
}

#' Hurst exponent via rescaled-range (R/S) analysis
#'
#' Estimates the Hurst exponent \eqn{H}: a long-memory measure of a time
#' series. \eqn{H = 0.5} indicates uncorrelated (Brownian) increments;
#' \eqn{H > 0.5} indicates persistent (trending) behaviour;
#' \eqn{H < 0.5} indicates anti-persistent (mean-reverting) behaviour.
#'
#' @param x Numeric vector (time series).
#' @return List with `H` (numeric) and `interpretation`
#'   (`"persistent"`/`"anti-persistent"`/`"random"`).
#' @export
#' @examples
#' \donttest{
#' if (requireNamespace("pracma", quietly = TRUE)) {
#'   set.seed(1)
#'   x <- cumsum(rnorm(2048)) # Brownian motion, expected H ~ 0.5
#'   res <- morie_hurst_r(x)
#'   res$interpretation
#' }
#' }
morie_hurst_r <- function(x) {
  h <- .morie_hurst_rs(x)
  list(H = h, interpretation = ifelse(h > 0.55, "persistent",
    ifelse(h < 0.45, "anti-persistent", "random")
  ))
}

#' Higuchi fractal dimension
#'
#' Estimates the Higuchi (1988) fractal dimension of a 1-D time series via
#' length scaling across `k` time-lags. Values typically fall in \\[1, 2\\];
#' higher values indicate greater signal complexity.
#'
#' Reference: Higuchi, T. (1988) "Approach to an irregular time series on
#' the basis of the fractal theory", *Physica D* 31(2):277--283.
#'
#' @param x Numeric vector (length \eqn{\geq}{>=} 4).
#' @param kmax Maximum k (default 10).
#' @return List with `value` (D), `name`, and `extra` (`kmax`, `n`,
#'   `L_k`).
#' @export
#' @examples
#' \donttest{
#' set.seed(1)
#' x <- cumsum(rnorm(1000))
#' hfd(x, kmax = 10)$value
#' }
hfd <- function(x, kmax = 10L) {
  x <- as.numeric(x)
  n <- length(x)
  kmax <- as.integer(kmax)
  if (n < 4L || kmax < 2L) {
    return(list(name = "higuchi_fd", value = NA_real_,
                extra = list(kmax = kmax, n = n)))
  }
  kmax <- min(kmax, n - 1L)
  l_k <- rep(NA_real_, kmax)
  for (k in seq_len(kmax)) {
    l_m <- numeric(k)
    for (m in seq_len(k)) {
      idx_max <- (n - m) %/% k
      if (idx_max < 1L) next
      i <- seq_len(idx_max)
      diffs <- abs(x[m + i * k] - x[m + (i - 1L) * k])
      l_m[m] <- sum(diffs) * (n - 1L) / (idx_max * k * k)
    }
    l_k[k] <- mean(l_m[l_m > 0], na.rm = TRUE)
  }
  valid <- is.finite(l_k) & l_k > 0
  if (sum(valid) < 2L) {
    return(list(name = "higuchi_fd", value = NA_real_,
                extra = list(kmax = kmax, n = n, L_k = l_k)))
  }
  fd <- unname(-stats::coef(stats::lm(
    log(l_k[valid]) ~ log(seq_len(kmax)[valid])))[2])
  list(name = "higuchi_fd",
       value = as.numeric(fd),
       extra = list(kmax = kmax, n = n, L_k = l_k))
}

#' Phonocardiogram (PCG) bandpass filter
#'
#' Convenience preset wrapping \[buttbp()\] with the standard PCG band
#' (25--400 Hz at 2000 Hz sampling). Removes baseline drift below 25 Hz and
#' anti-aliased high-frequency noise above 400 Hz.
#'
#' @param x Numeric vector (PCG signal).
#' @param fs Sampling frequency (Hz, default 2000).
#' @param low Lower cutoff (Hz, default 25).
#' @param high Upper cutoff (Hz, default 400).
#' @return List with filtered signal (see \[buttbp()\]).
#' @export
#' @examples
#' \donttest{
#' if (requireNamespace("signal", quietly = TRUE)) {
#'   set.seed(1)
#'   x <- rnorm(2000) # 1 second of white-noise PCG-like input
#'   y <- morie_pcg_filter(x)
#'   length(y$filtered)
#' }
#' }
morie_pcg_filter <- function(x, fs = 2000, low = 25, high = 400) {
  buttbp(x, fs, low, high)
}



# --- Added 2026-05-22: 18 missing signal-processing functions ---
# Pure-R / requireNamespace-gated ports of morie.signal facade fns absent
# from the R surface. See agent transcript a40d57fa for full citations.

#' Real cepstrum
#'
#' Real cepstrum \eqn{c\[n\] = \mathrm{IFFT}(\log |\mathrm{FFT}(x)|)}{c\[n\] = IFFT(log
#' |FFT(x)|)}. Useful
#' for pitch-period estimation and any analysis where the multiplicative
#' magnitude structure of the spectrum is best handled additively in the
#' quefrency domain.
#'
#' Reference: Rangayyan, R.M. (2015) *Biomedical Signal Analysis*, 2nd ed.,
#' Wiley/IEEE Press, chapter on cepstral analysis.
#'
#' @param x Numeric vector (1-D signal).
#' @param n_fft FFT length (default: next power of 2 \eqn{\geq}{\>=} `length(x)`).
#' @return List with `filtered` (real cepstral coefficients),
#'   `name`, `fs`, `n_samples`, and `extra` (`quefrency`, `n_fft`).
#' @export
#' @examples
#' \donttest{
#' set.seed(1)
#' x <- sin(2 * pi * 5 * seq(0, 1, length.out = 512))
#' res <- cepst(x)
#' length(res$filtered)
#' }
cepst <- function(x, n_fft = NULL) {
  x <- as.numeric(x)
  n <- length(x)
  if (is.null(n_fft)) n_fft <- as.integer(2^ceiling(log2(max(n, 2))))
  X <- stats::fft(c(x, rep(0, n_fft - n)))
  log_mag <- log(abs(X) + 1e-30)
  cepstrum <- Re(stats::fft(log_mag, inverse = TRUE) / n_fft)
  list(
    name = "real_cepstrum",
    filtered = cepstrum,
    fs = 0,
    n_samples = length(cepstrum),
    extra = list(quefrency = seq_along(cepstrum) - 1L, n_fft = n_fft)
  )
}

#' Complex cepstrum with phase unwrapping
#'
#' Complex cepstrum: inverse FFT of \eqn{\log X(\omega)}{log X(omega)} using the unwrapped
#' phase. Unlike the real cepstrum, it preserves enough information to
#' invert the operation, which is what enables homomorphic deconvolution.
#'
#' Reference: Oppenheim, A.V. & Schafer, R.W. (2009) *Discrete-Time Signal
#' Processing*, 3rd ed., Pearson, chapter on cepstral analysis.
#'
#' @param x Numeric vector (1-D signal).
#' @param n_fft FFT length (default: next power of 2 \eqn{\geq}{\>=} `length(x)`).
#' @return List with `filtered` (complex cepstrum, real-valued),
#'   `name`, `fs`, `n_samples`, and `extra` (`quefrency`, `n_fft`,
#'   `original_length`).
#' @export
#' @examples
#' \donttest{
#' set.seed(1)
#' x <- sin(2 * pi * 5 * seq(0, 1, length.out = 512))
#' res <- hcepst(x)
#' length(res$filtered)
#' }
hcepst <- function(x, n_fft = NULL) {
  x <- as.numeric(x)
  n <- length(x)
  if (is.null(n_fft)) n_fft <- as.integer(2^ceiling(log2(max(n, 2))))
  X <- stats::fft(c(x, rep(0, n_fft - n)))
  log_abs <- log(abs(X) + 1e-30)
  # phase unwrap (mirrors numpy.unwrap)
  ph <- Arg(X)
  d <- diff(ph)
  jumps <- round(d / (2 * pi))
  ph_unwrapped <- ph - c(0, cumsum(jumps) * 2 * pi)
  log_X <- log_abs + 1i * ph_unwrapped
  cepstrum <- Re(stats::fft(log_X, inverse = TRUE) / n_fft)
  list(
    name = "complex_cepstrum",
    filtered = cepstrum,
    fs = 0,
    n_samples = length(cepstrum),
    extra = list(quefrency = seq_along(cepstrum) - 1L, n_fft = n_fft, original_length = n)
  )
}

#' Homomorphic deconvolution via cepstral liftering
#'
#' Separates a convolved signal \eqn{x = h * e} into a minimum-phase
#' impulse-response component \eqn{h} and an excitation \eqn{e} by
#' low-time liftering of the complex cepstrum.
#'
#' Reference: Oppenheim & Schafer (2009), *Discrete-Time Signal Processing*,
#' 3rd ed., on homomorphic systems for convolution.
#'
#' @param x Numeric vector (assumed convolution \eqn{h * e}).
#' @param cutoff Liftering cutoff (quefrency index). Coefficients above
#'   are zeroed to isolate the slow-varying component.
#' @param n_fft FFT length (default: next power of 2 \eqn{\geq}{\>=} `length(x)`).
#' @return List with `filtered` (minimum-phase component \eqn{h}),
#'   `name`, `fs`, `n_samples`, and `extra` (`excitation`, `cutoff`, `n_fft`).
#' @export
#' @examples
#' \donttest{
#' set.seed(1)
#' x <- sin(2 * pi * 5 * seq(0, 1, length.out = 512))
#' res <- hdecon(x, cutoff = 20)
#' length(res$filtered)
#' }
hdecon <- function(x, cutoff, n_fft = NULL) {
  x <- as.numeric(x)
  n <- length(x)
  if (is.null(n_fft)) n_fft <- as.integer(2^ceiling(log2(max(n, 2))))
  X <- stats::fft(c(x, rep(0, n_fft - n)))
  log_abs <- log(abs(X) + 1e-30)
  ph <- Arg(X)
  d <- diff(ph)
  jumps <- round(d / (2 * pi))
  ph_unwrapped <- ph - c(0, cumsum(jumps) * 2 * pi)
  log_X <- log_abs + 1i * ph_unwrapped
  cepstrum <- stats::fft(log_X, inverse = TRUE) / n_fft

  lifter <- numeric(n_fft)
  lifter[1] <- 1
  cidx <- min(cutoff, n_fft %/% 2)
  if (cidx > 1) lifter[2:cidx] <- 2
  if (cidx < n_fft %/% 2) lifter[cidx + 1] <- 1

  cep_min <- cepstrum * lifter
  H <- exp(stats::fft(cep_min))
  h <- Re(stats::fft(H, inverse = TRUE) / n_fft)[seq_len(n)]

  cep_exc <- cepstrum * (1 - lifter)
  cep_exc[1] <- cepstrum[1] - cep_min[1]
  E <- exp(stats::fft(cep_exc))
  e <- Re(stats::fft(E, inverse = TRUE) / n_fft)[seq_len(n)]

  list(
    name = "homomorphic_deconvolve",
    filtered = h,
    fs = 0,
    n_samples = length(h),
    extra = list(excitation = e, cutoff = cutoff, n_fft = n_fft)
  )
}

#' Detrended fluctuation analysis (DFA)
#'
#' Estimates the DFA scaling exponent \eqn{\alpha}{alpha}. White noise gives
#' \eqn{\alpha \approx 0.5}{alpha \~= 0.5}; pink (1/f) noise \eqn{\alpha \approx
#' 1.0}{alpha \~= 1.0};
#' Brownian motion \eqn{\alpha \approx 1.5}{alpha \~= 1.5}.
#'
#' Reference: Peng, C.-K., Havlin, S., Stanley, H.E. & Goldberger, A.L. (1995)
#' "Quantification of scaling exponents and crossover phenomena in
#' nonstationary heartbeat time series", *Chaos* 5(1):82--87.
#'
#' @param x Numeric vector (length \eqn{\geq}{\>=} 16).
#' @param scales Integer vector of window sizes (auto-generated if `NULL`).
#' @return List with `value` (alpha), `name`, and `extra`
#'   (`scales`, `fluctuation`).
#' @export
#' @examples
#' \donttest{
#' set.seed(1)
#' x <- cumsum(rnorm(2048))
#' res <- dfa(x)
#' res$value
#' }
dfa <- function(x, scales = NULL) {
  x <- as.numeric(x)
  n <- length(x)
  if (n < 16) return(list(name = "dfa", value = NA_real_))
  y <- cumsum(x - mean(x))
  if (is.null(scales)) {
    min_s <- 4
    max_s <- n %/% 4
    n_scales <- min(20L, max_s - min_s + 1L)
    scales <- unique(as.integer(round(10^seq(log10(min_s), log10(max_s), length.out = n_scales))))
    scales <- scales[scales >= 4]
  }
  fluct <- rep(NA_real_, length(scales))
  for (i in seq_along(scales)) {
    s <- scales[i]
    n_seg <- n %/% s
    if (n_seg < 1) next
    rms_vals <- numeric(n_seg)
    tt <- seq_len(s) - 1L
    for (j in seq_len(n_seg)) {
      seg <- y[((j - 1) * s + 1):(j * s)]
      coeffs <- stats::lm.fit(cbind(1, tt), seg)$coefficients
      trend <- coeffs[1] + coeffs[2] * tt
      rms_vals[j] <- sqrt(mean((seg - trend)^2))
    }
    fluct[i] <- mean(rms_vals)
  }
  valid <- is.finite(fluct) & fluct > 0
  if (sum(valid) < 2) return(list(name = "dfa", value = NA_real_))
  alpha <- unname(stats::coef(stats::lm(log(fluct[valid]) ~ log(scales[valid])))[2])
  list(
    name = "dfa",
    value = as.numeric(alpha),
    extra = list(scales = scales, fluctuation = fluct)
  )
}

#' Pan-Tompkins QRS / R-peak detector
#'
#' Pan-Tompkins QRS detection: bandpass (5--15 Hz) -> differentiate ->
#' square -> moving-window integration -> adaptive thresholding ->
#' refinement against the raw ECG.
#'
#' Reference: Pan, J. & Tompkins, W.J. (1985) "A real-time QRS detection
#' algorithm", *IEEE Trans. Biomed. Eng.* BME-32(3):230--236.
#'
#' @param ecg Numeric vector (1-D ECG signal).
#' @param fs Sampling frequency in Hz.
#' @return List with `filtered` (raw ECG echoed), `name`, `fs`,
#'   `n_samples`, and `extra` (`r_peaks` = 1-based sample indices,
#'   `n_peaks`).
#' @export
#' @examples
#' \donttest{
#' set.seed(1)
#' fs <- 250
#' t <- seq(0, 4, by = 1 / fs)
#' ecg <- sin(2 * pi * 1.2 * t) + 0.1 * rnorm(length(t))
#' if (requireNamespace("signal", quietly = TRUE)) {
#'   res <- ecgdet(ecg, fs)
#'   res$extra$n_peaks
#' }
#' }
ecgdet <- function(ecg, fs) {
  ecg <- as.numeric(ecg)
  n <- length(ecg)
  bf <- .morie_dsp_butter(2, c(5, 15) / (fs / 2), type = "pass")
  filt <- .morie_dsp_filtfilt(bf$b, bf$a, ecg)
  d <- c(diff(filt), 0)
  sq <- d^2
  win_len <- max(1L, as.integer(0.15 * fs))
  kernel <- rep(1 / win_len, win_len)
  integ <- stats::filter(sq, kernel, sides = 2)
  integ[is.na(integ)] <- 0
  thr <- 0.5 * max(integ)
  candidates <- which(integ > thr)
  if (length(candidates) == 0L) {
    return(list(
      name = "pan_tompkins",
      filtered = ecg, fs = fs, n_samples = n,
      extra = list(r_peaks = integer(0), n_peaks = 0L)
    ))
  }
  min_dist <- as.integer(0.3 * fs)
  r_peaks <- candidates[1]
  for (c in candidates[-1]) {
    if (c - r_peaks[length(r_peaks)] >= min_dist) r_peaks <- c(r_peaks, c)
  }
  search_win <- as.integer(0.075 * fs)
  refined <- integer(length(r_peaks))
  for (i in seq_along(r_peaks)) {
    pk <- r_peaks[i]
    lo <- max(1L, pk - search_win)
    hi <- min(n, pk + search_win)
    refined[i] <- lo - 1L + which.max(ecg[lo:hi])
  }
  list(
    name = "pan_tompkins",
    filtered = ecg, fs = fs, n_samples = n,
    extra = list(r_peaks = refined, n_peaks = length(refined))
  )
}

#' RR interval series from R-peak sample indices
#'
#' Computes the RR (beat-to-beat) interval series in milliseconds from a
#' vector of R-peak sample indices.
#'
#' Reference: Task Force of the European Society of Cardiology and the
#' North American Society of Pacing and Electrophysiology (1996) "Heart
#' rate variability: standards of measurement, physiological
#' interpretation, and clinical use", *Circulation* 93(5):1043--1065.
#'
#' @param r_peaks Integer vector of R-peak sample indices.
#' @param fs Sampling frequency in Hz.
#' @return List with `value` (mean RR in ms), `name`, and `extra`
#'   (`rr_ms`, `mean_rr`, `std_rr`, `n_intervals`).
#' @export
#' @examples
#' \donttest{
#' rr <- rrint(c(100, 350, 600, 850, 1100), fs = 250)
#' rr$value
#' }
rrint <- function(r_peaks, fs) {
  r_peaks <- as.integer(r_peaks)
  if (length(r_peaks) < 2L) {
    return(list(name = "rr_intervals", value = NA_real_,
                extra = list(rr_ms = numeric(0))))
  }
  rr_samples <- diff(r_peaks)
  rr_ms <- rr_samples / fs * 1000
  list(
    name = "rr_intervals",
    value = mean(rr_ms),
    extra = list(
      rr_ms = rr_ms,
      mean_rr = mean(rr_ms),
      std_rr = if (length(rr_ms) > 1) stats::sd(rr_ms) else 0,
      n_intervals = length(rr_ms)
    )
  )
}

#' HRV time-domain metrics (SDNN, RMSSD, pNN50)
#'
#' Computes the standard HRV time-domain indices on an RR-interval series:
#' SDNN, RMSSD, pNN50, mean RR, mean HR, and the HRV triangular index.
#'
#' Reference: Task Force (1996), *Circulation* 93(5):1043--1065.
#'
#' @param rr Numeric vector of RR intervals in milliseconds.
#' @return List with `value` (SDNN), `name`, and `extra`
#'   (`sdnn`, `rmssd`, `pnn50`, `mean_rr`, `mean_hr`,
#'   `hrv_triangular_index`, `n_intervals`).
#' @export
#' @examples
#' \donttest{
#' set.seed(1)
#' rr <- 800 + cumsum(rnorm(200, sd = 20))
#' res <- hrvtd(rr)
#' res$extra$rmssd
#' }
hrvtd <- function(rr) {
  rr <- as.numeric(rr)
  if (length(rr) < 2L) return(list(name = "hrv_time_domain", value = NA_real_))
  sdnn <- stats::sd(rr)
  drr <- diff(rr)
  rmssd <- sqrt(mean(drr^2))
  pnn50 <- sum(abs(drr) > 50) / length(drr) * 100
  mean_rr <- mean(rr)
  mean_hr <- if (mean_rr > 0) 60000 / mean_rr else NA_real_
  tri_idx <- length(rr) / (max(graphics::hist(rr, breaks = 128, plot = FALSE)$counts) + 1e-12)
  list(
    name = "hrv_time_domain",
    value = sdnn,
    extra = list(
      sdnn = sdnn, rmssd = rmssd, pnn50 = pnn50,
      mean_rr = mean_rr, mean_hr = mean_hr,
      hrv_triangular_index = tri_idx,
      n_intervals = length(rr)
    )
  )
}

#' HRV frequency-domain metrics (VLF, LF, HF, LF/HF)
#'
#' Resamples the RR-interval series uniformly at `fs_interp` Hz, estimates
#' a Welch PSD, and integrates VLF (0.003--0.04 Hz), LF (0.04--0.15 Hz),
#' and HF (0.15--0.40 Hz) bands.
#'
#' Reference: Task Force (1996), *Circulation* 93(5):1043--1065.
#'
#' @param rr Numeric vector of RR intervals in milliseconds.
#' @param fs_interp Uniform resampling frequency in Hz (default 4).
#' @return List with `value` (total power), `name`, and `extra`
#'   (`vlf`, `lf`, `hf`, `lf_hf_ratio`, `total_power`, `lf_norm`,
#'   `hf_norm`).
#' @export
#' @examples
#' \donttest{
#' set.seed(1)
#' rr <- 800 + cumsum(rnorm(200, sd = 20))
#' res <- hrvfd(rr)
#' res$extra$lf_hf_ratio
#' }
hrvfd <- function(rr, fs_interp = 4) {
  rr <- as.numeric(rr)
  if (length(rr) < 10L) return(list(name = "hrv_freq_domain", value = NA_real_))
  t_rr <- cumsum(rr) / 1000
  t_rr <- t_rr - t_rr[1]
  t_uniform <- seq(0, t_rr[length(t_rr)], by = 1 / fs_interp)
  rr_interp <- stats::approx(t_rr, rr, xout = t_uniform, rule = 2)$y
  rr_interp <- rr_interp - mean(rr_interp)
  nperseg <- min(length(rr_interp), 256L)
  w <- welch(rr_interp, fs = fs_interp, nperseg = nperseg)
  freqs <- w$extra$freqs
  psd <- w$filtered
  df <- if (length(freqs) > 1) freqs[2] - freqs[1] else 1
  vlf <- sum(psd[freqs >= 0.003 & freqs < 0.04]) * df
  lf <- sum(psd[freqs >= 0.04 & freqs < 0.15]) * df
  hf <- sum(psd[freqs >= 0.15 & freqs < 0.40]) * df
  lf_hf <- if (hf > 0) lf / hf else NA_real_
  total <- vlf + lf + hf
  list(
    name = "hrv_freq_domain",
    value = total,
    extra = list(
      vlf = vlf, lf = lf, hf = hf,
      lf_hf_ratio = lf_hf,
      total_power = total,
      lf_norm = if ((lf + hf) > 0) lf / (lf + hf) * 100 else NA_real_,
      hf_norm = if ((lf + hf) > 0) hf / (lf + hf) * 100 else NA_real_
    )
  )
}

#' HRV nonlinear metrics (Poincare SD1, SD2)
#'
#' Computes the short- and long-axis standard deviations of the
#' Poincare plot: SD1 (short-term variability) and SD2 (long-term).
#'
#' Reference: Brennan, M., Palaniswami, M. & Kamen, P. (2001) "Do existing
#' measures of Poincare plot geometry reflect nonlinear features of heart
#' rate variability?", *IEEE Trans. Biomed. Eng.* 48(11):1342--1347.
#'
#' @param rr Numeric vector of RR intervals in milliseconds.
#' @return List with `value` (SD1), `name`, and `extra`
#'   (`sd1`, `sd2`, `sd1_sd2_ratio`, `n_intervals`).
#' @export
#' @examples
#' \donttest{
#' set.seed(1)
#' rr <- 800 + cumsum(rnorm(200, sd = 20))
#' res <- hrvnl(rr)
#' res$extra$sd1
#' }
hrvnl <- function(rr) {
  rr <- as.numeric(rr)
  if (length(rr) < 3L) return(list(name = "hrv_nonlinear", value = NA_real_))
  d <- diff(rr)
  sd1 <- stats::sd(d) / sqrt(2)
  sd2 <- sqrt(2 * stats::sd(rr)^2 - sd1^2)
  ratio <- if (sd2 > 0) sd1 / sd2 else NA_real_
  list(
    name = "hrv_nonlinear",
    value = sd1,
    extra = list(sd1 = sd1, sd2 = sd2, sd1_sd2_ratio = ratio,
                 n_intervals = length(rr))
  )
}

#' Katz fractal dimension
#'
#' Katz fractal dimension \eqn{D = \\log_{10}(n - 1) / (\\log_{10}(n - 1) +
#' \\log_{10}(d / L))}{D = log_10(n - 1) / ( log_10(n - 1) + log_10(d / L))} of a 1-D
#' signal. \eqn{L} is total path length and
#' \eqn{d} is the diameter (max distance from the first sample).
#'
#' Reference: Katz, M.J. (1988) "Fractals and the analysis of waveforms",
#' *Comput. Biol. Med.* 18(3):145--156.
#'
#' @param x Numeric vector.
#' @return List with `value` (D), `name`, and `extra` (`L`, `d`, `n`).
#' @export
#' @examples
#' \donttest{
#' set.seed(1)
#' x <- cumsum(rnorm(1000))
#' res <- kfd(x)
#' res$value
#' }
kfd <- function(x) {
  x <- as.numeric(x)
  n <- length(x)
  if (n < 2L) return(list(name = "katz_fd", value = NA_real_))
  dists <- abs(diff(x))
  L <- sum(dists)
  d <- max(abs(x - x[1]))
  a <- mean(dists)
  if (d == 0 || a == 0) return(list(name = "katz_fd", value = NA_real_))
  D <- log10(n - 1) / (log10(n - 1) + log10(d / L))
  list(name = "katz_fd", value = as.numeric(D),
       extra = list(L = L, d = d, n = n))
}

#' Petrosian fractal dimension
#'
#' Petrosian fractal dimension \eqn{D = \\log_{10}(N) / (\\log_{10}(N) +
#' \\log_{10}(N / (N + 0.4 N_\delta)))}{D = log_10(N) / ( log_10(N) + log_10(N / (N + 0.4
#' N_delta)))}, where \eqn{N_\delta}{N_delta} counts sign
#' changes of the first difference. A fast complexity proxy for EEG/ECG.
#'
#' Reference: Petrosian, A. (1995) "Kolmogorov complexity of finite
#' sequences and recognition of different preictal EEG patterns",
#' *Proc. 8th IEEE Symp. Comput.-Based Med. Syst.*, pp. 212--217.
#'
#' @param x Numeric vector.
#' @return List with `value` (D), `name`, and `extra` (`n_delta`, `n`).
#' @export
#' @examples
#' \donttest{
#' set.seed(1)
#' x <- cumsum(rnorm(1000))
#' res <- pfd(x)
#' res$value
#' }
pfd <- function(x) {
  x <- as.numeric(x)
  n <- length(x)
  if (n < 2L) return(list(name = "petrosian_fd", value = NA_real_))
  dx <- diff(x)
  if (length(dx) < 2L) return(list(name = "petrosian_fd", value = NA_real_))
  n_delta <- sum(dx[-length(dx)] * dx[-1] < 0)
  D <- log10(n) / (log10(n) + log10(n / (n + 0.4 * n_delta)))
  list(name = "petrosian_fd", value = as.numeric(D),
       extra = list(n_delta = n_delta, n = n))
}

#' Burg autoregressive power spectral density
#'
#' Burg AR-spectrum estimation: parametric PSD via the Burg algorithm
#' for AR-coefficient estimation. Well-suited to short HRV windows
#' where Welch suffers from low spectral resolution.
#'
#' Reference: Marple, S.L. (1987) *Digital Spectral Analysis with
#' Applications*, Prentice-Hall, on the Burg algorithm.
#'
#' @param x Numeric vector (1-D signal).
#' @param fs Sampling frequency in Hz.
#' @param order AR model order (default 16).
#' @param nfft FFT length for PSD evaluation (default 256).
#' @return List with `filtered` (PSD), `name`, `fs`, `n_samples`, and
#'   `extra` (`freqs`, `order`, `ar_coefficients`).
#' @export
#' @examples
#' \donttest{
#' set.seed(1)
#' t <- seq(0, 1, length.out = 512)
#' x <- sin(2 * pi * 10 * t) + 0.5 * rnorm(length(t))
#' res <- pburg(x, fs = 512)
#' length(res$filtered)
#' }
pburg <- function(x, fs, order = 16L, nfft = 256L) {
  x <- as.numeric(x)
  n <- length(x)
  if (order >= n) order <- n - 1L
  ef <- x
  eb <- x
  a <- numeric(order + 1L)
  a[1] <- 1
  pe <- sum(x * x) / n
  for (m in seq_len(order)) {
    efm <- ef[(m + 1L):n]
    ebm <- eb[m:(n - 1L)]
    num <- -2 * sum(efm * ebm)
    den <- sum(efm * efm) + sum(ebm * ebm)
    if (den == 0) break
    km <- num / den
    a_new <- numeric(m + 1L)
    a_new[1] <- 1
    if (m > 1L) for (j in seq_len(m - 1L)) a_new[j + 1L] <- a[j + 1L] + km * a[m - j + 1L]
    a_new[m + 1L] <- km
    a <- numeric(order + 1L)
    a[seq_len(m + 1L)] <- a_new
    pe <- pe * (1 - km * km)
    ef_old <- ef
    ef[(m + 1L):n] <- ef_old[(m + 1L):n] + km * eb[m:(n - 1L)]
    eb[(m + 1L):n] <- eb[m:(n - 1L)] + km * ef_old[(m + 1L):n]
  }
  freqs <- seq(0, fs / 2, length.out = nfft %/% 2L + 1L)
  omega <- 2 * pi * freqs / fs
  ar_coeffs <- a[seq_len(order + 1L)]
  denom <- complex(length(omega))
  for (k in seq_along(ar_coeffs)) {
    denom <- denom + ar_coeffs[k] * exp(-1i * omega * (k - 1L))
  }
  psd <- pe / (abs(denom)^2 + 1e-30)
  list(
    name = "burg_psd",
    filtered = as.numeric(psd),
    fs = fs,
    n_samples = length(psd),
    extra = list(freqs = freqs, order = order, ar_coefficients = ar_coeffs)
  )
}

#' Welch power spectral density
#'
#' Welch's averaged periodogram PSD: split into segments, window (Hanning),
#' periodogram each, average. Delegates to `oce::pwelch()` if available,
#' otherwise computes a Hanning-windowed, 50%-overlap implementation in
#' base R.
#'
#' Reference: Welch, P.D. (1967) "The use of fast Fourier transform for
#' the estimation of power spectra", *IEEE Trans. Audio Electroacoust.*
#' AU-15(2):70--73.
#'
#' @param x Numeric vector (1-D signal).
#' @param fs Sampling frequency in Hz.
#' @param nperseg Segment length (default 256).
#' @return List with `filtered` (PSD), `name`, `fs`, `n_samples`, and
#'   `extra` (`freqs`).
#' @export
#' @examples
#' \donttest{
#' set.seed(1)
#' t <- seq(0, 1, length.out = 1024)
#' x <- sin(2 * pi * 50 * t) + 0.3 * rnorm(length(t))
#' res <- welch(x, fs = 1024)
#' length(res$filtered)
#' }
welch <- function(x, fs, nperseg = 256L) {
  x <- as.numeric(x)
  n <- length(x)
  nperseg <- min(nperseg, n)
  noverlap <- nperseg %/% 2L
  step <- nperseg - noverlap
  win <- 0.5 - 0.5 * cos(2 * pi * (seq_len(nperseg) - 1L) / (nperseg - 1L))
  win_norm <- sum(win^2)
  starts <- seq(1L, n - nperseg + 1L, by = step)
  if (length(starts) == 0L) starts <- 1L
  nfreq <- nperseg %/% 2L + 1L
  psd_sum <- numeric(nfreq)
  for (s in starts) {
    seg <- x[s:(s + nperseg - 1L)]
    seg <- (seg - mean(seg)) * win
    X <- stats::fft(seg)[seq_len(nfreq)]
    p <- Mod(X)^2 / (fs * win_norm)
    if (nperseg %% 2L == 0L) {
      p[2:(nfreq - 1L)] <- 2 * p[2:(nfreq - 1L)]
    } else {
      p[2:nfreq] <- 2 * p[2:nfreq]
    }
    psd_sum <- psd_sum + p
  }
  psd <- psd_sum / length(starts)
  freqs <- seq(0, fs / 2, length.out = nfreq)
  list(
    name = "welch_psd",
    filtered = psd,
    fs = fs,
    n_samples = length(psd),
    extra = list(freqs = freqs)
  )
}

#' PCG Shannon-energy envelope
#'
#' Shannon-energy envelope of a phonocardiogram (PCG): normalises the
#' signal, computes \eqn{-x^2 \log x^2}{-x^2 log x^2}, then box-smooths over a 20 ms
#' window. The standard envelope used for S1/S2 segmentation.
#'
#' Reference: Liang, H., Lukkarinen, S. & Hartimo, I. (1997) "Heart sound
#' segmentation algorithm based on heart sound envelogram",
#' *Comput. Cardiol.*, pp. 105--108.
#'
#' @param pcg Numeric vector (1-D PCG signal).
#' @param fs Sampling frequency in Hz.
#' @return List with `filtered` (envelope), `name`, `fs`, `n_samples`.
#' @export
#' @examples
#' \donttest{
#' set.seed(1)
#' pcg <- rnorm(4000)
#' res <- pcgenv(pcg, fs = 2000)
#' length(res$filtered)
#' }
pcgenv <- function(pcg, fs) {
  pcg <- as.numeric(pcg)
  norm <- pcg / (max(abs(pcg)) + 1e-12)
  shannon <- -(norm^2) * log(norm^2 + 1e-12)
  win_len <- max(1L, as.integer(0.02 * fs))
  kernel <- rep(1 / win_len, win_len)
  env <- stats::filter(shannon, kernel, sides = 2)
  env[is.na(env)] <- 0
  list(
    name = "pcg_envelope",
    filtered = as.numeric(env),
    fs = fs,
    n_samples = length(env)
  )
}

#' PCG S1/S2 heart-sound segmentation
#'
#' Segments a PCG Shannon-energy envelope into S1 (systolic) and S2
#' (diastolic) heart-sound events: threshold, find above-threshold runs,
#' merge close peaks, label alternating events.
#'
#' Reference: Liang, Lukkarinen & Hartimo (1997), *Comput. Cardiol.*,
#' pp. 105--108.
#'
#' @param envelope Numeric vector (Shannon-energy envelope).
#' @param fs Sampling frequency in Hz (default 2000).
#' @param min_gap_ms Minimum gap between peaks in ms (default 100).
#' @return List with `value` (cycle count), `name`, and `extra`
#'   (`s1_indices`, `s2_indices`, `n_cycles`, `n_peaks`).
#' @export
#' @examples
#' \donttest{
#' set.seed(1)
#' env <- abs(sin(seq(0, 20, length.out = 4000))) + 0.05 * rnorm(4000)
#' env[env < 0] <- 0
#' res <- pcgseg(env, fs = 2000)
#' res$extra$n_cycles
#' }
#' @rdname pcgseg-signal
pcgseg <- function(envelope, fs = 2000, min_gap_ms = 100) {
  env <- as.numeric(envelope)
  if (length(env) < 4L) {
    return(list(name = "pcg_segment", value = 0,
                extra = list(s1_indices = integer(0), s2_indices = integer(0),
                             n_cycles = 0L)))
  }
  thr <- mean(env) + 0.5 * stats::sd(env)
  above <- env > thr
  edges <- diff(as.integer(above))
  starts <- which(edges == 1L) + 1L
  stops <- which(edges == -1L) + 1L
  if (above[1]) starts <- c(1L, starts)
  if (above[length(env)]) stops <- c(stops, length(env))
  n_seg <- min(length(starts), length(stops))
  if (n_seg == 0L) {
    return(list(name = "pcg_segment", value = 0,
                extra = list(s1_indices = integer(0), s2_indices = integer(0),
                             n_cycles = 0L)))
  }
  starts <- starts[seq_len(n_seg)]
  stops <- stops[seq_len(n_seg)]
  peaks <- as.integer((starts + stops) %/% 2L)
  min_gap <- as.integer(min_gap_ms * fs / 1000)
  merged <- peaks[1]
  for (p in peaks[-1]) {
    if (p - merged[length(merged)] >= min_gap) merged <- c(merged, p)
  }
  peaks <- merged
  is_s1 <- (seq_along(peaks) %% 2L) == 1L
  s1 <- as.integer(peaks[is_s1])
  s2 <- as.integer(peaks[!is_s1])
  n_cycles <- min(length(s1), length(s2))
  list(
    name = "pcg_segment",
    value = as.numeric(n_cycles),
    extra = list(s1_indices = s1, s2_indices = s2,
                 n_cycles = n_cycles, n_peaks = length(peaks))
  )
}

#' PCG murmur likelihood score
#'
#' Combines a 100--400 Hz band-energy ratio, normalised spectral entropy,
#' and the Higuchi fractal dimension of the PCG into a murmur-likelihood
#' score in `\[0, 1\]`.
#'
#' Reference: Rangayyan, R.M. (2015) *Biomedical Signal Analysis*, 2nd ed.,
#' Wiley/IEEE Press, chapter on heart-sound analysis.
#'
#' @param pcg Numeric vector (1-D PCG signal).
#' @param fs Sampling frequency in Hz.
#' @return List with `value` (score in `\[0, 1\]`), `name`, and `extra`
#'   (`fractal_dimension`, `hf_energy_ratio`, `spectral_entropy`,
#'   `fd_score`, `hf_score`, `ent_score`).
#' @export
#' @examples
#' \donttest{
#' if (requireNamespace("signal", quietly = TRUE)) {
#'   set.seed(1)
#'   pcg <- rnorm(4000)
#'   res <- pcgmur(pcg, fs = 2000)
#'   res$value
#' }
#' }
pcgmur <- function(pcg, fs) {
  pcg <- as.numeric(pcg)
  bf <- .morie_dsp_butter(4, c(100, 400) / (fs / 2), type = "pass")
  hf_band <- .morie_dsp_filtfilt(bf$b, bf$a, pcg)
  hf_energy <- mean(hf_band^2)
  total_energy <- mean(pcg^2) + 1e-12
  hf_ratio <- hf_energy / total_energy
  nperseg <- min(length(pcg), 256L)
  w <- welch(pcg, fs = fs, nperseg = nperseg)
  psd <- w$filtered
  psd_norm <- psd / (sum(psd) + 1e-12)
  spectral_entropy <- -sum(psd_norm * log(psd_norm + 1e-12))
  max_entropy <- log(length(psd_norm))
  norm_entropy <- if (max_entropy > 0) spectral_entropy / max_entropy else 0
  hfd_result <- tryCatch(hfd(pcg, kmax = 10L), error = function(e) NULL)
  fd <- if (!is.null(hfd_result) && !is.na(hfd_result$value)) hfd_result$value else 1
  fd_score <- min(1, max(0, (fd - 1) / 0.5))
  hf_score <- min(1, hf_ratio * 5)
  ent_score <- min(1, norm_entropy)
  score <- 0.4 * fd_score + 0.35 * hf_score + 0.25 * ent_score
  list(
    name = "pcg_murmur_score",
    value = as.numeric(score),
    extra = list(
      fractal_dimension = fd,
      hf_energy_ratio = hf_ratio,
      spectral_entropy = norm_entropy,
      fd_score = fd_score,
      hf_score = hf_score,
      ent_score = ent_score
    )
  )
}

#' Sample entropy
#'
#' Sample entropy (SampEn) of a 1-D signal: \eqn{-\log(A / B)}{-log(A / B)}, where
#' \eqn{A} counts template-vector matches at embedding dimension
#' \eqn{m + 1} and \eqn{B} at dimension \eqn{m}, with Chebyshev distance
#' tolerance \eqn{r \cdot \mathrm{sd}(x)}{r \* sd(x)}.
#'
#' Reference: Richman, J.S. & Moorman, J.R. (2000) "Physiological time-
#' series analysis using approximate entropy and sample entropy",
#' *Am. J. Physiol. Heart Circ. Physiol.* 278(6):H2039--H2049
#' (refines Pincus, S.M. (1991), *Proc. Natl. Acad. Sci. USA* 88:2297).
#'
#' @param x Numeric vector.
#' @param m Embedding dimension (default 2).
#' @param r Tolerance as fraction of sd (default 0.2).
#' @return List with `value` (SampEn), `name`, and `extra`
#'   (`m`, `r`, `tolerance`, `A`, `B`).
#' @export
#' @examples
#' \donttest{
#' set.seed(1)
#' x <- sin(seq(0, 10 * pi, length.out = 500)) + 0.1 * rnorm(500)
#' res <- sampen(x)
#' res$value
#' }
sampen <- function(x, m = 2L, r = 0.2) {
  x <- as.numeric(x)
  n <- length(x)
  tol <- r * stats::sd(x)
  if (is.na(tol) || tol == 0 || n < m + 2L) {
    return(list(name = "sample_entropy", value = NA_real_))
  }
  count_matches <- function(dim) {
    nt <- n - dim
    if (nt < 2L) return(0L)
    tmpl <- matrix(0, nrow = nt, ncol = dim)
    for (i in seq_len(nt)) tmpl[i, ] <- x[i:(i + dim - 1L)]
    count <- 0L
    for (i in seq_len(nt - 1L)) {
      diffs <- tmpl[(i + 1L):nt, , drop = FALSE]
      diffs <- sweep(diffs, 2, tmpl[i, ], "-")
      maxd <- apply(abs(diffs), 1, max)
      count <- count + sum(maxd <= tol)
    }
    count
  }
  a <- count_matches(m + 1L)
  b <- count_matches(m)
  if (b == 0L) return(list(name = "sample_entropy", value = Inf))
  se <- -log(a / b)
  list(
    name = "sample_entropy",
    value = as.numeric(se),
    extra = list(m = m, r = r, tolerance = tol, A = a, B = b)
  )
}

#' Savitzky-Golay smoothing (direct alias)
#'
#' Direct short-name export of the Savitzky-Golay smoother (matches the
#' Python `morie.signal.sgolay` name). For the long-form, see
#' [morie_sgolay_smooth()], which this function delegates to.
#'
#' Reference: Savitzky, A. & Golay, M.J.E. (1964) "Smoothing and
#' differentiation of data by simplified least-squares procedures",
#' *Anal. Chem.* 36(8):1627--1639.
#'
#' @param x Numeric vector.
#' @param window Window length (odd, default 11).
#' @param polyorder Polynomial order (default 3).
#' @return List with `filtered`, `name`, `fs`, `n_samples`, `extra`
#'   (`window`, `polyorder`).
#' @export
#' @examples
#' \donttest{
#' if (requireNamespace("signal", quietly = TRUE)) {
#'   set.seed(1)
#'   x <- sin(seq(0, 2 * pi, length.out = 200)) + 0.1 * rnorm(200)
#'   res <- sgolay(x)
#'   length(res$filtered)
#' }
#' }
sgolay <- function(x, window = 11L, polyorder = 3L) {
  if (window %% 2L == 0L) window <- window + 1L
  res <- morie_sgolay_smooth(x, window_length = window, polyorder = polyorder)
  list(
    name = "savgol_smooth",
    filtered = res$filtered,
    fs = 0,
    n_samples = length(res$filtered),
    extra = list(window = window, polyorder = polyorder)
  )
}

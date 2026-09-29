.sgm_rate <- function(v) {
  s <- ifelse(v >= 0, 1, -1)
  sum(s[-1] != s[-length(s)]) / (length(s) - 1)
}

.sgm_window <- function(name, n) {
  k <- 0:(n - 1)
  switch(name,
    hann = , hanning = 0.5 - 0.5 * cos(2 * pi * k / n),
    hamming = 0.54 - 0.46 * cos(2 * pi * k / n),
    boxcar = , rectangular = rep(1, n),
    stop("window must be 'hann', 'hamming' or 'boxcar'")
  )
}

#' Signal measures: group delay, noise level, LMS bound, QRS duration, ZCR, spectrogram
#'
#' `group_delay` is the exact group delay of `B(z)/A(z)` (the algorithm of
#' `scipy.signal.group_delay`) on `w = pi k / worN`; `noise_power` the
#' reference-based noise power or the successive-difference estimator
#' `sum diff(x)^2 / (2(N - 1))`; `noise_psd` the white-noise PSD level
#' `sigma^2 / fs` (two-sided) or twice it; `max_step_size` the LMS bound
#' `2 / (M P_x)`; `qrs_duration` the mean and SD of `(off - on)/fs`;
#' `zero_crossing_rate` the share of sign changes (zero counted positive);
#' `spcgm` the one-sided PSD spectrogram with scipy's conventions.
#'
#' @param b,a Numerator and denominator coefficients.
#' @param worN Number of frequencies.
#' @param x Signal.
#' @param signal Clean reference (NULL for the difference estimator).
#' @param fs Sampling frequency.
#' @param onesided One-sided PSD level.
#' @param order Number of LMS taps.
#' @param qrs_on,qrs_off Onset and offset sample indices.
#' @param frame_length Frame length (NULL for the whole signal).
#' @param nperseg Segment length.
#' @param noverlap Overlap (default `nperseg %/% 8`).
#' @param window "hann", "hamming" or "boxcar".
#' @param nfft DFT length (default `nperseg`).
#' @return Lists with `value` and the components of the Python arm.
#' @references Oppenheim, A. V. and Schafer, R. W. (2010). Discrete-Time
#'   Signal Processing, 3rd ed. Pearson. Rice, J. (1984). Bandwidth choice
#'   for nonparametric regression. Annals of Statistics 12, 1215-1230.
#'   Haykin, S. (2014). Adaptive Filter Theory, 5th ed. Pearson. Rabiner, L.
#'   R. and Schafer, R. W. (1978). Digital Processing of Speech Signals.
#'   Prentice-Hall. Rangayyan, R. M. (2015). Biomedical Signal Analysis, 2nd
#'   ed. Wiley-IEEE Press. Allen, J. B. and Rabiner, L. R. (1977). A unified
#'   approach to short-time Fourier analysis and synthesis. Proceedings of
#'   the IEEE 65, 1558-1564.
#' @examples
#' group_delay(c(1, 2, 1), 1, worN = 4)$value
#' zero_crossing_rate(c(1, -1, -2, 3, 0))$value
#' @export
group_delay <- function(b, a, worN = 512) {
  cc <- stats::convolve(as.numeric(b), as.numeric(a), type = "open")
  ws <- pi * (seq_len(worN) - 1) / worN
  k <- seq_along(cc) - 1
  gd <- vapply(ws, function(w) {
    e <- exp(-1i * w * k)
    den <- sum(cc * e)
    if (Mod(den) < 10 * .Machine$double.eps) 0 else Re(sum(k * cc * e) / den) - (length(a) - 1)
  }, 0)
  list(name = "group_delay", value = gd, frequencies = ws, delay = gd)
}

#' @rdname group_delay
#' @export
noise_power <- function(x, signal = NULL) {
  x <- as.numeric(x)
  n <- length(x)
  if (!is.null(signal)) {
    if (length(signal) != n) stop("x and signal must have equal length")
    pn <- mean((x - as.numeric(signal))^2)
    method <- "reference"
  } else {
    if (n < 2) stop("the difference estimator needs at least two samples")
    pn <- sum(diff(x)^2) / (2 * (n - 1))
    method <- "successive differences"
  }
  list(name = "noise_power", value = pn, noise_power = pn, n = n, method = method)
}

#' @rdname group_delay
#' @export
noise_psd <- function(x, fs = 1, onesided = FALSE) {
  x <- as.numeric(x)
  v <- mean((x - mean(x))^2)
  psd <- (if (onesided) 2 else 1) * v / fs
  list(name = "noise_psd", value = psd, psd = psd, variance = v, fs = fs, n = length(x), onesided = onesided)
}

#' @rdname group_delay
#' @export
max_step_size <- function(x, order = 16) {
  px <- mean(as.numeric(x)^2)
  if (px <= 0) stop("Signal power is zero; step size is undefined.")
  if (order <= 0) stop("Filter order must be positive.")
  list(name = "max_step_size", value = 2 / (order * px), mu_max = 2 / (order * px), order = order, Px = px)
}

#' @rdname group_delay
#' @export
qrs_duration <- function(qrs_on, qrs_off, fs = 1) {
  n <- min(length(qrs_on), length(qrs_off))
  if (n == 0) return(list(name = "qrs_duration", value = 0, qrs_durations = numeric(0), n_beats = 0))
  d <- (qrs_off[seq_len(n)] - qrs_on[seq_len(n)]) / fs
  list(name = "qrs_duration", value = mean(d), qrs_durations = d, mean_dur = mean(d),
       std_dur = if (n > 1) stats::sd(d) else 0, n_beats = n, fs = fs)
}

#' @rdname group_delay
#' @export
zero_crossing_rate <- function(x, frame_length = NULL) {
  x <- as.numeric(x)
  if (is.null(frame_length)) {
    if (length(x) < 2) stop("x needs at least two samples")
    return(list(name = "zero_crossing_rate", value = .sgm_rate(x)))
  }
  if (frame_length < 2) stop("frame_length must be at least 2")
  per <- vapply(seq_len(length(x) %/% frame_length) - 1, function(i) .sgm_rate(x[i * frame_length + seq_len(frame_length)]), 0)
  list(name = "zero_crossing_rate", value = mean(per), per_frame = per)
}

#' @rdname group_delay
#' @export
spcgm <- function(x, fs = 1, nperseg = 256, noverlap = NULL, window = "hann", nfft = NULL) {
  x <- as.numeric(x)
  n <- length(x)
  seg <- min(nperseg, n)
  ov <- if (is.null(noverlap)) seg %/% 8 else noverlap
  L <- if (is.null(nfft)) seg else nfft
  if (L < seg) stop("nfft must be at least nperseg")
  w <- .sgm_window(window, seg)
  scale <- 1 / (fs * sum(w^2))
  nf <- L %/% 2 + 1
  starts <- seq(0, n - seg, by = seg - ov)
  cols <- vapply(starts, function(s0) {
    s <- x[s0 + seq_len(seg)]
    s <- (s - mean(s)) * w
    p <- Mod(stats::fft(c(s, rep(0, L - seg))))[seq_len(nf)]^2 * scale
    idx <- 2:(nf - (if (L %% 2 == 0) 1 else 0))
    if (nf >= 2 && length(idx) && idx[1] <= idx[length(idx)]) p[idx] <- 2 * p[idx]
    p
  }, numeric(nf))
  Sxx <- matrix(cols, nf)
  list(name = "spcgm", value = Sxx, frequencies = (seq_len(nf) - 1) * fs / L, times = (starts + seg / 2) / fs, Sxx = Sxx)
}

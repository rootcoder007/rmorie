# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Rangayyan template-B mirrors -- the surface NOT already covered by
# R/dsp_native.R and R/dsp_spectral.R.
#
# The collision scan found eighteen morie_dsp_* functions, of which
# five already cover this shelf: morie_dsp_psd_periodogram,
# morie_dsp_psd_welch, morie_dsp_acf_from_psd, morie_dsp_coherence and
# morie_dsp_band_power. Those are deliberately NOT duplicated here.
#
# This file adds the autocorrelation estimator with both divisors,
# Yule-Walker and Burg AR estimation, the AR spectrum, boxcar and
# general FIR filters, the LMS error and gradient, the RLS normal
# equations, cepstral pitch, spectral bandwidth, the Pan-Tompkins
# threshold update, ensemble averaging and transfer-function
# estimation.
#
# Mirrors the morie.fn rg*/rng* modules. Spec: Rangayyan, R. M.
# (2015), Biomedical Signal Analysis, 2nd ed., Wiley-IEEE Press.

#' Autocorrelation estimate with both divisors
#'
#' \eqn{R_{xx}(m) = \frac{1}{N-|m|}\sum x(n)x(n+m)} (unbiased) and the
#' divisor-N biased form. The unbiased estimate is NOT guaranteed
#' positive semi-definite, so feeding it to an AR solve can yield an
#' unstable model -- which is why \code{\link{morie_yule_walker}} uses
#' the biased one. Mirrors \code{morie.fn.rgacf}.
#'
#' @param x numeric signal.
#' @param max_lag maximum lag; N - 1 by default.
#' @return list: lags, acf_unbiased, acf_biased, N.
#' @references Rangayyan (2015), Ch. 3.
#' @examples
#' morie_acf_estimate(rnorm(50), max_lag = 5)$acf_unbiased
#' @export
morie_acf_estimate <- function(x, max_lag = NULL) {
  x <- as.numeric(x)
  N <- length(x)
  if (N < 2L) stop("need at least 2 samples.", call. = FALSE)
  L <- if (is.null(max_lag)) N - 1L else as.integer(max_lag)
  if (L < 0L || L > N - 1L) {
    stop(sprintf("max_lag must lie in 0..%d.", N - 1L), call. = FALSE)
  }
  lags <- 0:L
  raw <- vapply(lags, function(m) sum(x[seq_len(N - m)] * x[(m + 1):N]), 0)
  list(lags = lags, acf_unbiased = raw / (N - lags), acf_biased = raw / N,
       N = N,
       method = "R_xx(m); unbiased divisor N-|m| is not PSD-guaranteed")
}

#' Yule-Walker AR estimation
#'
#' Solves the Toeplitz system \eqn{R a = -r} on the BIASED
#' autocorrelation, which is positive semi-definite and therefore
#' guarantees a stable fitted model. Mirrors
#' \code{morie.fn.rgyw}.
#'
#' @param x numeric signal.
#' @param order AR order.
#' @return list: a, sigma2, order, stable.
#' @references Rangayyan (2015), Ch. 3.
#' @examples
#' morie_yule_walker(rnorm(200), order = 2)$stable
#' @export
morie_yule_walker <- function(x, order = 4L) {
  p <- as.integer(order)
  if (p < 1L) stop("order must be at least 1.", call. = FALSE)
  x <- as.numeric(x)
  if (length(x) < p + 1L) {
    stop("need more samples than the order.", call. = FALSE)
  }
  R <- morie_acf_estimate(x, max_lag = p)$acf_biased
  Rm <- outer(seq_len(p), seq_len(p), function(i, j) R[abs(i - j) + 1L])
  r <- R[2:(p + 1L)]
  a <- tryCatch(solve(Rm, -r), error = function(e) qr.solve(Rm, -r))
  roots <- polyroot(rev(c(1, a)))
  list(a = a, sigma2 = as.numeric(R[1] + sum(a * r)), order = p,
       stable = all(Mod(roots) < 1),
       method = "Toeplitz Yule-Walker on the BIASED ACF (guarantees stability)")
}

#' Burg lattice AR estimation
#'
#' Chooses each reflection coefficient to minimise the combined
#' forward and backward prediction error power. No autocorrelation
#' estimate and no windowing, and every \eqn{|k_m| \le 1} makes
#' stability automatic. Mirrors \code{morie.fn.rgburg}.
#'
#' @param x numeric signal.
#' @param order AR order.
#' @return list: a, reflection, sigma2, order, stable.
#' @references Rangayyan (2015), Ch. 3.
#' @examples
#' morie_burg_method(rnorm(200), order = 2)$stable
#' @export
morie_burg_method <- function(x, order = 8L) {
  x <- as.numeric(x)
  p <- as.integer(order)
  N <- length(x)
  if (p < 1L) stop("order must be at least 1.", call. = FALSE)
  if (N < p + 1L) stop("need more samples than the order.", call. = FALSE)
  f <- x
  b <- x
  a <- numeric(0)
  E <- mean(x^2)
  ks <- numeric(p)
  for (m in seq_len(p)) {
    fm <- f[(m + 1L):N]
    bm <- b[m:(N - 1L)]
    den <- sum(fm^2) + sum(bm^2)
    k <- if (den > 0) -2 * sum(fm * bm) / den else 0
    k <- max(min(k, 1), -1)
    ks[m] <- k
    a <- if (length(a)) c(a, 0) + k * c(rev(a), 1) else k
    fn <- fm + k * bm
    bn <- bm + k * fm
    f <- c(numeric(m), fn)
    b <- c(numeric(m), bn)
    E <- E * (1 - k^2)
  }
  roots <- polyroot(rev(c(1, a)))
  list(a = a, reflection = ks, sigma2 = E, order = p,
       stable = all(Mod(roots) < 1),
       method = "Burg lattice; |k| <= 1 guarantees a stable model")
}

#' Autoregressive power spectrum
#'
#' \eqn{S_{AR}(f) = \sigma^2/|1 + \sum a_k e^{-j2\pi fkT}|^2}. Smooth
#' and not resolution-limited by the record length, which is both its
#' appeal and its risk: too high an order invents peaks. Mirrors
#' \code{morie.fn.rgarsp}.
#'
#' @param x numeric signal.
#' @param order AR order.
#' @param fs sampling frequency.
#' @param n_freqs frequency grid size.
#' @return list: freqs, psd, a, sigma2, stable.
#' @references Rangayyan (2015), Ch. 3.
#' @examples
#' morie_ar_spectrum(rnorm(300), order = 4, fs = 100)$stable
#' @export
morie_ar_spectrum <- function(x, order = 8L, fs = 1, n_freqs = 512L) {
  fs <- as.numeric(fs)
  if (fs <= 0) stop("fs must be positive.", call. = FALSE)
  yw <- morie_yule_walker(x, order = order)
  a <- yw$a
  freqs <- seq(0, fs / 2, length.out = as.integer(n_freqs))
  k <- seq_along(a)
  den <- vapply(freqs, function(f) {
    Mod(1 + sum(a * exp(-2i * pi * f / fs * k)))^2
  }, 0)
  list(freqs = freqs, psd = yw$sigma2 / pmax(den, 1e-300), a = a,
       sigma2 = yw$sigma2, order = yw$order, stable = yw$stable,
       method = "All-pole AR spectrum; high order invents peaks")
}

#' Causal moving-average (boxcar) filter
#'
#' \eqn{y[n] = (1/M)\sum_{k=0}^{M-1} x[n-k]}. Sinc magnitude response
#' with zeros at multiples of fs/M, and group delay (M-1)/2 -- the
#' output is NOT aligned with the input. Mirrors
#' \code{morie.fn.rgmavg}.
#'
#' @param x numeric signal.
#' @param M window length.
#' @return list: y, group_delay, M, N.
#' @references Rangayyan (2015), Ch. 3.
#' @examples
#' morie_moving_average(1:20, M = 4)$group_delay
#' @export
morie_moving_average <- function(x, M = 8L) {
  x <- as.numeric(x)
  M <- as.integer(M)
  if (M < 1L) stop("M must be at least 1.", call. = FALSE)
  if (length(x) < M) stop("need at least M samples.", call. = FALSE)
  y <- stats::filter(x, rep(1 / M, M), method = "convolution", sides = 1L)
  y <- as.numeric(y)
  # Startup transient: divide by M, matching the zero-padded
  # convolution morie.fn.rgmavg uses. Dividing by the number of
  # AVAILABLE samples instead would give a different (larger) ramp and
  # break parity -- y[2] would be 0.5 rather than 0.25 on 0:9 with M=4.
  na_i <- which(is.na(y))
  y[na_i] <- vapply(na_i, function(i) sum(x[seq_len(i)]) / M, 0)
  list(y = y, group_delay = (M - 1) / 2, M = M, N = length(x),
       method = "boxcar lowpass; zeros at fs/M multiples, delay (M-1)/2")
}

#' General FIR filter
#'
#' \eqn{y(n) = \sum_k b_k x(n-k)}. Linear phase only when the taps are
#' symmetric, which is checked rather than assumed. Mirrors
#' \code{morie.fn.rng087}.
#'
#' @param x numeric signal.
#' @param b_k filter taps.
#' @return list: y, linear_phase, dc_gain, order.
#' @references Rangayyan (2015), Ch. 3.
#' @examples
#' morie_fir_filter(1:20, c(0.25, 0.5, 0.25))$linear_phase
#' @export
morie_fir_filter <- function(x, b_k) {
  x <- as.numeric(x)
  b <- as.numeric(b_k)
  if (!length(b)) stop("b_k must be non-empty.", call. = FALSE)
  if (length(x) < length(b)) stop("signal shorter than the filter.", call. = FALSE)
  y <- as.numeric(stats::filter(x, b, method = "convolution", sides = 1L))
  y[is.na(y)] <- 0
  list(y = y, linear_phase = isTRUE(all.equal(b, rev(b))),
       dc_gain = sum(b), order = length(b) - 1L,
       method = "y(n) = sum b_k x(n-k); linear phase iff taps symmetric")
}

#' LMS instantaneous error and gradient
#'
#' \eqn{e(n) = x(n) - w^T r(n)} and
#' \eqn{\widehat\nabla e^2(n) = -2 e(n) r(n)}. The gradient of the
#' INSTANTANEOUS squared error needs no expectation -- that is
#' Widrow-Hoff's simplification and why LMS converges only in the
#' mean. Mirrors \code{morie.fn.rng156} and \code{rng159}.
#'
#' @param x numeric primary input.
#' @param w tap weights.
#' @param r matrix of reference vectors (rows = time).
#' @return list: error, mse, gradient.
#' @references Rangayyan (2015), Ch. 3.
#' @examples
#' morie_lms_error(rnorm(20), c(0.5, -0.2), matrix(rnorm(40), ncol = 2))$mse
#' @export
morie_lms_error <- function(x, w, r) {
  x <- as.numeric(x)
  R <- as.matrix(r)
  if (nrow(R) != length(x)) R <- t(R)
  if (nrow(R) != length(x)) stop("r must have one row per sample.", call. = FALSE)
  w <- as.numeric(w)
  if (length(w) != ncol(R)) stop("w must match the columns of r.", call. = FALSE)
  e <- x - as.numeric(R %*% w)
  list(error = e, mse = mean(e^2), gradient = -2 * e * R,
       method = "e = x - w'r; grad e^2 = -2 e r (no expectation needed)")
}

#' RLS correlation matrix and normal equations
#'
#' \eqn{\Phi(n) = \sum \lambda^{n-i} r(i)r^T(i)} and
#' \eqn{\Theta(n) = \sum \lambda^{n-i} r(i)x(i)}, whose solution
#' \eqn{\Phi w = \Theta} is the RLS weight vector. The effective
#' memory is about \eqn{1/(1-\lambda)} samples, and that -- not n --
#' governs how fast RLS tracks a change. Mirrors
#' \code{morie.fn.rng165} and \code{rng166}.
#'
#' @param r matrix of reference vectors.
#' @param x optional primary input, for Theta and the weights.
#' @param lam forgetting factor in (0, 1].
#' @return list: Phi, Theta, weights, effective_memory.
#' @references Rangayyan (2015), Ch. 3.
#' @examples
#' morie_rls_phi(matrix(rnorm(40), ncol = 2), lam = 0.99)$effective_memory
#' @export
morie_rls_phi <- function(r, x = NULL, lam = 0.99) {
  R <- as.matrix(r)
  lam <- as.numeric(lam)
  if (!isTRUE(lam > 0 && lam <= 1)) {
    stop("lam must lie in (0, 1].", call. = FALSE)
  }
  n <- nrow(R)
  w <- lam^((n - 1):0)
  Phi <- t(R * w) %*% R
  out <- list(Phi = Phi, effective_memory = if (lam == 1) Inf else 1 / (1 - lam),
              lam = lam, n = n,
              method = "Phi = sum lambda^(n-i) r r'; memory ~ 1/(1-lambda)")
  if (!is.null(x)) {
    xv <- as.numeric(x)
    if (length(xv) != n) stop("x must have one entry per row of r.", call. = FALSE)
    Theta <- as.numeric(t(R * w) %*% xv)
    out$Theta <- Theta
    out$weights <- tryCatch(solve(Phi, Theta),
                            error = function(e) qr.solve(Phi, Theta))
  }
  out
}

#' Cepstral pitch estimate
#'
#' Pitch period at the quefrency of the dominant rahmonic of
#' \eqn{c(q) = \mathrm{IDFT}\{\log|X(f)|\}}. The logarithm turns the
#' product of excitation and envelope spectra into a sum, separating
#' them in quefrency. Mirrors \code{morie.fn.rgcepsp}.
#'
#' @param x numeric signal.
#' @param fs sampling frequency.
#' @param f0_range plausible pitch range in Hz.
#' @return list: f0, period_s, peak_value.
#' @references Rangayyan (2015), Ch. 3.
#' @examples
#' morie_cepstrum_pitch(rnorm(1024), 8000)$f0
#' @export
morie_cepstrum_pitch <- function(x, fs, f0_range = c(50, 500)) {
  x <- as.numeric(x)
  fs <- as.numeric(fs)
  if (fs <= 0) stop("fs must be positive.", call. = FALSE)
  lo <- f0_range[1]
  hi <- f0_range[2]
  if (!isTRUE(lo > 0 && lo < hi)) stop("f0_range must satisfy 0 < lo < hi.", call. = FALSE)
  if (length(x) < 16L) stop("need at least 16 samples.", call. = FALSE)
  spec <- Mod(stats::fft(x))
  ceps <- Re(stats::fft(log(pmax(spec, 1e-300)), inverse = TRUE)) / length(x)
  q <- (seq_along(ceps) - 1) / fs
  band <- which(q >= 1 / hi & q <= 1 / lo)
  if (!length(band)) stop("f0_range maps outside the quefrencies.", call. = FALSE)
  ipk <- band[which.max(ceps[band])]
  T0 <- q[ipk]
  list(f0 = if (T0 > 0) 1 / T0 else NA_real_, period_s = T0,
       peak_value = ceps[ipk],
       method = "log separates excitation from envelope in quefrency")
}

#' Spectral bandwidth by two criteria
#'
#' \code{"3dB"} spans where \eqn{S(f) \ge S_{max}/2}; \code{"99"} is
#' the narrowest band about the peak holding 99% of the power. The two
#' answer different questions and can differ by an order of magnitude
#' on a peaky spectrum. Mirrors \code{morie.fn.rgbwbnd}.
#'
#' @param psd,freqs spectrum and matching frequencies.
#' @param criterion "3dB" or "99".
#' @return list: bandwidth, f_low, f_high, f_peak.
#' @references Rangayyan (2015), Ch. 3.
#' @examples
#' f <- seq(0, 50, length.out = 200)
#' morie_spectral_bandwidth(exp(-(f - 10)^2 / 2), f)$bandwidth
#' @export
morie_spectral_bandwidth <- function(psd, freqs, criterion = "3dB") {
  S <- as.numeric(psd)
  f <- as.numeric(freqs)
  if (length(S) != length(f)) stop("psd and freqs must match.", call. = FALSE)
  if (any(S < 0)) stop("a PSD cannot be negative.", call. = FALSE)
  ipk <- which.max(S)
  if (criterion == "3dB") {
    above <- which(S >= S[ipk] / 2)
    lo <- f[above[1]]
    hi <- f[above[length(above)]]
  } else if (criterion == "99") {
    total <- sum(S)
    if (total <= 0) stop("spectrum has zero power.", call. = FALSE)
    li <- hi_i <- ipk
    acc <- S[ipk]
    while (acc < 0.99 * total && (li > 1 || hi_i < length(S))) {
      left <- if (li > 1) S[li - 1] else -Inf
      right <- if (hi_i < length(S)) S[hi_i + 1] else -Inf
      if (left >= right) {
        li <- li - 1
        acc <- acc + S[li]
      } else {
        hi_i <- hi_i + 1
        acc <- acc + S[hi_i]
      }
    }
    lo <- f[li]
    hi <- f[hi_i]
  } else {
    stop("criterion must be '3dB' or '99'.", call. = FALSE)
  }
  list(bandwidth = hi - lo, f_low = lo, f_high = hi, f_peak = f[ipk],
       criterion = criterion,
       method = "3dB and 99% answer different questions")
}

#' Pan-Tompkins adaptive threshold update
#'
#' \eqn{SPKI = 0.125\,PEAKI + 0.875\,SPKI} for signal peaks and the
#' same 1/8 recursion for noise, with detection threshold
#' \eqn{NPKI + 0.25(SPKI - NPKI)} floating between them. Mirrors
#' \code{morie.fn.rng190}.
#'
#' @param PEAKI peak amplitudes, processed in order.
#' @param SPKI,NPKI running estimates.
#' @param is_signal optional per-peak class.
#' @return list: SPKI, NPKI, threshold, classified.
#' @references Rangayyan (2015), Ch. 4.
#' @examples
#' morie_pan_tompkins_update(c(1, 1, 0.05), SPKI = 1, NPKI = 0.05)$threshold
#' @export
morie_pan_tompkins_update <- function(PEAKI, SPKI = NULL, NPKI = NULL,
                                      is_signal = NULL) {
  peaks <- as.numeric(PEAKI)
  if (!length(peaks)) stop("PEAKI must be non-empty.", call. = FALSE)
  if (any(peaks < 0)) stop("peak amplitudes must be non-negative.", call. = FALSE)
  spki <- if (is.null(SPKI)) peaks[1] else as.numeric(SPKI)
  npki <- if (is.null(NPKI)) peaks[1] / 2 else as.numeric(NPKI)
  flags <- if (is.null(is_signal)) NULL else as.logical(is_signal)
  if (!is.null(flags) && length(flags) != length(peaks)) {
    stop("is_signal must have one entry per peak.", call. = FALSE)
  }
  cls <- logical(length(peaks))
  for (i in seq_along(peaks)) {
    thr <- npki + 0.25 * (spki - npki)
    sig <- if (is.null(flags)) peaks[i] > thr else flags[i]
    if (sig) spki <- 0.125 * peaks[i] + 0.875 * spki
    else npki <- 0.125 * peaks[i] + 0.875 * npki
    cls[i] <- sig
  }
  list(SPKI = spki, NPKI = npki, threshold = npki + 0.25 * (spki - npki),
       classified = cls, n_peaks = length(peaks),
       method = "Pan-Tompkins 1/8 trackers; threshold floats between them")
}

#' Ensemble average across realisations
#'
#' \eqn{\bar x(t) = (1/M)\sum_k x_k(t)}, averaging ACROSS realisations
#' at each time rather than along one record. Coherent averaging of M
#' repeats improves SNR by \eqn{\sqrt M}. Mirrors
#' \code{morie.fn.rng018}.
#'
#' @param x_k matrix, one realisation per row.
#' @return list: ensemble_mean, ensemble_std, snr_gain, M, T.
#' @references Rangayyan (2015), Ch. 3.
#' @examples
#' morie_ensemble_average(matrix(rnorm(200), nrow = 10))$snr_gain
#' @export
morie_ensemble_average <- function(x_k) {
  X <- as.matrix(x_k)
  if (!nrow(X) || !ncol(X)) stop("x_k must be non-empty.", call. = FALSE)
  list(ensemble_mean = colMeans(X),
       ensemble_std = apply(X, 2, stats::sd),
       snr_gain = sqrt(nrow(X)), M = nrow(X), T = ncol(X),
       method = "mean ACROSS realisations; SNR gain sqrt(M)")
}

#' Transfer function and coherence
#'
#' \eqn{H(f) = S_{xy}/S_{xx}} with
#' \eqn{\gamma^2 = |S_{xy}|^2/(S_{xx}S_{yy})}. Coherence near 1
#' validates H; a low value means noise or nonlinearity dominates and
#' H is meaningless there. A SINGLE segment gives coherence
#' identically 1 and is refused. Mirrors \code{morie.fn.rgtfe}.
#'
#' @param x,y numeric input and output.
#' @param fs sampling frequency.
#' @param nperseg segment length.
#' @return list: freqs, H, magnitude, coherence, n_segments.
#' @references Rangayyan (2015), Ch. 3.
#' @examples
#' set.seed(1); x <- rnorm(512)
#' morie_transfer_function(x, x, nperseg = 128)$n_segments
#' @export
morie_transfer_function <- function(x, y, fs = 1, nperseg = NULL) {
  x <- as.numeric(x)
  y <- as.numeric(y)
  if (length(x) != length(y)) stop("x and y must match.", call. = FALSE)
  fs <- as.numeric(fs)
  if (fs <= 0) stop("fs must be positive.", call. = FALSE)
  N <- length(x)
  seg <- if (is.null(nperseg)) max(8L, N %/% 8L) else as.integer(nperseg)
  if (seg < 2L || seg > N) stop("nperseg out of range.", call. = FALSE)
  step <- seg %/% 2L
  starts <- seq(1L, N - seg + 1L, by = step)
  if (length(starts) < 2L) {
    stop(paste("coherence needs at least 2 segments; a single segment gives",
               "gamma^2 == 1 everywhere and is uninformative."), call. = FALSE)
  }
  w <- 0.5 - 0.5 * cos(2 * pi * (seq_len(seg) - 1) / (seg - 1))
  Sxx <- Syy <- 0
  Sxy <- complex(seg %/% 2L + 1L)
  for (s in starts) {
    X <- stats::fft(x[s:(s + seg - 1L)] * w)[seq_len(seg %/% 2L + 1L)]
    Y <- stats::fft(y[s:(s + seg - 1L)] * w)[seq_len(seg %/% 2L + 1L)]
    Sxx <- Sxx + Mod(X)^2
    Syy <- Syy + Mod(Y)^2
    Sxy <- Sxy + Conj(X) * Y
  }
  Hf <- Sxy / pmax(Sxx, 1e-300)
  list(freqs = seq(0, fs / 2, length.out = seg %/% 2L + 1L), H = Hf,
       magnitude = Mod(Hf), phase = Arg(Hf),
       coherence = pmin(pmax(Mod(Sxy)^2 / pmax(Sxx * Syy, 1e-300), 0), 1),
       n_segments = length(starts),
       method = "H = Sxy/Sxx with coherence; low gamma^2 invalidates H")
}

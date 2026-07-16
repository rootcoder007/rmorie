# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Native digital-signal-processing engines (feat/native-specializations,
# module 20). Replaces the signal package (butter, filtfilt, fir1,
# windows, hilbert, findpeaks, Welch PSD/CSD/coherence) and the
# wavelets package (dwt/idwt for haar/d4/la8) with base-R
# implementations. tests/cross validates against signal/wavelets where
# installed.

# ---------------------------------------------------------------------------
# IIR design: Butterworth via bilinear transform
# ---------------------------------------------------------------------------

#' Internal helper: polynomial from roots (real coefficients)
#' @noRd
.morie_dsp_poly <- function(roots_) {
  p <- 1 + 0i
  for (r in roots_) p <- c(p, 0) - c(0, p * r)
  re <- Re(p)
  re[abs(re) < 1e-12 & abs(Im(p)) < 1e-8] <- 0
  re
}

#' Internal helper: Butterworth digital filter design
#'
#' `W` is the normalized cutoff (Nyquist = 1), scalar for
#' low/high-pass, length-2 for band-pass. Returns list(b, a) matching
#' signal::butter's convention.
#'
#' @srrstats {G1.0} Butterworth (1930) prototype + bilinear transform
#'   (Oppenheim & Schafer, Discrete-Time Signal Processing).
#' @noRd
.morie_dsp_butter <- function(n, W,
                              type = c("low", "high", "pass", "stop")) {
  type <- match.arg(type)
  # analog prototype poles (unit cutoff)
  k <- seq_len(n)
  p_proto <- exp(1i * pi * (2 * k + n - 1) / (2 * n))
  if (type %in% c("low", "high")) {
    wc <- tan(pi * W[1] / 2)
    if (type == "low") {
      p_a <- wc * p_proto
      z_a <- complex(0)                      # zeros at infinity
      gain_pt <- 1 + 0i                      # DC (z = 1)
    } else {
      p_a <- wc / p_proto
      z_a <- rep(0 + 0i, n)                  # zeros at s = 0
      gain_pt <- -1 + 0i                     # Nyquist (z = -1)
    }
  } else if (type == "pass") {
    w1 <- tan(pi * W[1] / 2); w2 <- tan(pi * W[2] / 2)
    bw <- w2 - w1; w0 <- sqrt(w1 * w2)
    # s -> (s^2 + w0^2) / (bw * s)
    p_a <- c()
    for (p in p_proto) {
      disc <- sqrt((bw * p / 2)^2 - w0^2 + 0i)
      p_a <- c(p_a, bw * p / 2 + disc, bw * p / 2 - disc)
    }
    z_a <- rep(0 + 0i, n)
    gain_pt <- exp(1i * 2 * atan(w0))        # z at center frequency
  } else {
    # band-stop: s -> (bw * s) / (s^2 + w0^2)
    w1 <- tan(pi * W[1] / 2); w2 <- tan(pi * W[2] / 2)
    bw <- w2 - w1; w0 <- sqrt(w1 * w2)
    p_a <- c()
    for (p in p_proto) {
      q <- bw / (2 * p)
      disc <- sqrt(q^2 - w0^2 + 0i)
      p_a <- c(p_a, q + disc, q - disc)
    }
    z_a <- rep(c(1i * w0, -1i * w0), n)
    gain_pt <- 1 + 0i                        # DC passes in a stopband
  }
  # bilinear transform s = (z - 1)/(z + 1)
  p_d <- (1 + p_a) / (1 - p_a)
  z_d <- if (length(z_a)) (1 + z_a) / (1 - z_a) else complex(0)
  # zeros at infinity map to z = -1
  n_inf <- length(p_d) - length(z_d)
  z_d <- c(z_d, rep(-1 + 0i, n_inf))
  b <- .morie_dsp_poly(z_d)
  a <- .morie_dsp_poly(p_d)
  # normalize gain to 1 at the reference point
  H <- sum(b * gain_pt^(-(seq_along(b) - 1))) /
    sum(a * gain_pt^(-(seq_along(a) - 1)))
  b <- b / Mod(H)
  list(b = as.numeric(b), a = as.numeric(a))
}

#' Internal helper: direct-form II transposed IIR/FIR filter
#' @noRd
.morie_dsp_filter <- function(b, a, x) {
  b <- as.numeric(b); a <- as.numeric(a)
  if (a[1] != 1) { b <- b / a[1]; a <- a / a[1] }
  if (length(a) == 1L) {
    # pure FIR: convolution
    y <- stats::convolve(x, rev(b), type = "open")[seq_along(x)]
    return(as.numeric(y))
  }
  # a(L) y = b(L) x  ==  y = AR-recursive(MA-convolved x): both passes
  # run in C via stats::filter. The convolution's leading NAs are the
  # partial sums with zero-padded history.
  u <- as.numeric(stats::filter(x, b, method = "convolution",
                                sides = 1))
  lead <- which(is.na(u))
  u[lead] <- vapply(lead, function(i)
    sum(b[seq_len(i)] * x[i:1]), numeric(1))
  y <- stats::filter(u, -a[-1L], method = "recursive")
  as.numeric(y)
}

#' Internal helper: zero-phase forward-backward filtering
#'
#' Odd-extension padding of length 3 * (filter order), forward pass,
#' reversed pass, trim (the MATLAB/scipy filtfilt convention).
#' @noRd
.morie_dsp_filtfilt <- function(b, a, x) {
  n <- length(x)
  nfilt <- max(length(b), length(a))
  pad <- min(3L * (nfilt - 1L), n - 1L)
  if (pad > 0) {
    # odd extension at both ends
    xx <- c(2 * x[1] - x[seq(pad + 1, 2, by = -1)], x,
            2 * x[n] - x[seq(n - 1, n - pad, by = -1)])
  } else {
    xx <- x
  }
  y <- .morie_dsp_filter(b, a, xx)
  y <- rev(.morie_dsp_filter(b, a, rev(y)))
  if (pad > 0) y <- y[(pad + 1):(pad + n)]
  y
}

# ---------------------------------------------------------------------------
# FIR design + windows
# ---------------------------------------------------------------------------

#' Internal helper: cosine windows
#' @noRd
.morie_dsp_window <- function(name, n) {
  k <- seq(0, n - 1) / (n - 1)
  switch(name,
    hamming = 0.54 - 0.46 * cos(2 * pi * k),
    hanning = ,
    hann = 0.5 - 0.5 * cos(2 * pi * k),
    blackman = 0.42 - 0.5 * cos(2 * pi * k) + 0.08 * cos(4 * pi * k),
    rectangular = rep(1, n),
    stop("Unknown window: ", name))
}

#' Internal helper: windowed-sinc FIR design (signal::fir1 convention)
#'
#' `n` is the filter ORDER (returns n + 1 taps); `W` normalized with
#' Nyquist = 1.
#' @noRd
.morie_dsp_fir1 <- function(n, W, type = c("low", "high", "pass"),
                            window = "hamming") {
  type <- match.arg(type)
  m <- seq(0, n) - n / 2
  # ideal low-pass with cutoff pi*fc (Nyquist = 1 convention)
  sinc <- function(fc) ifelse(m == 0, fc, sin(pi * fc * m) / (pi * m))
  h <- switch(type,
    low = sinc(W[1]),
    high = { d <- ifelse(m == 0, 1, 0); d - sinc(W[1]) },
    pass = sinc(W[2]) - sinc(W[1]))
  w <- if (is.function(window)) {
    window
  } else if (is.numeric(window)) {
    if (length(window) != n + 1L) {
      stop("numeric window must have length order + 1 (", n + 1L,
           "), got ", length(window), call. = FALSE)
    }
    window
  } else {
    .morie_dsp_window(window, n + 1L)
  }
  h <- h * w
  # normalize: unity gain at DC (low/pass ref) or Nyquist (high)
  if (type == "low") {
    h <- h / sum(h)
  } else if (type == "high") {
    h <- h / sum(h * cos(pi * (seq_along(h) - 1)))
  } else {
    f0 <- (W[1] + W[2]) / 2
    h <- h / abs(sum(h * exp(-1i * pi * f0 * (seq_along(h) - 1))))
  }
  as.numeric(h)
}

# ---------------------------------------------------------------------------
# Analytic signal, peaks
# ---------------------------------------------------------------------------

#' Internal helper: analytic signal via FFT (Hilbert transform)
#' @noRd
.morie_dsp_hilbert <- function(x) {
  n <- length(x)
  X <- stats::fft(x)
  h <- numeric(n)
  if (n %% 2 == 0) {
    h[c(1, n / 2 + 1)] <- 1
    h[2:(n / 2)] <- 2
  } else {
    h[1] <- 1
    h[2:((n + 1) / 2)] <- 2
  }
  stats::fft(X * h, inverse = TRUE) / n
}

#' Internal helper: local maxima above a height, minimum spacing
#' @noRd
.morie_dsp_findpeaks <- function(x, min_height = -Inf,
                                 min_distance = 1L) {
  n <- length(x)
  if (n < 3L) return(integer(0))
  cand <- which(x[2:(n - 1)] > x[1:(n - 2)] &
                  x[2:(n - 1)] >= x[3:n]) + 1L
  cand <- cand[x[cand] >= min_height]
  if (!length(cand) || min_distance <= 1L) return(cand)
  cand <- cand[order(x[cand], decreasing = TRUE)]
  taken <- integer(0)
  for (p in cand) {
    if (!length(taken) || all(abs(taken - p) >= min_distance)) {
      taken <- c(taken, p)
    }
  }
  sort(taken)
}

# ---------------------------------------------------------------------------
# Welch spectra
# ---------------------------------------------------------------------------

#' Internal helper: Welch cross-spectral density
#'
#' Segments of length `nfft` with 50 percent overlap, Hamming window,
#' averaged cross-periodograms. `y = x` gives the PSD.
#' @noRd
.morie_dsp_welch_csd <- function(x, y = x, nfft = 256L, fs = 1,
                                 overlap = 0.5) {
  n <- length(x)
  nfft <- min(nfft, n)
  step <- max(1L, floor(nfft * (1 - overlap)))
  starts <- seq(1L, n - nfft + 1L, by = step)
  w <- .morie_dsp_window("hamming", nfft)
  U <- sum(w^2)
  acc <- complex(nfft)
  for (s in starts) {
    seg_x <- x[s:(s + nfft - 1L)] * w
    seg_y <- y[s:(s + nfft - 1L)] * w
    acc <- acc + Conj(stats::fft(seg_x)) * stats::fft(seg_y)
  }
  S <- acc / (length(starts) * U * fs)
  n_out <- floor(nfft / 2) + 1L
  list(freq = (seq_len(n_out) - 1L) * fs / nfft,
       spec = S[seq_len(n_out)])
}

#' Internal helper: magnitude-squared coherence via Welch
#' @noRd
.morie_dsp_coherence <- function(x, y, nfft = 256L, fs = 1) {
  sxx <- .morie_dsp_welch_csd(x, x, nfft, fs)
  syy <- .morie_dsp_welch_csd(y, y, nfft, fs)
  sxy <- .morie_dsp_welch_csd(x, y, nfft, fs)
  coh <- Mod(sxy$spec)^2 / (Re(sxx$spec) * Re(syy$spec))
  list(freq = sxx$freq, coherence = pmin(pmax(Re(coh), 0), 1))
}

# ---------------------------------------------------------------------------
# Discrete wavelet transform (haar / d4 / la8, periodic boundary)
# ---------------------------------------------------------------------------

#' Internal helper: scaling filters (Daubechies / least-asymmetric)
#' @noRd
.morie_dsp_wt_filter <- function(name) {
  g <- switch(tolower(name),
    haar = c(0.7071067811865475, 0.7071067811865475),
    d4 = c(0.4829629131445341, 0.8365163037378079,
           0.2241438680420134, -0.1294095225512604),
    d6 = c(0.3326705529500825, 0.8068915093110924,
           0.4598775021184914, -0.1350110200102546,
           -0.0854412738820267, 0.0352262918857095),
    d8 = c(0.2303778133088964, 0.7148465705529154,
           0.6308807679298587, -0.0279837694168599,
           -0.1870348117190931, 0.0308413818355607,
           0.0328830116668852, -0.0105974017850690),
    la8 = c(-0.07576571478935668, -0.02963552764596039,
            0.49761866763256290, 0.80373875180538600,
            0.29785779560560505, -0.09921954357695636,
            -0.01260396726226383, 0.03222310060407815),
    stop("Unknown wavelet filter: ", name))
  L <- length(g)
  h <- rev(g) * (-1)^(seq_len(L) - 1)   # QMF wavelet filter
  list(g = g, h = h)
}

#' Internal helper: one DWT stage (periodic, downsample by 2)
#' @noRd
.morie_dsp_dwt_stage <- function(v, g, h) {
  N <- length(v)
  L <- length(g)
  half <- N %/% 2
  W <- numeric(half); V <- numeric(half)
  tt <- seq_len(half)
  for (l in seq_len(L)) {
    idx <- (2L * tt - l) %% N + 1L
    W <- W + h[l] * v[idx]
    V <- V + g[l] * v[idx]
  }
  list(W = W, V = V)
}

#' Internal helper: inverse DWT stage (periodic, upsample by 2)
#'
#' Exact adjoint of the analysis stage: the transform is orthonormal,
#' so the inverse is the transpose of the analysis operator.
#' @noRd
.morie_dsp_idwt_stage <- function(W, V, g, h) {
  half <- length(V)
  N <- 2L * half
  L <- length(g)
  v <- numeric(N)
  tt <- seq_len(half)
  for (l in seq_len(L)) {
    j <- (2L * tt - l) %% N + 1L   # distinct j per t at fixed l
    v[j] <- v[j] + h[l] * W + g[l] * V
  }
  v
}

#' Internal helper: multi-level DWT (pyramid), periodic boundary
#'
#' Returns list(W = list of detail vectors level 1..J, V = final
#' smooth, filter, n_levels, n).
#' @noRd
.morie_dsp_dwt <- function(x, filter = "la8", n_levels = 4L) {
  f <- .morie_dsp_wt_filter(filter)
  v <- as.numeric(x)
  n <- length(v)
  J <- min(n_levels, floor(log2(n)))
  W <- vector("list", J)
  for (j in seq_len(J)) {
    st <- .morie_dsp_dwt_stage(v, f$g, f$h)
    W[[j]] <- st$W
    v <- st$V
  }
  list(W = W, V = v, filter = filter, n_levels = J, n = n)
}

#' Internal helper: inverse multi-level DWT
#' @noRd
.morie_dsp_idwt <- function(dw) {
  f <- .morie_dsp_wt_filter(dw$filter)
  v <- dw$V
  for (j in rev(seq_len(dw$n_levels))) {
    v <- .morie_dsp_idwt_stage(dw$W[[j]], v, f$g, f$h)
  }
  v
}

#' Internal helper: Savitzky-Golay smoothing (native)
#'
#' Least-squares local polynomial convolution coefficients; edges use
#' the asymmetric fits (the standard SG treatment).
#' @noRd
.morie_dsp_sgolay <- function(x, polyorder = 3L, window_length = 11L) {
  n <- length(x)
  m <- (window_length - 1L) %/% 2L
  A <- outer(-m:m, 0:polyorder, "^")
  H <- A %*% solve(crossprod(A)) %*% t(A)
  # central smoothing coefficients: middle row of the hat matrix
  h_mid <- H[m + 1L, ]
  y <- as.numeric(stats::filter(x, rev(h_mid), sides = 2))
  # asymmetric edge fits
  for (i in seq_len(m)) {
    y[i] <- sum(H[i, ] * x[seq_len(window_length)])
    y[n - i + 1L] <- sum(H[window_length - i + 1L, ]
                         * x[(n - window_length + 1L):n])
  }
  y
}

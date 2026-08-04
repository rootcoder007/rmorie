# Rangayyan signal-analysis batch 1: AIC order selection, Bartlett PSD,
# AR -> cepstrum.  Mirror of the Python arm; formulas read off the PDF
# held in the corpus (Biomedical Signal Analysis, 2024).  Nothing here
# calls stats:: or signal::.

AICorder <- function(prediction_errors, n_samples, window = "hamming") {
  # Rangayyan eq. (7.60):  I(P) = log(eps_P) + 2P/Ne,  Ne = 0.4 N for a
  # Hamming window -- the EFFECTIVE sample count after windowing, which
  # is what distinguishes this from the textbook N log(sigma^2) + 2p.
  eps <- as.numeric(prediction_errors)
  if (!length(eps)) stop("need at least one prediction error")
  if (any(eps <= 0)) stop("prediction errors must be positive")
  n <- as.integer(n_samples)
  if (n <= 0) stop("n_samples must be positive")
  frac <- if (is.character(window)) {
    switch(tolower(window), hamming = 0.4, rectangular = 1, none = 1,
           stop("unknown window: ", window))
  } else {
    f <- as.numeric(window)
    if (f <= 0 || f > 1) stop("effective-sample fraction must be in (0, 1]")
    f
  }
  n_eff <- frac * n
  crit <- log(eps) + 2 * seq_along(eps) / n_eff
  list(order = which.min(crit), criterion = crit, n_effective = n_eff,
       method = "Rangayyan (2024) eq. (7.60)")
}

.morie_dft_power <- function(seg) {
  # |DFT|^2 per bin by direct evaluation: exact at any M, no padding.
  m <- length(seg)
  vapply(0:(m %/% 2), function(k) {
    ang <- -2 * pi * k * (0:(m - 1)) / m
    re <- .morie_fsum(seg * cos(ang))
    im <- .morie_fsum(seg * sin(ang))
    re * re + im * im
  }, numeric(1))
}

BartlettPSD <- function(x, fs = 1, n_segments = NULL,
                        segment_length = NULL) {
  # Rangayyan eqs. (6.14)-(6.16): split into K DISJOINT segments of M
  # samples, periodogram each, take the sample mean.  Averaging K
  # independent periodograms divides the variance by K and multiplies
  # the resolution bandwidth by K -- the trade the method exists for.
  # Welch's overlapping variant is a different estimator.
  xs <- as.numeric(x)
  n <- length(xs)
  if (n < 2) stop("need at least two samples")
  if (is.null(n_segments) == is.null(segment_length))
    stop("give exactly one of n_segments, segment_length")
  if (!is.null(n_segments)) {
    k <- as.integer(n_segments)
    if (k < 1) stop("n_segments must be positive")
    m <- n %/% k
  } else {
    m <- as.integer(segment_length)
    if (m < 2) stop("segment_length must be at least 2")
    k <- n %/% m
  }
  if (k < 1 || m < 2) stop("segmentation leaves no usable segment")
  acc <- NULL
  for (i in seq_len(k)) {
    seg <- xs[((i - 1) * m + 1):(i * m)]
    p <- .morie_dft_power(seg) / m
    acc <- if (is.null(acc)) p else acc + p
  }
  psd <- acc / k
  list(psd = psd, freqs = (seq_along(psd) - 1) * fs / m,
       n_segments = k, segment_length = m,
       method = "Rangayyan (2024) eqs. (6.14)-(6.16)")
}

ARtoCepstrum <- function(a_coeffs, gain = NULL) {
  # Rangayyan eq. (7.65):
  #   h(1) = -a1;  h(n) = -a_n - sum_{k=1}^{n-1} (1 - k/n) a_k h(n-k)
  # Going through the AR coefficients avoids the phase unwrapping the
  # FFT cepstrum needs (Section 4.7.3).
  a <- as.numeric(a_coeffs)
  p <- length(a)
  if (!p) stop("need at least one AR coefficient")
  h <- numeric(p + 1)
  for (n in seq_len(p)) {
    acc <- -a[n]
    if (n > 1) {
      k <- seq_len(n - 1)
      acc <- acc - .morie_fsum((1 - k / n) * a[k] * h[n - k + 1])
    }
    h[n + 1] <- acc
  }
  c0 <- NULL
  if (!is.null(gain)) {
    g <- as.numeric(gain)
    if (g <= 0) stop("gain must be positive")
    c0 <- log(g)
  }
  list(cepstrum = h[-1], c0 = c0, order = p,
       method = "Rangayyan (2024) eq. (7.65)")
}

# pre-policy spellings
morie_ar_order_aic <- AICorder
morie_bartlett_psd <- BartlettPSD
morie_ar_to_cepstrum <- ARtoCepstrum

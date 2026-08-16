# Rangayyan correlation, spectral density, coherence, the matched-filter
# derivation, and PSD moments.  Mirror of the Python bsacorr module.
#
# Equation numbers verified in the PDF: 3.81, 3.91, 3.96, 4.24-4.32,
# 4.33-4.49, 4.53-4.57, 6.32-6.43.

#' .morie_rg_xcorr
#'
#' Part of the rangayyan_corr implementation; see the file header for
#' the source it follows.
#'
#' @param x See Usage.
#' @param y See Usage.
#' @param maxlag Defaults to \code{NULL}.
#' @return A list with \code{lags}, \code{values}.
#' @export
.morie_rg_xcorr <- function(x, y, maxlag = NULL) {
  n <- length(x)
  m <- length(y)
  lo <- if (is.null(maxlag)) -(n - 1L) else -as.integer(maxlag)
  hi <- if (is.null(maxlag)) (m - 1L) else as.integer(maxlag)
  lags <- lo:hi
  vals <- vapply(lags, function(k) {
    i <- seq_len(n)
    j <- i + k
    keep <- j >= 1L & j <= m
    if (!any(keep)) 0 else .morie_fsum(x[i[keep]] * y[j[keep]])
  }, numeric(1))
  list(lags = lags, values = vals)
}

#' Eqs (4.24)-(4.25): the inner product and the correlation coefficient
#'
#' it normalizes to.  The book notes the means may be removed first (eq
#' 3.97), which is a DIFFERENT quantity -- without removal gamma is the
#' cosine between the raw vectors, with it, Pearson\'s r.
#'
#' @param x See Usage.
#' @param y See Usage.
#' @param subtract_mean Defaults to \code{FALSE}.
#' @return A list with \code{dot_product}, \code{gamma}, \code{energy_x}, \code{energy_y}, \code{n}, \code{mean_removed}, \code{method}.
#' @export
DotProd <- function(x, y, subtract_mean = FALSE) {
  # eqs (4.24)-(4.25): the inner product and the correlation coefficient
  # it normalizes to.  The book notes the means may be removed first
  # (eq 3.97), which is a DIFFERENT quantity -- without removal gamma is
  # the cosine between the raw vectors, with it, Pearson's r.
  xs <- as.numeric(x)
  ys <- as.numeric(y)
  if (length(xs) != length(ys)) stop("x and y must have the same length")
  if (!length(xs)) stop("need at least one sample")
  if (subtract_mean) {
    xs <- xs - .morie_fsum(xs) / length(xs)
    ys <- ys - .morie_fsum(ys) / length(ys)
  }
  dp <- .morie_fsum(xs * ys)
  ex <- .morie_fsum(xs * xs)
  ey <- .morie_fsum(ys * ys)
  list(
    dot_product = dp,
    gamma = if (ex > 0 && ey > 0) dp / sqrt(ex * ey) else NULL,
    energy_x = ex, energy_y = ey, n = length(xs),
    mean_removed = isTRUE(subtract_mean),
    method = "Rangayyan (2024) eqs. (4.24)-(4.25)"
  )
}

#' Eq (4.26): the continuous counterpart of eq (4.24).  Tabulated it is
#'
#' the discrete inner product SCALED BY dt; dropping the dt turns an
#' integral into a sum and leaves the answer wrong by a factor of the
#' sampling interval.
#'
#' @param x See Usage.
#' @param y See Usage.
#' @param t Defaults to \code{NULL}.
#' @param dt Defaults to \code{1}.
#' @return A list with \code{theta}, \code{integrand}, \code{discrete_sum}, \code{duration}, \code{n}, \code{method}.
#' @export
ContProj <- function(x, y, t = NULL, dt = 1) {
  # eq (4.26): the continuous counterpart of eq (4.24).  Tabulated it is
  # the discrete inner product SCALED BY dt; dropping the dt turns an
  # integral into a sum and leaves the answer wrong by a factor of the
  # sampling interval.
  xs <- as.numeric(x)
  ys <- as.numeric(y)
  if (length(xs) != length(ys)) stop("x and y must have the same length")
  if (length(xs) < 2L) stop("need at least two samples to integrate")
  ts <- if (is.null(t)) (seq_along(xs) - 1) * as.numeric(dt) else as.numeric(t)
  if (length(ts) != length(xs)) stop("t and x must have the same length")
  prod <- xs * ys
  list(
    theta = .morie_rg_gridint(prod, ts), integrand = prod,
    discrete_sum = .morie_fsum(prod),
    duration = ts[length(ts)] - ts[1], n = length(xs),
    method = "Rangayyan (2024) eq. (4.26)"
  )
}

#' Eq (4.29): Theta_xy = E[x(n) y^T(n)], an N x N matrix carrying all
#'
#' pairwise delays -- which is why the outer product appears in the
#' Wiener and RLS normal equations.  Under stationarity the entries
#' depend only on i - j; that Toeplitz structure is MEASURED, and the
#' deviation is the number to look at, not the boolean.
#'
#' @param x See Usage.
#' @param y See Usage.
#' @param order See Usage.
#' @param tol Defaults to \code{0.001}.
#' @return A list with \code{theta}, \code{order}, \code{n_positions}, \code{toeplitz_deviation}, \code{relative_deviation}, \code{tol}, \code{toeplitz}, \code{method}.
#' @export
CcfOuter <- function(x, y, order, tol = 1e-3) {
  # eq (4.29): Theta_xy = E[x(n) y^T(n)], an N x N matrix carrying all
  # pairwise delays -- which is why the outer product appears in the
  # Wiener and RLS normal equations.  Under stationarity the entries
  # depend only on i - j; that Toeplitz structure is MEASURED, and the
  # deviation is the number to look at, not the boolean.
  xs <- as.numeric(x)
  ys <- as.numeric(y)
  if (length(xs) != length(ys)) stop("x and y must have the same length")
  n <- as.integer(order)
  if (n < 1L) stop("order must be at least 1")
  if (length(xs) < n) stop(sprintf("need at least %d samples", n))
  m <- length(xs) - n + 1L
  k <- n:length(xs)
  mat <- matrix(0, n, n)
  for (i in seq_len(n)) {
    for (j in seq_len(n)) {
      mat[i, j] <- .morie_fsum(xs[k - i + 1L] * ys[k - j + 1L]) / m
    }
  }
  dev <- 0
  for (d in (-(n - 1L)):(n - 1L)) {
    idx <- seq_len(n)
    jdx <- idx - d
    keep <- jdx >= 1L & jdx <= n
    band <- mat[cbind(idx[keep], jdx[keep])]
    if (length(band) > 1L) dev <- max(dev, max(abs(band - mean(band))))
  }
  scale <- max(abs(mat))
  if (scale == 0) scale <- 1
  list(
    theta = mat, order = n, n_positions = m, toeplitz_deviation = dev,
    relative_deviation = dev / scale, tol = as.numeric(tol),
    toeplitz = dev <= as.numeric(tol) * scale,
    method = "Rangayyan (2024) eq. (4.29)"
  )
}

#' Eqs (4.30)-(4.31): S_xx = |X|^2 and S_xy = X Y*.  Both routes to the
#'
#' CSD -- the transform of the CCF and the product -- are computed and
#' compared; they agree only for the full CIRCULAR lag range, since a
#' truncated CCF gives a smoothed CSD, a different estimator.
#'
#' @param x See Usage.
#' @param y See Usage.
#' @param fs Defaults to \code{1}.
#' @return A list with \code{csd}, \code{via_ccf}, \code{ccf_circular}, \code{freqs}, \code{max_difference}, \code{agrees}, \code{n}, \code{method}.
#' @export
Csd <- function(x, y, fs = 1) {
  # eqs (4.30)-(4.31): S_xx = |X|^2 and S_xy = X Y*.  Both routes to the
  # CSD -- the transform of the CCF and the product -- are computed and
  # compared; they agree only for the full CIRCULAR lag range, since a
  # truncated CCF gives a smoothed CSD, a different estimator.
  xs <- as.numeric(x)
  ys <- as.numeric(y)
  if (length(xs) != length(ys)) stop("x and y must have the same length")
  n <- length(xs)
  if (n < 2L) stop("need at least two samples")
  fx <- .morie_rg_dft(xs)
  fy <- .morie_rg_dft(ys)
  prod <- complex(real = fx$re, imaginary = fx$im) *
    Conj(complex(real = fy$re, imaginary = fy$im))
  idx <- seq_len(n) - 1L
  circ <- vapply(idx, function(k) {
    .morie_fsum(xs * ys[((idx + k) %% n) + 1L])
  }, numeric(1))
  fc <- .morie_rg_dft(circ)
  viaccf <- complex(real = fc$re, imaginary = -fc$im)
  gap <- max(Mod(prod - viaccf))
  scale <- max(Mod(prod))
  if (scale == 0) scale <- 1
  list(
    csd = prod, via_ccf = viaccf, ccf_circular = circ,
    freqs = idx * as.numeric(fs) / n, max_difference = gap,
    agrees = gap <= 1e-8 * scale, n = n,
    method = "Rangayyan (2024) eqs. (4.30)-(4.31)"
  )
}

#' Eq (4.32).  The book is emphatic: computed from two single
#'
#' observations the magnitude is UNITY AT EVERY FREQUENCY, which is
#' incorrect; each density must be estimated by AVERAGING over several
#' observations.  So a single segment is refused rather than returning
#' the meaningless all-ones answer.
#'
#' @param x See Usage.
#' @param y See Usage.
#' @param fs Defaults to \code{1}.
#' @param nperseg Defaults to \code{NULL}.
#' @param noverlap Defaults to \code{NULL}.
#' @return A list with \code{coherence}, \code{phase}, \code{sxx}, \code{syy}, \code{sxy}, \code{freqs}, \code{n_segments}, \code{nperseg}, \code{method}.
#' @export
Cohere <- function(x, y, fs = 1, nperseg = NULL, noverlap = NULL) {
  # eq (4.32).  The book is emphatic: computed from two single
  # observations the magnitude is UNITY AT EVERY FREQUENCY, which is
  # incorrect; each density must be estimated by AVERAGING over several
  # observations.  So a single segment is refused rather than returning
  # the meaningless all-ones answer.
  xs <- as.numeric(x)
  ys <- as.numeric(y)
  if (length(xs) != length(ys)) stop("x and y must have the same length")
  n <- length(xs)
  m <- if (is.null(nperseg)) max(8L, n %/% 8L) else as.integer(nperseg)
  if (m < 4L) stop("segments must hold at least four samples")
  step <- m - (if (is.null(noverlap)) m %/% 2L else as.integer(noverlap))
  if (step < 1L) stop("noverlap must be smaller than nperseg")
  starts <- seq(1L, n - m + 1L, by = step)
  if (length(starts) < 2L) {
    stop(sprintf(
      paste(
        "eq. (4.32) needs the spectra AVERAGED over several",
        "observations; %d segment(s) of %d samples would give",
        "a coherence of 1 at every frequency"
      ),
      length(starts), m
    ))
  }
  half <- m %/% 2L + 1L
  sxx <- numeric(half)
  syy <- numeric(half)
  sxy <- complex(half)
  for (s in starts) {
    a <- xs[s:(s + m - 1L)]
    b <- ys[s:(s + m - 1L)]
    fa <- .morie_rg_dft(a - mean(a))
    fb <- .morie_rg_dft(b - mean(b))
    A <- complex(
      real = fa$re[seq_len(half)],
      imaginary = fa$im[seq_len(half)]
    )
    B <- complex(
      real = fb$re[seq_len(half)],
      imaginary = fb$im[seq_len(half)]
    )
    sxx <- sxx + Mod(A)^2
    syy <- syy + Mod(B)^2
    sxy <- sxy + A * Conj(B)
  }
  kn <- length(starts)
  sxx <- sxx / kn
  syy <- syy / kn
  sxy <- sxy / kn
  den <- sxx * syy
  gam <- ifelse(den > 0, sqrt(Mod(sxy)^2 / den), 0)
  list(
    coherence = gam, phase = Arg(sxy), sxx = sxx, syy = syy, sxy = sxy,
    freqs = (seq_len(half) - 1L) * as.numeric(fs) / m,
    n_segments = kn, nperseg = m,
    method = "Rangayyan (2024) eq. (4.32)"
  )
}

#' The square of eq (4.32).  The two forms are NOT interchangeable:
#'
#' 0.5 magnitude coherence is 0.25 magnitude-squared coherence.
#'
#' @param x See Usage.
#' @param y See Usage.
#' @param fs Defaults to \code{1}.
#' @param nperseg Defaults to \code{NULL}.
#' @param noverlap Defaults to \code{NULL}.
#' @return The value of \code{r}, as built in the body.
#' @export
Msc <- function(x, y, fs = 1, nperseg = NULL, noverlap = NULL) {
  # The square of eq (4.32).  The two forms are NOT interchangeable:
  # 0.5 magnitude coherence is 0.25 magnitude-squared coherence.
  r <- Cohere(x, y, fs = fs, nperseg = nperseg, noverlap = noverlap)
  r$msc <- r$coherence^2
  r$magnitude_coherence <- r$coherence
  r$method <- "Rangayyan (2024) eq. (4.32), squared"
  r
}

#' Eqs (4.25), (4.28): the correlation coefficient at every shift
#'
#' gamma is normalized at EVERY shift by the energy of the segment under
#' the template, which is what makes it a correlation rather than a
#' convolution -- a large low-frequency excursion cannot produce a
#' spurious match because the normalization divides the amplitude out.
#'
#' @param x See Usage.
#' @param ref See Usage.
#' @param threshold Defaults to \code{NULL}.
#' @param subtract_mean Defaults to \code{TRUE}.
#' @return The value of \code{out}, as built in the body.
#' @export
Template <- function(x, ref, threshold = NULL, subtract_mean = TRUE) {
  # eqs (4.25), (4.28): the correlation coefficient at every shift.
  # gamma is normalized at EVERY shift by the energy of the segment under
  # the template, which is what makes it a correlation rather than a
  # convolution -- a large low-frequency excursion cannot produce a
  # spurious match because the normalization divides the amplitude out.
  xs <- as.numeric(x)
  ts <- as.numeric(ref)
  n <- length(xs)
  m <- length(ts)
  if (m < 2L) stop("the template needs at least two samples")
  if (n < m) stop("the signal is shorter than the template")
  if (subtract_mean) ts <- ts - mean(ts)
  et <- .morie_fsum(ts * ts)
  if (et <= 0) stop("the template has zero energy")
  gam <- vapply(seq_len(n - m + 1L), function(k) {
    seg <- xs[k:(k + m - 1L)]
    if (subtract_mean) seg <- seg - mean(seg)
    es <- .morie_fsum(seg * seg)
    if (es <= 0) 0 else .morie_fsum(seg * ts) / sqrt(es * et)
  }, numeric(1))
  best <- which.max(gam)
  out <- list(
    gamma = gam, best_shift = best - 1L, best_gamma = gam[best],
    n_positions = length(gam), template_length = m,
    mean_removed = isTRUE(subtract_mean),
    method = "Rangayyan (2024) eqs. (4.25), (4.28)"
  )
  if (!is.null(threshold)) {
    thr <- as.numeric(threshold)
    above <- gam >= thr
    hits <- integer(0)
    i <- 1L
    while (i <= length(gam)) {
      if (above[i]) {
        j <- i
        while (j < length(gam) && above[j + 1L]) j <- j + 1L
        hits <- c(hits, (i:j)[which.max(gam[i:j])] - 1L)
        i <- j + 1L
      } else {
        i <- i + 1L
      }
    }
    out$detections <- hits
    out$threshold <- thr
    out$n_detections <- length(hits)
  }
  out
}

# ------------------------------------------------------- matched filter

#' Eq (4.33): X(omega) = integral x(t) exp(-j omega t) dt.  An integral,
#'
#' so it carries the sampling interval.
#'
#' @param x See Usage.
#' @param omega See Usage.
#' @param dt Defaults to \code{1}.
#' @return A list with \code{X}, \code{omega}, \code{dt}, \code{n}, \code{method}.
#' @export
MfInput <- function(x, omega, dt = 1) {
  # eq (4.33): X(omega) = integral x(t) exp(-j omega t) dt.  An integral,
  # so it carries the sampling interval.
  xs <- as.numeric(x)
  if (!length(xs)) stop("need at least one sample")
  step <- as.numeric(dt)
  if (step <= 0) stop("dt must be positive")
  ws <- as.numeric(omega)
  idx <- seq_along(xs) - 1L
  vals <- vapply(
    ws, function(w) {
      complex(
        real = .morie_fsum(xs * cos(-w * idx * step)),
        imaginary = .morie_fsum(xs * sin(-w * idx * step))
      ) * step
    },
    complex(1)
  )
  one <- length(ws) == 1L
  list(
    X = if (one) vals[[1]] else vals,
    omega = if (one) ws[[1]] else ws, dt = step, n = length(xs),
    method = "Rangayyan (2024) eq. (4.33)"
  )
}

#' Eq (4.34), computed as y = x * h: exact for a finite record, where
#'
#' the frequency-domain route would need fine enough sampling to avoid
#' wrap-around.  eq (4.38) reads M_y off the peak returned here.
#'
#' @param x See Usage.
#' @param h See Usage.
#' @param dt Defaults to \code{1}.
#' @return A list with \code{y}, \code{t}, \code{peak_index}, \code{peak_time}, \code{peak_magnitude}, \code{dt}, \code{method}.
#' @export
MfOutput <- function(x, h, dt = 1) {
  # eq (4.34), computed as y = x * h: exact for a finite record, where
  # the frequency-domain route would need fine enough sampling to avoid
  # wrap-around.  eq (4.38) reads M_y off the peak returned here.
  xs <- as.numeric(x)
  hs <- as.numeric(h)
  if (!length(xs) || !length(hs)) {
    stop("both signals need at least one sample")
  }
  step <- as.numeric(dt)
  if (step <= 0) stop("dt must be positive")
  y <- .morie_rg_conv(xs, hs) * step
  peak <- which.max(abs(y))
  list(
    y = y, t = (seq_along(y) - 1L) * step, peak_index = peak - 1L,
    peak_time = (peak - 1L) * step, peak_magnitude = abs(y[peak]),
    dt = step, method = "Rangayyan (2024) eq. (4.34)"
  )
}

#' Eq (4.35): S_eta_i = P_eta_i / 2.  The factor of two is the TWO-SIDED
#'
#' convention: integrating the flat density over all f returns P, not
#' twice it.  Getting it wrong is a factor of two in every SNR below.
#'
#' @param power See Usage.
#' @param freqs Defaults to \code{NULL}.
#' @return The value of \code{out}, as built in the body.
#' @export
MfNoiseIn <- function(power, freqs = NULL) {
  # eq (4.35): S_eta_i = P_eta_i / 2.  The factor of two is the TWO-SIDED
  # convention: integrating the flat density over all f returns P, not
  # twice it.  Getting it wrong is a factor of two in every SNR below.
  p <- as.numeric(power)
  if (p < 0) stop("noise power cannot be negative")
  out <- list(
    density = p / 2, power = p, two_sided = TRUE,
    method = "Rangayyan (2024) eq. (4.35)"
  )
  if (!is.null(freqs)) {
    out$psd <- rep(p / 2, length(freqs))
    out$freqs <- as.numeric(freqs)
  }
  out
}

#' Eqs (4.36)-(4.37): S_eta_o = (P/2)|H|^2 and its integral.  eq (4.36)
#'
#' holds only because the input was white, so the density factored out.
#'
#' @param power See Usage.
#' @param H See Usage.
#' @param freqs Defaults to \code{NULL}.
#' @param df Defaults to \code{1}.
#' @return A list with \code{psd}, \code{power}, \code{rms}, \code{input_power}, \code{input_density}, \code{method}.
#' @export
MfNoiseOut <- function(power, H, freqs = NULL, df = 1) {
  # eqs (4.36)-(4.37): S_eta_o = (P/2)|H|^2 and its integral.  eq (4.36)
  # holds only because the input was white, so the density factored out.
  p <- as.numeric(power)
  if (p < 0) stop("noise power cannot be negative")
  Hs <- as.complex(H)
  if (!length(Hs)) stop("need at least one transfer-function sample")
  psd <- (p / 2) * Mod(Hs)^2
  total <- if (!is.null(freqs)) {
    fv <- as.numeric(freqs)
    if (length(fv) != length(Hs)) {
      stop("freqs and H must have the same length")
    }
    if (length(fv) > 1L) .morie_rg_gridint(psd, fv) else psd[1] * as.numeric(df)
  } else {
    .morie_fsum(psd) * as.numeric(df)
  }
  list(
    psd = psd, power = total, rms = if (total > 0) sqrt(total) else 0,
    input_power = p, input_density = p / 2,
    method = "Rangayyan (2024) eqs. (4.36)-(4.37)"
  )
}

#' Eq (4.38): M_y = |y(t0)|, the numerator of the SNR everything else
#'
#' maximizes.  It is a MAGNITUDE, so the phase of X H at t0 is what the
#' optimal filter of eq (4.48) is chosen to align.
#'
#' @param X See Usage.
#' @param H See Usage.
#' @param freqs See Usage.
#' @param t0 See Usage.
#' @return A list with \code{my}, \code{y}, \code{t0}, \code{phase}, \code{method}.
#' @export
MfPeak <- function(X, H, freqs, t0) {
  # eq (4.38): M_y = |y(t0)|, the numerator of the SNR everything else
  # maximizes.  It is a MAGNITUDE, so the phase of X H at t0 is what the
  # optimal filter of eq (4.48) is chosen to align.
  Xs <- as.complex(X)
  Hs <- as.complex(H)
  fv <- as.numeric(freqs)
  if (!(length(Xs) == length(Hs) && length(Hs) == length(fv))) {
    stop("X, H and freqs must have the same length")
  }
  if (length(fv) < 2L) {
    stop("need at least two frequency points to integrate")
  }
  t <- as.numeric(t0)
  ang <- 2 * pi * fv * t
  p <- Xs * Hs * complex(real = cos(ang), imaginary = sin(ang))
  val <- complex(
    real = .morie_rg_gridint(Re(p), fv),
    imaginary = .morie_rg_gridint(Im(p), fv)
  )
  list(
    my = Mod(val), y = val, t0 = t, phase = Arg(val),
    method = "Rangayyan (2024) eq. (4.38)"
  )
}

#' Eq (4.39): a PEAK-to-MEAN ratio, not the mean-to-mean of an ordinary
#'
#' SNR.  The matched filter maximizes the output at ONE INSTANT, which
#' is the right criterion for a known transient and the wrong one for a
#' continuous signal.
#'
#' @param my See Usage.
#' @param noise_power See Usage.
#' @return A list with \code{snr}, \code{snr_db}, \code{amplitude_snr}, \code{my}, \code{noise_power}, \code{peak_to_mean}, \code{method}.
#' @export
MfSnr <- function(my, noise_power) {
  # eq (4.39): a PEAK-to-MEAN ratio, not the mean-to-mean of an ordinary
  # SNR.  The matched filter maximizes the output at ONE INSTANT, which
  # is the right criterion for a known transient and the wrong one for a
  # continuous signal.
  m <- as.numeric(my)
  p <- as.numeric(noise_power)
  if (p <= 0) stop("the output noise power must be positive")
  ratio <- m * m / p
  list(
    snr = ratio, snr_db = if (ratio > 0) 10 * log10(ratio) else -Inf,
    amplitude_snr = m / sqrt(p), my = m, noise_power = p,
    peak_to_mean = TRUE, method = "Rangayyan (2024) eq. (4.39)"
  )
}

#' Eq (4.40): E_x = integral x^2 dt = integral |X|^2 df, Parseval named
#'
#' for the role it plays here -- E_x is constant for a given signal, so
#' maximizing eq (4.41) is equivalent to maximizing eq (4.39).
#'
#' @param x Defaults to \code{NULL}.
#' @param t Defaults to \code{NULL}.
#' @param dt Defaults to \code{1}.
#' @param X Defaults to \code{NULL}.
#' @param freqs Defaults to \code{NULL}.
#' @return The value of \code{out}, as built in the body.
#' @export
SigEnergy <- function(x = NULL, t = NULL, dt = 1, X = NULL, freqs = NULL) {
  # eq (4.40): E_x = integral x^2 dt = integral |X|^2 df, Parseval named
  # for the role it plays here -- E_x is constant for a given signal, so
  # maximizing eq (4.41) is equivalent to maximizing eq (4.39).
  out <- list(method = "Rangayyan (2024) eq. (4.40)")
  if (!is.null(x)) {
    xs <- as.numeric(x)
    if (length(xs) < 2L) stop("need at least two samples to integrate")
    ts <- if (is.null(t)) (seq_along(xs) - 1) * as.numeric(dt) else as.numeric(t)
    out$energy_time <- .morie_rg_gridint(xs * xs, ts)
    out$energy <- out$energy_time
  }
  if (!is.null(X)) {
    if (is.null(freqs)) stop("give the frequency grid alongside X")
    Xs <- as.complex(X)
    fv <- as.numeric(freqs)
    if (length(Xs) != length(fv)) {
      stop("X and freqs must have the same length")
    }
    out$energy_freq <- .morie_rg_gridint(Mod(Xs)^2, fv)
    if (is.null(out$energy)) out$energy <- out$energy_freq
  }
  if (!is.null(out$energy_time) && !is.null(out$energy_freq)) {
    gap <- abs(out$energy_time - out$energy_freq)
    out$max_difference <- gap
    out$parseval_holds <- gap <= 1e-6 * max(out$energy_time, 1)
  }
  if (is.null(out$energy)) stop("give a signal, a spectrum, or both")
  out
}

#' Eq (4.41).  Dividing by the constant E_x is what turns eq (4.39) into
#'
#' something Schwarz\'s inequality applies to; by eq (4.46) the ratio
#' cannot exceed 2/P_eta_i, with equality exactly at eq (4.48).
#'
#' @param X See Usage.
#' @param H See Usage.
#' @param freqs See Usage.
#' @param t0 See Usage.
#' @param noise_power See Usage.
#' @return A list with \code{ratio}, \code{bound}, \code{optimality}, \code{numerator}, \code{energy_h}, \code{energy_x}, \code{method}.
#' @export
MfRatio <- function(X, H, freqs, t0, noise_power) {
  # eq (4.41).  Dividing by the constant E_x is what turns eq (4.39) into
  # something Schwarz's inequality applies to; by eq (4.46) the ratio
  # cannot exceed 2/P_eta_i, with equality exactly at eq (4.48).
  Xs <- as.complex(X)
  Hs <- as.complex(H)
  fv <- as.numeric(freqs)
  if (!(length(Xs) == length(Hs) && length(Hs) == length(fv))) {
    stop("X, H and freqs must have the same length")
  }
  if (length(fv) < 2L) stop("need at least two frequency points")
  p <- as.numeric(noise_power)
  if (p <= 0) stop("the input noise power must be positive")
  num <- MfPeak(Xs, Hs, fv, t0)$my^2
  eh <- .morie_rg_gridint(Mod(Hs)^2, fv)
  ex <- .morie_rg_gridint(Mod(Xs)^2, fv)
  den <- (p / 2) * eh * ex
  if (den <= 0) stop("the denominator of eq. (4.41) vanishes")
  ratio <- num / den
  bound <- 2 / p
  list(
    ratio = ratio, bound = bound,
    optimality = if (bound > 0) ratio / bound else NULL,
    numerator = num, energy_h = eh, energy_x = ex,
    method = "Rangayyan (2024) eqs. (4.41), (4.46)"
  )
}

#' Eq (4.42), with equality exactly when A = K B* -- which, applied with
#'
#' A = H and B = X exp(+j2 pi f t0), IS the matched-filter derivation.
#'
#' @param A See Usage.
#' @param B See Usage.
#' @param grid See Usage.
#' @return A list with \code{lhs}, \code{rhs}, \code{holds}, \code{ratio}, \code{equality}, \code{k}, \code{collinear}, \code{method}.
#' @export
SchwarzC <- function(A, B, grid) {
  # eq (4.42), with equality exactly when A = K B* -- which, applied with
  # A = H and B = X exp(+j2 pi f t0), IS the matched-filter derivation.
  As <- as.complex(A)
  Bs <- as.complex(B)
  g <- as.numeric(grid)
  if (!(length(As) == length(Bs) && length(Bs) == length(g))) {
    stop("A, B and the grid must have the same length")
  }
  if (length(g) < 2L) stop("need at least two grid points to integrate")
  prod <- As * Bs
  inner <- complex(
    real = .morie_rg_gridint(Re(prod), g),
    imaginary = .morie_rg_gridint(Im(prod), g)
  )
  ea <- .morie_rg_gridint(Mod(As)^2, g)
  eb <- .morie_rg_gridint(Mod(Bs)^2, g)
  lhs <- Mod(inner)^2
  rhs <- ea * eb
  keep <- Mod(Bs) > 1e-300
  ks <- if (any(keep)) As[keep] / Conj(Bs[keep]) else complex(0)
  k <- if (length(ks)) ks[1] else NULL
  list(
    lhs = lhs, rhs = rhs, holds = lhs <= rhs * (1 + 1e-9),
    ratio = if (rhs > 0) lhs / rhs else NULL,
    equality = rhs > 0 && abs(lhs - rhs) <= 1e-9 * rhs, k = k,
    collinear = length(ks) > 0 &&
      all(Mod(ks - k) <= 1e-9 * (1 + Mod(k))),
    method = "Rangayyan (2024) eq. (4.42)"
  )
}

#' Eq (4.43), the real case of eq (4.42); equality when a = K b, the two
#'
#' functions collinear as vectors in function space.
#'
#' @param a See Usage.
#' @param b See Usage.
#' @param grid Defaults to \code{NULL}.
#' @param dt Defaults to \code{1}.
#' @return A list with \code{lhs}, \code{rhs}, \code{holds}, \code{equality}, \code{k}, \code{collinear}, \code{method}.
#' @export
SchwarzR <- function(a, b, grid = NULL, dt = 1) {
  # eq (4.43), the real case of eq (4.42); equality when a = K b, the two
  # functions collinear as vectors in function space.
  av <- as.numeric(a)
  bv <- as.numeric(b)
  if (length(av) != length(bv)) stop("a and b must have the same length")
  if (length(av) < 2L) stop("need at least two samples to integrate")
  g <- if (is.null(grid)) {
    (seq_along(av) - 1) * as.numeric(dt)
  } else {
    as.numeric(grid)
  }
  inner <- .morie_rg_gridint(av * bv, g)
  ea <- .morie_rg_gridint(av * av, g)
  eb <- .morie_rg_gridint(bv * bv, g)
  lhs <- inner * inner
  rhs <- ea * eb
  keep <- abs(bv) > 1e-300
  ks <- if (any(keep)) av[keep] / bv[keep] else numeric(0)
  k <- if (length(ks)) ks[1] else NULL
  list(
    lhs = lhs, rhs = rhs, holds = lhs <= rhs * (1 + 1e-9),
    equality = rhs > 0 && abs(lhs - rhs) <= 1e-9 * rhs, k = k,
    collinear = length(ks) > 0 && all(abs(ks - k) <= 1e-9 * (1 + abs(k))),
    method = "Rangayyan (2024) eq. (4.43)"
  )
}

#' Eq (4.44): |a.b| <= |a||b|.  The ratio of the two sides is the cosine
#'
#' between the vectors, which is exactly the correlation coefficient of
#' eq (4.25) on mean-removed signals.
#'
#' @param a See Usage.
#' @param b See Usage.
#' @return A list with \code{lhs}, \code{rhs}, \code{holds}, \code{cosine}, \code{equality}, \code{norm_a}, \code{norm_b}, \code{method}.
#' @export
CauchySch <- function(a, b) {
  # eq (4.44): |a.b| <= |a||b|.  The ratio of the two sides is the cosine
  # between the vectors, which is exactly the correlation coefficient of
  # eq (4.25) on mean-removed signals.
  av <- as.numeric(a)
  bv <- as.numeric(b)
  if (length(av) != length(bv)) stop("a and b must have the same length")
  if (!length(av)) stop("need at least one component")
  dp <- .morie_fsum(av * bv)
  na <- sqrt(.morie_fsum(av * av))
  nb <- sqrt(.morie_fsum(bv * bv))
  list(
    lhs = abs(dp), rhs = na * nb, holds = abs(dp) <= na * nb * (1 + 1e-12),
    cosine = if (na > 0 && nb > 0) dp / (na * nb) else NULL,
    equality = na > 0 && nb > 0 &&
      abs(abs(dp) - na * nb) <= 1e-9 * na * nb,
    norm_a = na, norm_b = nb, method = "Rangayyan (2024) eq. (4.44)"
  )
}

#' Eq (4.45): |a+b| <= |a|+|b|, with equality when the vectors align
#'
#' It is the statement that no combination of two signals carries more
#' amplitude than their amplitudes sum to -- the reason uncorrelated
#' noise adds in POWER, not in amplitude.
#'
#' @param a See Usage.
#' @param b See Usage.
#' @return A list with \code{lhs}, \code{rhs}, \code{holds}, \code{equality}, \code{norm_sum}, \code{norm_a}, \code{norm_b}, \code{method}.
#' @export
Triangle <- function(a, b) {
  # eq (4.45): |a+b| <= |a|+|b|, with equality when the vectors align.
  # It is the statement that no combination of two signals carries more
  # amplitude than their amplitudes sum to -- the reason uncorrelated
  # noise adds in POWER, not in amplitude.
  av <- as.numeric(a)
  bv <- as.numeric(b)
  if (length(av) != length(bv)) stop("a and b must have the same length")
  if (!length(av)) stop("need at least one component")
  ns <- sqrt(.morie_fsum((av + bv)^2))
  na <- sqrt(.morie_fsum(av * av))
  nb <- sqrt(.morie_fsum(bv * bv))
  list(
    lhs = ns, rhs = na + nb, holds = ns <= (na + nb) * (1 + 1e-12),
    equality = abs(ns - (na + nb)) <= 1e-9 * (na + nb + 1),
    norm_sum = ns, norm_a = na, norm_b = nb,
    method = "Rangayyan (2024) eq. (4.45)"
  )
}

#' Eq (4.48): H(f) = K X*(f) exp(-j 2 pi f t0).  The CONJUGATE is what
#'
#' cancels the signal\'s phase so every component arrives in step at t0
#' -- that coherent addition is the whole gain of the method.
#'
#' @param X See Usage.
#' @param freqs See Usage.
#' @param t0 See Usage.
#' @param gain Defaults to \code{1}.
#' @return A list with \code{H}, \code{freqs}, \code{t0}, \code{gain}, \code{magnitude}, \code{conjugate_of_signal}, \code{method}.
#' @export
MfTf <- function(X, freqs, t0, gain = 1) {
  # eq (4.48): H(f) = K X*(f) exp(-j 2 pi f t0).  The CONJUGATE is what
  # cancels the signal's phase so every component arrives in step at t0 --
  # that coherent addition is the whole gain of the method.
  Xs <- as.complex(X)
  fv <- as.numeric(freqs)
  if (length(Xs) != length(fv)) stop("X and freqs must have the same length")
  k <- as.numeric(gain)
  t <- as.numeric(t0)
  ang <- -2 * pi * fv * t
  H <- k * Conj(Xs) * complex(real = cos(ang), imaginary = sin(ang))
  list(
    H = H, freqs = fv, t0 = t, gain = k, magnitude = Mod(H),
    conjugate_of_signal = TRUE,
    method = "Rangayyan (2024) eqs. (4.48), (4.55)"
  )
}

#' Eqs (4.49), (4.56): h(t) = K x(t0 - t), reversed and delayed.  The
#'
#' delay must be at least the reference duration or the filter is not
#' causal; the book notes an N-point DFT of an N-sample template gives
#' only N-1, one sample short.
#'
#' @param x See Usage.
#' @param t0 Defaults to \code{NULL}.
#' @param gain Defaults to \code{1}.
#' @param dt Defaults to \code{1}.
#' @return A list with \code{h}, \code{t0}, \code{shift_samples}, \code{gain}, \code{causal}, \code{reversed}, \code{n_reference}, \code{method}.
#' @export
MfImpulse <- function(x, t0 = NULL, gain = 1, dt = 1) {
  # eqs (4.49), (4.56): h(t) = K x(t0 - t), reversed and delayed.  The
  # delay must be at least the reference duration or the filter is not
  # causal; the book notes an N-point DFT of an N-sample template gives
  # only N-1, one sample short.
  xs <- as.numeric(x)
  n <- length(xs)
  if (n < 1L) stop("need at least one sample")
  step <- as.numeric(dt)
  if (step <= 0) stop("dt must be positive")
  shift <- if (is.null(t0)) n else as.integer(round(as.numeric(t0) / step))
  if (shift < n) {
    stop(sprintf(paste(
      "t0 must be at least the reference duration (%d",
      "samples) for a causal filter; the DFT of eq. (4.55)",
      "at length N supplies only N-1"
    ), n))
  }
  h <- numeric(shift + 1L)
  j <- shift - (seq_len(n) - 1L)
  h[j + 1L] <- as.numeric(gain) * xs
  list(
    h = h, t0 = shift * step, shift_samples = shift,
    gain = as.numeric(gain), causal = TRUE, reversed = TRUE,
    n_reference = n, method = "Rangayyan (2024) eqs. (4.49), (4.56)"
  )
}

#' Filtering with h(t) = K x(t0 - t) makes the convolution equivalent to
#'
#' CORRELATION, so y(t) = K phi_x(t - t0): the output is a delayed copy
#' of the reference\'s ACF and peaks at t0 with K times its energy.
#'
#' @param x See Usage.
#' @param gain Defaults to \code{1}.
#' @param dt Defaults to \code{1}.
#' @return A list with \code{y}, \code{acf}, \code{lags}, \code{peak_index}, \code{peak_value}, \code{expected_peak}, \code{energy}, \code{max_difference}, \code{equals_acf}, \code{method}.
#' @export
MfAcf <- function(x, gain = 1, dt = 1) {
  # Filtering with h(t) = K x(t0 - t) makes the convolution equivalent to
  # CORRELATION, so y(t) = K phi_x(t - t0): the output is a delayed copy
  # of the reference's ACF and peaks at t0 with K times its energy.
  xs <- as.numeric(x)
  n <- length(xs)
  if (n < 2L) stop("need at least two samples")
  h <- MfImpulse(xs, gain = gain, dt = dt)$h
  y <- .morie_rg_conv(xs, h)
  xc <- .morie_rg_xcorr(xs, xs)
  shifted <- as.numeric(gain) * xc$values
  peak <- which.max(y)
  energy <- .morie_fsum(xs * xs)
  offset <- (peak - 1L) - (n - 1L)
  aligned <- if (offset >= 0L && offset + length(shifted) <= length(y)) {
    y[(offset + 1L):(offset + length(shifted))]
  } else {
    numeric(0)
  }
  gap <- if (length(aligned) == length(shifted)) {
    max(abs(aligned - shifted))
  } else {
    Inf
  }
  list(
    y = y, acf = xc$values, lags = xc$lags, peak_index = peak - 1L,
    peak_value = y[peak], expected_peak = as.numeric(gain) * energy,
    energy = energy, max_difference = gap,
    equals_acf = gap <= 1e-9 * (1 + abs(as.numeric(gain) * energy)),
    method = "Rangayyan (2024) Section 4.6.1, y(t) = K phi_x(t - t0)"
  )
}

#' Eqs (4.53)-(4.54): g(n) = 3 d(n) + 2 d(n-1) + d(n-2) and its filter
#'
#' h(n) = d(n) + 2 d(n-1) + 3 d(n-2) -- the same three numbers reversed.
#' The book notes the output around the peak reproduces the ACF of g,
#' which is checked here rather than asserted.
#'
#' @param amplitudes Defaults to \code{NULL}.
#' @return A list with \code{g}, \code{h}, \code{y}, \code{acf}, \code{delay}, \code{max_difference}, \code{output_is_acf}, \code{method}.
#' @export
RefPattern <- function(amplitudes = NULL) {
  # eqs (4.53)-(4.54): g(n) = 3 d(n) + 2 d(n-1) + d(n-2) and its filter
  # h(n) = d(n) + 2 d(n-1) + 3 d(n-2) -- the same three numbers reversed.
  # The book notes the output around the peak reproduces the ACF of g,
  # which is checked here rather than asserted.
  g <- if (is.null(amplitudes)) c(3, 2, 1) else as.numeric(amplitudes)
  if (!length(g)) stop("need at least one sample")
  h <- rev(g)
  y <- .morie_rg_conv(g, h)
  acf <- .morie_rg_xcorr(g, g)$values
  gap <- max(abs(y - acf))
  list(
    g = g, h = h, y = y, acf = acf, delay = length(g) - 1L,
    max_difference = gap, output_is_acf = gap <= 1e-12,
    method = "Rangayyan (2024) eqs. (4.53)-(4.54)"
  )
}

#' Eq (4.55), the same expression as eq (4.48) restated in Section
#' 4.6.2,
#'
#' so it delegates.  What is specific here is the DFT caveat.
#'
#' @param X See Usage.
#' @param freqs See Usage.
#' @param t0 See Usage.
#' @param gain Defaults to \code{1}.
#' @return The value of \code{r}, as built in the body.
#' @export
MfTfEeg <- function(X, freqs, t0, gain = 1) {
  # eq (4.55), the same expression as eq (4.48) restated in Section 4.6.2,
  # so it delegates.  What is specific here is the DFT caveat.
  r <- MfTf(X, freqs, t0, gain = gain)
  r$dft_shift_caveat <- paste(
    "an N-point DFT of an N-sample template",
    "supplies a shift of N-1; the causal filter",
    "needs N"
  )
  r$method <- "Rangayyan (2024) eq. (4.55)"
  r
}

#' Eq (4.56), identical to eq (4.49).  Section 4.6.2 adds that because h
#'
#' is a reversed reference, the filtering is equivalent to correlation.
#'
#' @param x See Usage.
#' @param t0 Defaults to \code{NULL}.
#' @param gain Defaults to \code{1}.
#' @param dt Defaults to \code{1}.
#' @return The value of \code{r}, as built in the body.
#' @export
MfImpEeg <- function(x, t0 = NULL, gain = 1, dt = 1) {
  # eq (4.56), identical to eq (4.49).  Section 4.6.2 adds that because h
  # is a reversed reference, the filtering is equivalent to correlation.
  r <- MfImpulse(x, t0 = t0, gain = gain, dt = dt)
  r$equivalent_to_correlation <- TRUE
  r$method <- "Rangayyan (2024) eq. (4.56)"
  r
}

#' Eq (4.57): Y(f) = X X* = S_x(f).  Because Y is a PSD it is real and
#'
#' nonnegative -- the phase has been cancelled exactly, the
#' frequency-domain statement of the coherent addition at t0.
#'
#' @param x See Usage.
#' @param dt Defaults to \code{1}.
#' @return A list with \code{Y}, \code{psd}, \code{max_imaginary}, \code{max_difference}, \code{is_psd}, \code{n}, \code{method}.
#' @export
MfPsd <- function(x, dt = 1) {
  # eq (4.57): Y(f) = X X* = S_x(f).  Because Y is a PSD it is real and
  # nonnegative -- the phase has been cancelled exactly, the
  # frequency-domain statement of the coherent addition at t0.
  xs <- as.numeric(x)
  n <- length(xs)
  if (n < 2L) stop("need at least two samples")
  f <- .morie_rg_dft(xs)
  X <- complex(real = f$re, imaginary = f$im)
  Y <- X * Conj(X)
  psd <- Mod(X)^2
  gap <- max(abs(Re(Y) - psd))
  list(
    Y = Y, psd = psd, max_imaginary = max(abs(Im(Y))),
    max_difference = gap, is_psd = gap <= 1e-9 * (1 + max(psd)), n = n,
    method = "Rangayyan (2024) eq. (4.57)"
  )
}

#' Eq (4.46) rearranged: M_y^2 / P_eta_o = 2 E_x / P_eta_i, the familiar
#'
#' 2E/N0.  It depends on the signal ONLY through its energy -- the same
#' detectability from a short loud transient as from a long quiet one of
#' equal energy.  That is the substantive content of the theorem.
#'
#' @param x See Usage.
#' @param noise_power See Usage.
#' @param t Defaults to \code{NULL}.
#' @param dt Defaults to \code{1}.
#' @return A list with \code{snr}, \code{snr_db}, \code{energy}, \code{noise_power}, \code{n0}, \code{depends_only_on_energy}, \code{method}.
#' @export
MfMaxSnr <- function(x, noise_power, t = NULL, dt = 1) {
  # eq (4.46) rearranged: M_y^2 / P_eta_o = 2 E_x / P_eta_i, the familiar
  # 2E/N0.  It depends on the signal ONLY through its energy -- the same
  # detectability from a short loud transient as from a long quiet one of
  # equal energy.  That is the substantive content of the theorem.
  xs <- as.numeric(x)
  if (length(xs) < 2L) stop("need at least two samples")
  p <- as.numeric(noise_power)
  if (p <= 0) stop("the input noise power must be positive")
  ts <- if (is.null(t)) (seq_along(xs) - 1) * as.numeric(dt) else as.numeric(t)
  energy <- .morie_rg_gridint(xs * xs, ts)
  snr <- 2 * energy / p
  list(
    snr = snr, snr_db = if (snr > 0) 10 * log10(snr) else -Inf,
    energy = energy, noise_power = p, n0 = p,
    depends_only_on_energy = TRUE,
    method = "Rangayyan (2024) eq. (4.46)"
  )
}

#' MatchedFilt
#'
#' Part of the rangayyan_corr implementation; see the file header for
#' the source it follows.
#'
#' @param ref See Usage.
#' @param x Defaults to \code{NULL}.
#' @param noise_psd Defaults to \code{NULL}.
#' @param freqs Defaults to \code{NULL}.
#' @param t0 Defaults to \code{NULL}.
#' @param gain Defaults to \code{1}.
#' @param dt Defaults to \code{1}.
#' @return The value of \code{out}, as built in the body.
#' @export
MatchedFilt <- function(ref, x = NULL, noise_psd = NULL, freqs = NULL,
                        t0 = NULL, gain = 1, dt = 1) {
  # eqs (4.48)-(4.49) for the white-noise case.  The derivation assumed
  # white noise (eq 4.35); for COLOURED noise the same Schwarz argument
  # gives H = K X* exp(-j2pi f t0) / P_nn(f), which whitens first.
  # `whitened` says which was used -- applying the white-noise filter to
  # coloured noise is a real loss of detectability.
  rs <- as.numeric(ref)
  n <- length(rs)
  if (n < 2L) stop("the reference needs at least two samples")
  step <- as.numeric(dt)
  f <- .morie_rg_dft(rs)
  X <- complex(real = f$re, imaginary = f$im)
  shift <- if (is.null(t0)) n else as.integer(round(as.numeric(t0) / step))
  k <- seq_len(n) - 1L
  ang <- -2 * pi * k * shift / n
  H <- as.numeric(gain) * Conj(X) *
    complex(real = cos(ang), imaginary = sin(ang))
  whitened <- FALSE
  if (!is.null(noise_psd)) {
    pn <- as.numeric(noise_psd)
    if (length(pn) != n) {
      stop(sprintf("noise_psd needs one value per DFT bin (%d)", n))
    }
    if (any(pn <= 0)) stop("the noise PSD must be positive everywhere")
    H <- H / pn
    whitened <- TRUE
  }
  h <- if (!whitened) {
    MfImpulse(rs, t0 = shift * step, gain = gain, dt = step)$h
  } else {
    NULL
  }
  out <- list(
    H = H, h = h, shift_samples = shift, gain = as.numeric(gain),
    whitened = whitened, n_reference = n,
    freqs = if (is.null(freqs)) k / (n * step) else as.numeric(freqs),
    method = paste(
      "Rangayyan (2024) eqs. (4.48)-(4.49); the",
      "noise_psd branch is the coloured-noise",
      "generalization"
    )
  )
  if (!is.null(x)) {
    xs <- as.numeric(x)
    taps <- if (!is.null(h)) h else .morie_rg_idft_re(Re(H), Im(H))
    y <- .morie_rg_conv(xs, taps)
    out$y <- y
    out$peak_index <- which.max(y) - 1L
  }
  out
}

# ---------------------------------------------------- spectral quantities

#' Eq (3.81): the 1/N and the PLUS sign are what distinguish it from the
#'
#' forward transform of eq (3.80); getting either wrong scales the
#' signal by N or reverses it in time, neither visible on a magnitude
#' plot.
#'
#' @param X See Usage.
#' @return A list with \code{x}, \code{complex}, \code{n}, \code{max_imaginary}, \code{method}.
#' @export
Idft <- function(X) {
  # eq (3.81): the 1/N and the PLUS sign are what distinguish it from the
  # forward transform of eq (3.80); getting either wrong scales the
  # signal by N or reverses it in time, neither visible on a magnitude plot.
  Xs <- as.complex(X)
  n <- length(Xs)
  if (!n) stop("need at least one coefficient")
  idx <- seq_len(n) - 1L
  out <- vapply(idx, function(i) {
    ang <- 2 * pi * i * idx / n
    sum(Xs * complex(real = cos(ang), imaginary = sin(ang))) / n
  }, complex(1))
  list(
    x = Re(out), complex = out, n = n, max_imaginary = max(abs(Im(out))),
    method = "Rangayyan (2024) eq. (3.81)"
  )
}

#' Eq (3.91), discrete form: sum |x(n)|^2 = (1/N) sum |X(k)|^2.  With
#' the
#'
#' unnormalized forward transform of eq (3.80) the spectral sum is N
#' times the time-domain energy, so omitting the 1/N inflates it by the
#' record length.
#'
#' @param x See Usage.
#' @return A list with \code{energy_time}, \code{energy_freq}, \code{psd}, \code{max_difference}, \code{holds}, \code{n}, \code{method}.
#' @export
Parseval <- function(x) {
  # eq (3.91), discrete form: sum |x(n)|^2 = (1/N) sum |X(k)|^2.  With the
  # unnormalized forward transform of eq (3.80) the spectral sum is N
  # times the time-domain energy, so omitting the 1/N inflates it by the
  # record length.
  xs <- as.numeric(x)
  n <- length(xs)
  if (n < 1L) stop("need at least one sample")
  f <- .morie_rg_dft(xs)
  te <- .morie_fsum(xs * xs)
  fe <- .morie_fsum(f$re^2 + f$im^2) / n
  gap <- abs(te - fe)
  list(
    energy_time = te, energy_freq = fe, psd = (f$re^2 + f$im^2) / n,
    max_difference = gap, holds = gap <= 1e-9 * max(te, 1), n = n,
    method = "Rangayyan (2024) eq. (3.91)"
  )
}

#' Eq (3.96): the sum separates into a signal sum growing linearly in M
#'
#' and a zero-mean noise sum growing only as sqrt(M).  The SUM is
#' returned, because that is the equation; the average is beside it so
#' the division by M is explicit.
#'
#' @param observations See Usage.
#' @return A list with \code{sum}, \code{average}, \code{m}, \code{n}, \code{signal_growth}, \code{noise_growth}, \code{method}.
#' @export
SyncSum <- function(observations) {
  # eq (3.96): the sum separates into a signal sum growing linearly in M
  # and a zero-mean noise sum growing only as sqrt(M).  The SUM is
  # returned, because that is the equation; the average is beside it so
  # the division by M is explicit.
  recs <- lapply(observations, as.numeric)
  m <- length(recs)
  if (!m) stop("need at least one observation")
  n <- length(recs[[1]])
  if (!n) stop("records must be nonempty")
  if (any(vapply(recs, length, integer(1)) != n)) {
    stop("all realizations must have the same length")
  }
  mat <- matrix(unlist(recs), nrow = m, byrow = TRUE)
  total <- vapply(seq_len(n), function(i) .morie_fsum(mat[, i]), numeric(1))
  list(
    sum = total, average = total / m, m = m, n = n,
    signal_growth = "linear in M", noise_growth = "sqrt(M)",
    method = "Rangayyan (2024) eq. (3.96)"
  )
}

#' Eqs (6.32)-(6.43).  The sums run over ONE HALF of the periodic PSD:
#'
#' for a real signal S(k) is even-symmetric about fs/2, so summing the
#' whole period double-counts every frequency and pins the mean at fs/2
#' regardless of the signal.
#'
#' @param psd See Usage.
#' @param fs Defaults to \code{1}.
#' @param freqs Defaults to \code{NULL}.
#' @return A list with \code{total_power}, \code{mean_frequency}, \code{median_frequency}, \code{variance}, \code{bandwidth}, \code{skewness}, \code{kurtosis}, \code{fm3}, \code{fm4}, \code{n_bins}, \code{uniformity}, \code{method}.
#' @export
SpecMoments <- function(psd, fs = 1, freqs = NULL) {
  # eqs (6.32)-(6.43).  The sums run over ONE HALF of the periodic PSD:
  # for a real signal S(k) is even-symmetric about fs/2, so summing the
  # whole period double-counts every frequency and pins the mean at fs/2
  # regardless of the signal.
  p <- as.numeric(psd)
  if (!length(p)) stop("need at least one bin")
  if (any(p < 0)) stop("a PSD cannot be negative")
  nh <- length(p)
  f <- if (is.null(freqs)) {
    n <- if (nh > 1L) 2L * (nh - 1L) else 1L
    (seq_len(nh) - 1L) * as.numeric(fs) / n
  } else {
    as.numeric(freqs)
  }
  if (length(f) != nh) stop("psd and freqs must have the same length")
  ep <- .morie_fsum(p)
  if (ep <= 0) stop("the PSD has zero total power")
  fmean <- .morie_fsum(f * p) / ep
  cum <- cumsum(p)
  idx <- which(cum >= 0.5 * ep)[1]
  fmed <- if (is.na(idx)) f[nh] else f[idx]
  fm2 <- .morie_fsum((f - fmean)^2 * p) / ep
  fm3 <- .morie_fsum((f - fmean)^3 * p) / ep
  fm4 <- .morie_fsum((f - fmean)^4 * p) / ep
  mx <- max(p)
  list(
    total_power = ep, mean_frequency = fmean, median_frequency = fmed,
    variance = fm2, bandwidth = if (fm2 > 0) sqrt(fm2) else 0,
    skewness = if (fm2 > 0) fm3 / fm2^1.5 else NULL,
    kurtosis = if (fm2 > 0) fm4 / (fm2 * fm2) else NULL,
    fm3 = fm3, fm4 = fm4, n_bins = nh,
    uniformity = if (mx > 0) (ep / nh) / mx else 0,
    method = "Rangayyan (2024) eqs. (6.32)-(6.43)"
  )
}

#' Eqs (6.34)-(6.35) on the EMG periodogram.  Both fall as a muscle
#'
#' fatigues, but they are not interchangeable: the median is far less
#' sensitive to the high-frequency tail, where an EMG record carries
#' mostly instrumentation noise, so it is the more stable fatigue index
#' and the two diverge exactly when the tail is contaminated.
#'
#' @param x See Usage.
#' @param fs See Usage.
#' @param nperseg Defaults to \code{NULL}.
#' @return A list with \code{mean_frequency}, \code{median_frequency}, \code{difference}, \code{bandwidth}, \code{total_power}, \code{psd}, \code{freqs}, \code{fs}, \code{nperseg}, \code{method}.
#' @export
EmgFreq <- function(x, fs, nperseg = NULL) {
  # eqs (6.34)-(6.35) on the EMG periodogram.  Both fall as a muscle
  # fatigues, but they are not interchangeable: the median is far less
  # sensitive to the high-frequency tail, where an EMG record carries
  # mostly instrumentation noise, so it is the more stable fatigue index
  # and the two diverge exactly when the tail is contaminated.
  xs <- as.numeric(x)
  fsv <- as.numeric(fs)
  if (fsv <= 0) stop("fs must be positive")
  if (length(xs) < 8L) stop("need at least eight samples")
  m <- if (is.null(nperseg)) length(xs) else min(as.integer(nperseg), length(xs))
  pg <- .morie_rg_periodogram(xs[seq_len(m)], fsv)
  mom <- SpecMoments(pg$psd, freqs = pg$freqs)
  list(
    mean_frequency = mom$mean_frequency,
    median_frequency = mom$median_frequency,
    difference = mom$mean_frequency - mom$median_frequency,
    bandwidth = mom$bandwidth, total_power = mom$total_power,
    psd = pg$psd, freqs = pg$freqs, fs = fsv, nperseg = m,
    method = "Rangayyan (2024) eqs. (6.34)-(6.35)"
  )
}

#' Rayleigh: delta_f = fs/N, set by the RECORD LENGTH.  Zero-padding
#'
#' interpolates the same spectrum more finely and does NOT improve
#' resolution -- the commonest misreading of a smoother-looking plot.
#' Every window that suppresses leakage widens the main lobe, so a
#' window buys dynamic range with resolution.
#'
#' @param n See Usage.
#' @param fs Defaults to \code{1}.
#' @param window Defaults to \code{"rectangular"}.
#' @return A list with \code{delta_f}, \code{resolution}, \code{main_lobe_bins}, \code{sidelobe_db}, \code{equivalent_noise_bandwidth_bins}, \code{duration}, \code{n}, \code{fs}, \code{window}, \code{zero_padding_helps}, \code{method}.
#' @export
SpecRes <- function(n, fs = 1, window = "rectangular") {
  # Rayleigh: delta_f = fs/N, set by the RECORD LENGTH.  Zero-padding
  # interpolates the same spectrum more finely and does NOT improve
  # resolution -- the commonest misreading of a smoother-looking plot.
  # Every window that suppresses leakage widens the main lobe, so a
  # window buys dynamic range with resolution.
  nn <- as.integer(n)
  if (nn < 2L) stop("need at least two samples")
  fsv <- as.numeric(fs)
  if (fsv <= 0) stop("fs must be positive")
  tab <- list(
    rectangular = c(2, -13.3, 1), hann = c(4, -31.5, 1.5),
    hamming = c(4, -42.7, 1.36), blackman = c(6, -58.1, 1.73)
  )
  key <- tolower(window)
  if (is.null(tab[[key]])) {
    stop(sprintf(
      "unknown window '%s'; known: %s", window,
      paste(sort(names(tab)), collapse = ", ")
    ))
  }
  v <- tab[[key]]
  df <- fsv / nn
  list(
    delta_f = df, resolution = df * v[1] / 2, main_lobe_bins = v[1],
    sidelobe_db = v[2], equivalent_noise_bandwidth_bins = v[3],
    duration = nn / fsv, n = nn, fs = fsv, window = key,
    zero_padding_helps = FALSE,
    method = paste(
      "Rayleigh criterion; Rangayyan (2024) Section 6.4 on",
      "windowing and leakage"
    )
  )
}

#' Section 6.4: bin k sits at k fs / N, and a band power is the sum of
#'
#' its bins times the bin width fs/N.  The bin width turns a density
#' into a power; omitting it leaves a quantity that changes with the
#' record length, so two band powers at different N are not comparable.
#'
#' @param psd See Usage.
#' @param fs See Usage.
#' @param n Defaults to \code{NULL}.
#' @param bands Defaults to \code{NULL}.
#' @return The value of \code{out}, as built in the body.
#' @export
PsdHz <- function(psd, fs, n = NULL, bands = NULL) {
  # Section 6.4: bin k sits at k fs / N, and a band power is the sum of
  # its bins times the bin width fs/N.  The bin width turns a density
  # into a power; omitting it leaves a quantity that changes with the
  # record length, so two band powers at different N are not comparable.
  p <- as.numeric(psd)
  if (!length(p)) stop("need at least one bin")
  fsv <- as.numeric(fs)
  if (fsv <= 0) stop("fs must be positive")
  nn <- if (is.null(n)) 2L * (length(p) - 1L) else as.integer(n)
  if (nn < 2L) stop("the DFT length must be at least 2")
  width <- fsv / nn
  freqs <- (seq_len(length(p)) - 1L) * width
  out <- list(
    freqs = freqs, bin_width = width, psd = p, n = nn, fs = fsv,
    total_power = .morie_fsum(p) * width, nyquist = fsv / 2,
    method = "Rangayyan (2024) Section 6.4"
  )
  if (!is.null(bands)) {
    powers <- lapply(bands, function(b) {
      if (b[2] <= b[1]) stop("a band has hi <= lo")
      .morie_fsum(p[freqs >= b[1] & freqs < b[2]]) * width
    })
    tot <- sum(unlist(powers))
    out$band_power <- powers
    out$band_fraction <- lapply(powers, function(v) {
      if (tot > 0) v / tot else 0
    })
  }
  out
}

#' S_avg(f) = (1/M) sum |PCG_k(f)|^2.  The averaging is of POWER
#'
#' SPECTRA, not waveforms, and that is the point: successive PCG cycles
#' are not sample-aligned -- a murmur is turbulent, its phase differs
#' cycle to cycle -- so waveform averaging as in eq (3.96) would cancel
#' exactly the murmur being measured.  Both are computed so the
#' difference is visible.
#'
#' @param cycles See Usage.
#' @param fs Defaults to \code{1}.
#' @return A list with \code{average_psd}, \code{psd_of_average}, \code{mean_waveform}, \code{freqs}, \code{m}, \code{n}, \code{power_retained}, \code{method}.
#' @export
PcgSyncAvg <- function(cycles, fs = 1) {
  # S_avg(f) = (1/M) sum |PCG_k(f)|^2.  The averaging is of POWER
  # SPECTRA, not waveforms, and that is the point: successive PCG cycles
  # are not sample-aligned -- a murmur is turbulent, its phase differs
  # cycle to cycle -- so waveform averaging as in eq (3.96) would cancel
  # exactly the murmur being measured.  Both are computed so the
  # difference is visible.
  recs <- lapply(cycles, as.numeric)
  m <- length(recs)
  if (!m) stop("need at least one cycle")
  n <- min(vapply(recs, length, integer(1)))
  if (n < 4L) stop("cycles need at least four samples")
  recs <- lapply(recs, function(r) r[seq_len(n)])
  half <- n %/% 2L + 1L
  acc <- numeric(half)
  for (r in recs) {
    f <- .morie_rg_dft(r - mean(r))
    acc <- acc + (f$re[seq_len(half)]^2 + f$im[seq_len(half)]^2) / n
  }
  avg_psd <- acc / m
  mat <- matrix(unlist(recs), nrow = m, byrow = TRUE)
  mean_wave <- vapply(
    seq_len(n), function(i) .morie_fsum(mat[, i]) / m,
    numeric(1)
  )
  fm <- .morie_rg_dft(mean_wave - mean(mean_wave))
  psd_of_avg <- (fm$re[seq_len(half)]^2 + fm$im[seq_len(half)]^2) / n
  list(
    average_psd = avg_psd, psd_of_average = psd_of_avg,
    mean_waveform = mean_wave,
    freqs = (seq_len(half) - 1L) * as.numeric(fs) / n, m = m, n = n,
    power_retained = if (sum(avg_psd) > 0) {
      sum(psd_of_avg) / sum(avg_psd)
    } else {
      0
    },
    method = paste(
      "spectral synchronized averaging; contrast the",
      "waveform averaging of Rangayyan (2024) eq. (3.96)"
    )
  )
}

#' Section 3.5: averaging M aligned epochs raises the SNR by sqrt(M).  A
#'
#' large artifact is NOT zero-mean over the epochs it contaminates, so
#' it does not average away at any M; the remedy is to reject those
#' epochs first.  Averaging without rejection and quoting the sqrt(M)
#' gain overstates the result by exactly the artifact that survived.
#'
#' @param epochs See Usage.
#' @param reject Defaults to \code{NULL}.
#' @return A list with \code{average}, \code{sd}, \code{m}, \code{m_kept}, \code{rejected}, \code{n_rejected}, \code{peaks}, \code{n}, \code{snr_gain}, \code{snr_gain_db}, \code{artifact_factor}, \code{method}.
#' @export
ErpArtifact <- function(epochs, reject = NULL) {
  # Section 3.5: averaging M aligned epochs raises the SNR by sqrt(M).  A
  # large artifact is NOT zero-mean over the epochs it contaminates, so
  # it does not average away at any M; the remedy is to reject those
  # epochs first.  Averaging without rejection and quoting the sqrt(M)
  # gain overstates the result by exactly the artifact that survived.
  recs <- lapply(epochs, as.numeric)
  m <- length(recs)
  if (!m) stop("need at least one epoch")
  n <- length(recs[[1]])
  if (!n) stop("epochs must be nonempty")
  if (any(vapply(recs, length, integer(1)) != n)) {
    stop("all epochs must have the same length")
  }
  peaks <- vapply(recs, function(r) max(abs(r)), numeric(1))
  kept <- seq_len(m)
  dropped <- integer(0)
  if (!is.null(reject)) {
    thr <- as.numeric(reject)
    if (thr <= 0) stop("the rejection threshold must be positive")
    kept <- which(peaks <= thr)
    dropped <- which(peaks > thr) - 1L
    if (!length(kept)) stop("every epoch exceeds the rejection threshold")
  }
  k <- length(kept)
  mat <- matrix(unlist(recs[kept]), nrow = k, byrow = TRUE)
  avg <- vapply(seq_len(n), function(i) .morie_fsum(mat[, i]) / k, numeric(1))
  sd <- vapply(
    seq_len(n),
    function(i) sqrt(.morie_fsum((mat[, i] - avg[i])^2) / k),
    numeric(1)
  )
  list(
    average = avg, sd = sd, m = m, m_kept = k, rejected = dropped,
    n_rejected = length(dropped), peaks = peaks, n = n,
    snr_gain = sqrt(k), snr_gain_db = 10 * log10(k),
    artifact_factor = 1 / sqrt(k),
    method = "Rangayyan (2024) Section 3.5 (synchronized averaging)"
  )
}

#' SeizCohere
#'
#' Part of the rangayyan_corr implementation; see the file header for
#' the source it follows.
#'
#' @param channels See Usage.
#' @param fs See Usage.
#' @param window See Usage.
#' @param step Defaults to \code{NULL}.
#' @param bands Defaults to \code{NULL}.
#' @param nperseg Defaults to \code{NULL}.
#' @return A list with \code{times}, \code{coherence}, \code{bands}, \code{window}, \code{step}, \code{nperseg}, \code{n_windows}, \code{n_channels}, \code{sustained_criterion}, \code{method}.
#' @export
SeizCohere <- function(channels, fs, window, step = NULL, bands = NULL,
                       nperseg = NULL) {
  # Section 4.5.3: the coherence of eq (4.32) detects rhythms in COMMON
  # between channels, so a seizure shows as sustained widespread rhythm.
  # Two properties are kept explicit: each window must hold several
  # segments (eq 4.32's averaging caveat), and the criterion is a RUN of
  # elevated windows, so run lengths are returned, not a per-window verdict.
  chans <- lapply(channels, as.numeric)
  if (length(chans) < 2L) {
    stop("need at least two channels to form a coherence")
  }
  n <- min(vapply(chans, length, integer(1)))
  w <- as.integer(window)
  if (w > n) stop("the window is longer than the record")
  hop <- if (is.null(step)) w %/% 2L else as.integer(step)
  if (hop < 1L) stop("step must be at least one sample")
  fsv <- as.numeric(fs)
  if (fsv <= 0) stop("fs must be positive")
  if (is.null(bands)) {
    bands <- list(
      delta = c(0.5, 4), theta = c(4, 8), alpha = c(8, 13),
      beta = c(13, 30)
    )
  }
  seg <- if (is.null(nperseg)) max(8L, w %/% 8L) else as.integer(nperseg)
  starts <- seq(1L, n - w + 1L, by = hop)
  times <- (starts - 1L) / fsv
  curves <- setNames(vector("list", length(bands)), names(bands))
  for (nm in names(bands)) curves[[nm]] <- numeric(length(starts))
  for (si in seq_along(starts)) {
    s <- starts[si]
    acc <- setNames(vector("list", length(bands)), names(bands))
    for (i in seq_along(chans)) {
      for (j in seq_along(chans)) {
        if (j <= i) next
        c0 <- Cohere(chans[[i]][s:(s + w - 1L)], chans[[j]][s:(s + w - 1L)],
          fs = fsv, nperseg = seg
        )
        for (nm in names(bands)) {
          b <- bands[[nm]]
          v <- c0$coherence[c0$freqs >= b[1] & c0$freqs < b[2]]
          if (length(v)) acc[[nm]] <- c(acc[[nm]], mean(v))
        }
      }
    }
    for (nm in names(bands)) {
      curves[[nm]][si] <- if (length(acc[[nm]])) mean(acc[[nm]]) else 0
    }
  }
  list(
    times = times, coherence = curves, bands = bands, window = w,
    step = hop, nperseg = seg, n_windows = length(starts),
    n_channels = length(chans),
    sustained_criterion = paste(
      "a seizure is a RUN of elevated windows;",
      "one high window is a transient"
    ),
    method = "Rangayyan (2024) Section 4.5.3, eq. (4.32)"
  )
}

#' CardioResp
#'
#' Part of the rangayyan_corr implementation; see the file header for
#' the source it follows.
#'
#' @param ecg_rate See Usage.
#' @param resp See Usage.
#' @param fs See Usage.
#' @param band Defaults to \code{c(0.15, 0.4)}.
#' @param nperseg Defaults to \code{NULL}.
#' @return A list with \code{plv}, \code{mean_phase_difference}, \code{phase_difference}, \code{coherence_peak}, \code{coherence_mean}, \code{coherence}, \code{freqs}, \code{band}, \code{n}, \code{fs}, \code{method}.
#' @export
CardioResp <- function(ecg_rate, resp, fs, band = c(0.15, 0.40),
                       nperseg = NULL) {
  # Coherence (eq 4.32) measures LINEAR association; the phase-locking
  # value measures whether the phase DIFFERENCE is constant regardless of
  # amplitude.  They answer different questions and can disagree --
  # respiratory sinus arrhythmia is a frequency modulation, so PLV sees
  # it where coherence may not.  Reporting only one is how a nonlinear
  # coupling gets missed.
  a <- as.numeric(ecg_rate)
  b <- as.numeric(resp)
  if (length(a) != length(b)) {
    stop("the two signals must have the same length")
  }
  n <- length(a)
  if (n < 16L) stop("need at least sixteen samples")
  fsv <- as.numeric(fs)
  if (fsv <= 0) stop("fs must be positive")
  lo <- band[1]
  hi <- band[2]
  if (!(lo >= 0 && lo < hi && hi <= fsv / 2)) {
    stop("the band must satisfy 0 <= lo < hi <= fs/2")
  }
  analytic <- function(sig) {
    f <- .morie_rg_dft(sig - mean(sig))
    k <- seq_len(n) - 1L
    fk <- ifelse(k <= n %/% 2L, k * fsv / n, (k - n) * fsv / n)
    take <- fk >= 0 & fk >= lo & fk < hi
    scale <- ifelse(k == 0L | k == n %/% 2L, 1, 2)
    re <- ifelse(take, scale * f$re, 0)
    im <- ifelse(take, scale * f$im, 0)
    idx <- seq_len(n) - 1L
    xr <- vapply(idx, function(i) {
      ang <- 2 * pi * i * idx / n
      .morie_fsum(re * cos(ang) - im * sin(ang)) / n
    }, numeric(1))
    xi <- vapply(idx, function(i) {
      ang <- 2 * pi * i * idx / n
      .morie_fsum(re * sin(ang) + im * cos(ang)) / n
    }, numeric(1))
    complex(real = xr, imaginary = xi)
  }
  za <- analytic(a)
  zb <- analytic(b)
  dphi <- Arg(za) - Arg(zb)
  cre <- .morie_fsum(cos(dphi)) / n
  cim <- .morie_fsum(sin(dphi)) / n
  plv <- sqrt(cre^2 + cim^2)
  coh <- Cohere(a, b, fs = fsv, nperseg = nperseg)
  inband <- coh$coherence[coh$freqs >= lo & coh$freqs < hi]
  list(
    plv = plv, mean_phase_difference = Arg(complex(
      real = cre,
      imaginary = cim
    )),
    phase_difference = dphi,
    coherence_peak = if (length(inband)) max(inband) else 0,
    coherence_mean = if (length(inband)) mean(inband) else 0,
    coherence = coh$coherence, freqs = coh$freqs, band = c(lo, hi),
    n = n, fs = fsv,
    method = paste(
      "coherence per Rangayyan (2024) eq. (4.32);",
      "PLV per Lachaux et al. (1999)"
    )
  )
}

# pre-policy spellings
morie_ch4_dot_product <- DotProd
morie_ch4_continuous_dot_product <- ContProj
morie_ch4_ccf_outer_product <- CcfOuter
morie_ch4_csd_from_ccf <- Csd
morie_ch4_coherence_spectrum <- Cohere
morie_coherence_cxy <- Msc
morie_template_match <- Template
morie_ch4_matched_filter_input_ft <- MfInput
morie_ch4_matched_filter_output <- MfOutput
morie_ch4_white_noise_psd_input <- MfNoiseIn
morie_ch4_noise_psd_at_output <- MfNoiseOut
morie_ch4_matched_filter_instantaneous <- MfPeak
morie_ch4_peak_power_snr <- MfSnr
morie_ch4_signal_total_energy <- SigEnergy
morie_ch4_snr_normalized_ratio <- MfRatio
morie_ch4_schwarz_complex <- SchwarzC
morie_ch4_schwarz_real <- SchwarzR
morie_ch4_cauchy_schwarz <- CauchySch
morie_ch4_triangle_inequality <- Triangle
morie_ch4_matched_filter_transfer <- MfTf
morie_ch4_matched_filter_impulse <- MfImpulse
morie_ch4_matched_filter_output_acf <- MfAcf
morie_ch4_basic_signal_g <- RefPattern
morie_ch4_matched_filter_H_eeg <- MfTfEeg
morie_ch4_matched_filter_impulse_eeg <- MfImpEeg
morie_ch4_matched_filter_output_psd <- MfPsd
morie_matched_filter <- MatchedFilt
morie_matched_filter_snr <- MfMaxSnr
morie_ch3_idft <- Idft
morie_ch3_parseval <- Parseval
morie_ch3_synchronized_sum <- SyncSum
morie_spectral_moments <- SpecMoments
morie_emg_peak_freq <- EmgFreq
morie_spectral_resolution <- SpecRes
morie_psd_to_hz <- PsdHz
morie_pcg_sync_avg <- PcgSyncAvg
morie_erp_artifact_remove <- ErpArtifact
morie_seizure_detect <- SeizCohere
morie_coupled_freq_select <- CardioResp

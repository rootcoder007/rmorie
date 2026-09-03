# Rangayyan signal-level features -- RMS, Hjorth complexity, Willison
# turns count, SNR, synchronized averaging, fractal dimension, spectral
# entropy.  Mirror of the Python bsastat module.
#
# One placeholder formula was flatly wrong: the form factor.  The stub
# said RMS(x)/mean(|x|); eq (5.26) defines it as Hjorth's complexity,
# the ratio of the mobility of the first derivative to the mobility of
# the signal.  For a sinusoid the book's value is 1; the stub's is
# pi/(2 sqrt 2) = 1.111.

#' Eq (3.9): RMS = sqrt((1/N) sum x^2), divisor N.  With a window, the
#'
#' short-time RMS the book uses for EMG activity (Section 5.6).
#'
#' @param x Coerced to numeric by the body, with \code{as.numeric}.
#' @param window Optional; may be \code{NULL}. Coerced to integer by the body, with
#' \code{as.integer}.
#' @return The value of \code{out}, as built in the body.
#' @export
Rms <- function(x, window = NULL) {
  # eq (3.9): RMS = sqrt((1/N) sum x^2), divisor N.  With a window, the
  # short-time RMS the book uses for EMG activity (Section 5.6).
  xs <- as.numeric(x)
  if (!length(xs)) stop("need at least one sample")
  ms <- .morie_fsum(xs * xs) / length(xs)
  out <- list(
    rms = sqrt(ms), ms = ms, n = length(xs),
    method = "Rangayyan (2024) eq. (3.9)"
  )
  if (!is.null(window)) {
    w <- as.integer(window)
    if (w < 1L) stop("window must be at least one sample")
    out$short_time <- vapply(seq_along(xs), function(i) {
      seg <- xs[max(1L, i - w + 1L):i]
      sqrt(.morie_fsum(seg * seg) / length(seg))
    }, numeric(1))
    out$window <- w
  }
  out
}

#' Eqs (5.25)-(5.26): activity = var(x); mobility = sd(x\')/sd(x);
#'
#' form factor = mobility(x\')/mobility(x) =
#' (sd(x\'\')/sd(x\'))/(sd(x\')/sd(x)). The book states a sinusoid has
#' complexity 1 and that more variable waveforms give larger values.
#' Ratios are dimensionless, so no fs.
#'
#' @param x Coerced to numeric by the body, with \code{as.numeric}.
#' @return A list with \code{form_factor}, \code{complexity}, \code{mobility},
#' \code{activity}, \code{mobility_of_derivative}, \code{n}, \code{method}.
#' @export
FormFactor <- function(x) {
  # eqs (5.25)-(5.26): activity = var(x); mobility = sd(x')/sd(x);
  # form factor = mobility(x')/mobility(x) = (sd(x'')/sd(x'))/(sd(x')/sd(x)).
  # The book states a sinusoid has complexity 1 and that more variable
  # waveforms give larger values.  Ratios are dimensionless, so no fs.
  xs <- as.numeric(x)
  if (length(xs) < 4L) {
    stop("need at least four samples for a second derivative")
  }
  pvar <- function(v) {
    mu <- .morie_fsum(v) / length(v)
    .morie_fsum((v - mu)^2) / length(v)
  }
  d1 <- diff(xs)
  d2 <- diff(d1)
  a0 <- pvar(xs)
  a1 <- pvar(d1)
  a2 <- pvar(d2)
  if (a0 <= 0) {
    stop("a constant signal has zero activity; mobility and form factor are undefined")
  }
  mob <- sqrt(a1 / a0)
  if (a1 <= 0) {
    stop("the first derivative is constant; the form factor is undefined")
  }
  mob1 <- sqrt(a2 / a1)
  list(
    form_factor = mob1 / mob, complexity = mob1 / mob, mobility = mob,
    activity = a0, mobility_of_derivative = mob1, n = length(xs),
    method = "Rangayyan (2024) eqs. (5.25)-(5.26)"
  )
}

#' .morie_rg_turns
#'
#' A step of the rangayyan_stat implementation. Called by \code{TurnsCount}.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param seg A vector; its length is taken and its elements indexed.
#' @param threshold Passed to \code{>}.
#' @return A list with \code{turns}, \code{positions}.
#' @export
.morie_rg_turns <- function(seg, threshold) {
  if (length(seg) < 3L) {
    return(list(turns = 0L, positions = integer(0)))
  }
  turns <- 0L
  idx <- integer(0)
  last <- seg[1]
  direction <- 0L
  for (i in 2:length(seg)) {
    step <- seg[i] - seg[i - 1L]
    if (step == 0) next
    d <- if (step > 0) 1L else -1L
    if (direction == 0L) {
      direction <- d
      next
    }
    if (d != direction) {
      if (abs(seg[i - 1L] - last) > threshold) {
        turns <- turns + 1L
        idx <- c(idx, i - 1L)
        last <- seg[i - 1L]
      }
      direction <- d
    }
  }
  list(turns = turns, positions = idx)
}

#' Section 5.6.3, Willison: a turn is a change of slope, counted only
#'
#' when the swing since the LAST COUNTED TURN exceeds the threshold (100
#' microvolts in the book).  Measuring against the last counted turn
#' rather than the previous sample is what makes it robust in noise, and
#' what separates it from counting turning points.
#'
#' @param x Coerced to numeric by the body, with \code{as.numeric}.
#' @param threshold Passed to \code{.morie_rg_turns}. Defaults to \code{100}.
#' @param window Optional; may be \code{NULL}. Coerced to integer by the body, with
#' \code{as.integer}.
#' @return The value of \code{out}, as built in the body.
#' @export
TurnsCount <- function(x, threshold = 100, window = NULL) {
  # Section 5.6.3, Willison: a turn is a change of slope, counted only
  # when the swing since the LAST COUNTED TURN exceeds the threshold
  # (100 microvolts in the book).  Measuring against the last counted
  # turn rather than the previous sample is what makes it robust in
  # noise, and what separates it from counting turning points.
  xs <- as.numeric(x)
  if (length(xs) < 3L) stop("need at least three samples to have a turn")
  if (threshold < 0) stop("threshold must be nonnegative")
  r <- .morie_rg_turns(xs, threshold)
  out <- list(
    turns = r$turns, positions = r$positions,
    threshold = threshold, n = length(xs),
    method = "Rangayyan (2024) Section 5.6.3"
  )
  if (!is.null(window)) {
    w <- as.integer(window)
    if (w < 3L) stop("window must hold at least three samples")
    out$short_time <- vapply(
      seq_along(xs), function(i) {
        .morie_rg_turns(xs[max(1L, i - w + 1L):i], threshold)$turns
      },
      integer(1)
    )
    out$window <- w
    out$rate <- r$turns / length(xs)
  }
  out
}

#' Section 3.2.1 gives two definitions in one sentence, and they are not
#'
#' interchangeable: the power ratio (10 log10) and the peak-to-peak
#' amplitude over the noise RMS (20 log10, roughly 9 dB higher for a
#' sinusoid).  Both are returned; snr_db is the one named.
#'
#' @param signal Coerced to numeric by the body, with \code{as.numeric}.
#' @param noise Coerced to numeric by the body, with \code{as.numeric}.
#' @param definition One of \code{"peak"}, \code{"power"}. Defaults to \code{"power"}.
#' @return A list with \code{snr_db}, \code{snr_power_db}, \code{snr_peak_db},
#' \code{signal_power}, \code{noise_power}, \code{noise_rms}, \code{definition},
#' \code{method}.
#' @export
Snr <- function(signal, noise, definition = "power") {
  # Section 3.2.1 gives two definitions in one sentence, and they are not
  # interchangeable: the power ratio (10 log10) and the peak-to-peak
  # amplitude over the noise RMS (20 log10, roughly 9 dB higher for a
  # sinusoid).  Both are returned; snr_db is the one named.
  s <- as.numeric(signal)
  e <- as.numeric(noise)
  if (!length(s) || !length(e)) {
    stop("both signal and noise need at least one sample")
  }
  if (!definition %in% c("power", "peak")) {
    stop("definition must be 'power' or 'peak'")
  }
  ps <- .morie_fsum(s * s) / length(s)
  pn <- .morie_fsum(e * e) / length(e)
  if (pn <= 0) stop("noise power is zero; the SNR is unbounded")
  power_db <- 10 * log10(ps / pn)
  peak_db <- if (max(s) > min(s)) {
    20 * log10((max(s) - min(s)) / sqrt(pn))
  } else {
    -Inf
  }
  list(
    snr_db = if (definition == "power") power_db else peak_db,
    snr_power_db = power_db, snr_peak_db = peak_db,
    signal_power = ps, noise_power = pn, noise_rms = sqrt(pn),
    definition = definition,
    method = "Rangayyan (2024) Section 3.2.1"
  )
}

#' The power form of Section 3.2.1 applied to the residual against a
#'
#' known clean reference.  This penalises distortion as well as leftover
#' noise -- comparing only noise power would flatter an over-smoothing
#' filter.
#'
#' @param clean Coerced to numeric by the body, with \code{as.numeric}.
#' @param filtered Coerced to numeric by the body, with \code{as.numeric}.
#' @return A list with \code{snr_db}, \code{residual_power}, \code{signal_power},
#' \code{residual}, \code{n}, \code{method}.
#' @export
SnrFilt <- function(clean, filtered) {
  # The power form of Section 3.2.1 applied to the residual against a
  # known clean reference.  This penalises distortion as well as leftover
  # noise -- comparing only noise power would flatter an over-smoothing
  # filter.
  cc <- as.numeric(clean)
  ff <- as.numeric(filtered)
  if (length(cc) != length(ff)) {
    stop("clean and filtered must have the same length")
  }
  if (!length(cc)) stop("need at least one sample")
  resid <- ff - cc
  ps <- .morie_fsum(cc * cc)
  pr <- .morie_fsum(resid * resid)
  list(
    snr_db = if (pr <= 0) Inf else 10 * log10(ps / pr),
    residual_power = pr, signal_power = ps, residual = resid,
    n = length(cc), method = "Rangayyan (2024) Section 3.2.1"
  )
}

#' Eqs (3.95)-(3.96): y_k = x_k + eta_k, and the sum over k separates
#'
#' into a signal sum that grows as M and a zero-mean noise sum that
#' grows only as sqrt(M).  Dividing by M therefore shrinks the noise SD
#' by 1/sqrt(M): an SNR gain of sqrt(M), or 10 log10(M) dB.
#'
#' @param observations Iterated over elementwise, with \code{lapply}.
#' @return A list with \code{average}, \code{sd}, \code{m}, \code{n}, \code{se},
#' \code{snr_gain}, \code{snr_gain_db}, \code{alignment_note}, \code{method}.
#' @export
SyncAvg <- function(observations) {
  # eqs (3.95)-(3.96): y_k = x_k + eta_k, and the sum over k separates
  # into a signal sum that grows as M and a zero-mean noise sum that
  # grows only as sqrt(M).  Dividing by M therefore shrinks the noise SD
  # by 1/sqrt(M): an SNR gain of sqrt(M), or 10 log10(M) dB.
  recs <- lapply(observations, as.numeric)
  m <- length(recs)
  if (!m) stop("need at least one observation")
  n <- length(recs[[1]])
  if (!n) stop("records must be nonempty")
  if (any(vapply(recs, length, integer(1)) != n)) {
    stop(
      "all realizations must have the same length; averaging ragged ",
      "records would average a different number of traces at ",
      "different instants"
    )
  }
  mat <- matrix(unlist(recs), nrow = m, byrow = TRUE)
  avg <- vapply(seq_len(n), function(i) .morie_fsum(mat[, i]) / m, numeric(1))
  sd <- vapply(
    seq_len(n),
    function(i) sqrt(.morie_fsum((mat[, i] - avg[i])^2) / m),
    numeric(1)
  )
  list(
    average = avg, sd = sd, m = m, n = n, se = sd / sqrt(m),
    snr_gain = sqrt(m), snr_gain_db = 10 * log10(m),
    alignment_note = paste(
      "eqs. (3.95)-(3.96) assume the realizations",
      "are already aligned; misalignment smears",
      "the average"
    ),
    method = "Rangayyan (2024) eqs. (3.95)-(3.96)"
  )
}

#' Eq (3.95): y_k(n) = x_k(n) + eta_k(n), the model that synchronized
#'
#' averaging assumes.  A single x is read as the same signal repeated,
#' which is the book\'s "identical and aligned" case.
#'
#' @param x A list; the body checks with \code{is.list}.
#' @param eta Iterated over elementwise, with \code{lapply}.
#' @return A list with \code{y}, \code{m}, \code{n}, \code{identical_repetitions}, \code{method}.
#' @export
ObsReal <- function(x, eta) {
  # eq (3.95): y_k(n) = x_k(n) + eta_k(n), the model that synchronized
  # averaging assumes.  A single x is read as the same signal repeated,
  # which is the book's "identical and aligned" case.
  noises <- lapply(eta, as.numeric)
  m <- length(noises)
  if (!m) stop("need at least one noise realization")
  n <- length(noises[[1]])
  if (any(vapply(noises, length, integer(1)) != n)) {
    stop("all noise realizations must have the same length")
  }
  signals <- if (is.list(x)) {
    lapply(x, as.numeric)
  } else {
    rep(list(as.numeric(x)), m)
  }
  if (length(signals) == 1L) signals <- rep(signals, m)
  if (length(signals) != m) {
    stop("give one signal per realization, or one for all")
  }
  if (any(vapply(signals, length, integer(1)) != n)) {
    stop("signal and noise records must have equal length")
  }
  y <- Map(function(s, e) s + e, signals, noises)
  first <- signals[[1]]
  identical_reps <- all(vapply(
    signals,
    function(s) all(abs(s - first) < 1e-12),
    logical(1)
  ))
  list(
    y = y, m = m, n = n, identical_repetitions = identical_reps,
    method = "Rangayyan (2024) eq. (3.95)"
  )
}

#' Eqs (6.50)-(6.52): an fBm signal has PSD ~ 1/f^beta, and for a 1-D
#'
#' signal H = (beta-1)/2, FD = (5-beta)/2.  beta is MINUS the slope of
#' log10 P against log10 f.  The DC bin is dropped: log(0) is undefined
#' and DC carries the mean, not the scaling.
#'
#' @param psd Coerced to numeric by the body, with \code{as.numeric}.
#' @param freqs Coerced to numeric by the body, with \code{as.numeric}.
#' @param fmin Optional; may be \code{NULL}. Passed to \code{is.null}.
#' @param fmax Optional; may be \code{NULL}. Passed to \code{is.null}.
#' @return A list with \code{fd}, \code{beta}, \code{hurst}, \code{slope},
#' \code{intercept}, \code{n_bins}, \code{r_squared}, \code{in_range}, \code{band},
#' \code{method}.
#' @export
FdPsd <- function(psd, freqs, fmin = NULL, fmax = NULL) {
  # eqs (6.50)-(6.52): an fBm signal has PSD ~ 1/f^beta, and for a 1-D
  # signal H = (beta-1)/2, FD = (5-beta)/2.  beta is MINUS the slope of
  # log10 P against log10 f.  The DC bin is dropped: log(0) is undefined
  # and DC carries the mean, not the scaling.
  p <- as.numeric(psd)
  f <- as.numeric(freqs)
  if (length(p) != length(f)) stop("psd and freqs must have the same length")
  keep <- f > 0 & p > 0
  if (!is.null(fmin)) keep <- keep & f >= fmin
  if (!is.null(fmax)) keep <- keep & f <= fmax
  if (sum(keep) < 3L) {
    stop("need at least three positive-frequency bins in the band to fit a slope")
  }
  lx <- log10(f[keep])
  ly <- log10(p[keep])
  n <- length(lx)
  mx <- .morie_fsum(lx) / n
  my <- .morie_fsum(ly) / n
  sxx <- .morie_fsum((lx - mx)^2)
  if (sxx <= 0) stop("all retained bins share one frequency")
  slope <- .morie_fsum((lx - mx) * (ly - my)) / sxx
  beta <- -slope
  inter <- my - slope * mx
  ss_tot <- .morie_fsum((ly - my)^2)
  ss_res <- .morie_fsum((ly - (inter + slope * lx))^2)
  list(
    fd = (5 - beta) / 2, beta = beta, hurst = (beta - 1) / 2,
    slope = slope, intercept = inter, n_bins = n,
    r_squared = if (ss_tot > 0) 1 - ss_res / ss_tot else NA_real_,
    in_range = beta >= 0.5 && beta <= 1.5,
    band = c(min(f[keep]), max(f[keep])),
    method = "Rangayyan (2024) eqs. (6.50)-(6.52)"
  )
}

#' .morie_rg_periodogram
#'
#' A step of the rangayyan_stat implementation. Called by \code{EmgFreq}, \code{FdVag},
#' \code{SigFeatures}.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param xs A vector; its length is taken.
#' @param fs Numeric; combined arithmetically in the body.
#' @return A list with \code{psd}, \code{freqs}.
#' @export
.morie_rg_periodogram <- function(xs, fs) {
  m <- length(xs)
  mu <- .morie_fsum(xs) / m
  seg <- xs - mu
  idx <- seq_len(m) - 1L
  k <- 0:(m %/% 2L)
  p <- vapply(k, function(kk) {
    ang <- -2 * pi * kk * idx / m
    re <- .morie_fsum(seg * cos(ang))
    im <- .morie_fsum(seg * sin(ang))
    (re * re + im * im) / m
  }, numeric(1))
  list(psd = p, freqs = k * fs / m)
}

#' Sections 6.6.2-6.6.3: PSA is the book\'s preferred FD estimator for a
#'
#' self-affine signal, applied to knee-joint VAG signals.  The band is
#' an argument because it decides the answer -- a band that reaches down
#' into baseline drift fits the drift\'s slope, not the signal\'s.
#'
#' @param x Coerced to numeric by the body, with \code{as.numeric}.
#' @param fs Coerced to numeric by the body, with \code{as.numeric}.
#' @param fmin Passed to \code{FdPsd}. Defaults to \code{100}.
#' @param fmax Passed to \code{FdPsd}. Defaults to \code{500}.
#' @param nperseg Optional; may be \code{NULL}. Coerced to integer by the body, with
#' \code{as.integer}.
#' @return The value of \code{r}, as built in the body.
#' @export
FdVag <- function(x, fs, fmin = 100, fmax = 500, nperseg = NULL) {
  # Sections 6.6.2-6.6.3: PSA is the book's preferred FD estimator for a
  # self-affine signal, applied to knee-joint VAG signals.  The band is
  # an argument because it decides the answer -- a band that reaches
  # down into baseline drift fits the drift's slope, not the signal's.
  xs <- as.numeric(x)
  if (length(xs) < 8L) stop("need at least eight samples")
  fsv <- as.numeric(fs)
  if (fsv <= 0) stop("fs must be positive")
  m <- if (is.null(nperseg)) length(xs) else min(as.integer(nperseg), length(xs))
  pg <- .morie_rg_periodogram(xs[seq_len(m)], fsv)
  r <- FdPsd(pg$psd, pg$freqs, fmin = fmin, fmax = fmax)
  r$fs <- fsv
  r$nperseg <- m
  r$method <- "Rangayyan (2024) Sections 6.6.2-6.6.3"
  r
}

#' Katz (1988): FD = log10(n) / (log10(n) + log10(d/L)), with L the path
#'
#' length, d the greatest distance from the first point, n = L/a.
#' Rangayyan (2024) Section 5.13.2 covers the ruler, box-counting and
#' Higuchi methods instead, so this is cited to Katz.  It mixes the
#' amplitude and time axes, so rescaling the signal changes the answer.
#'
#' @param x Coerced to numeric by the body, with \code{as.numeric}.
#' @param dt Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{1}.
#' @return A list with \code{fd}, \code{total_length}, \code{max_distance},
#' \code{mean_step}, \code{n_steps}, \code{n}, \code{scale_sensitive}, \code{method}.
#' @export
KatzFd <- function(x, dt = 1) {
  # Katz (1988): FD = log10(n) / (log10(n) + log10(d/L)), with L the path
  # length, d the greatest distance from the first point, n = L/a.
  # Rangayyan (2024) Section 5.13.2 covers the ruler, box-counting and
  # Higuchi methods instead, so this is cited to Katz.  It mixes the
  # amplitude and time axes, so rescaling the signal changes the answer.
  xs <- as.numeric(x)
  n <- length(xs)
  if (n < 3L) stop("need at least three samples")
  step <- as.numeric(dt)
  if (step <= 0) stop("dt must be positive")
  lengths <- sqrt(step^2 + diff(xs)^2)
  total <- .morie_fsum(lengths)
  if (total <= 0) stop("the waveform has zero length")
  d <- max(sqrt(((seq_len(n) - 1L) * step)^2 + (xs - xs[1])^2))
  if (d <= 0) stop("every point coincides with the first")
  a <- total / (n - 1L)
  steps <- total / a
  denom <- log10(steps) + log10(d / total)
  if (denom == 0) stop("the Katz ratio is degenerate for this waveform")
  list(
    fd = log10(steps) / denom, total_length = total, max_distance = d,
    mean_step = a, n_steps = steps, n = n, scale_sensitive = TRUE,
    method = paste(
      "Katz (1988); Rangayyan (2024) Section 5.13.2 covers",
      "the ruler, box-counting and Higuchi methods instead"
    )
  )
}

#' Eq (3.11) applied to the PSD normalized to unit mass.  Rangayyan
#'
#' defines the spectral MOMENTS of Section 6.4.4 but prints no
#' spectral-entropy equation, so this is said to be eq (3.11) applied to
#' the PSD rather than quoted from the book.
#'
#' @param psd Coerced to numeric by the body, with \code{as.numeric}.
#' @param freqs Optional; may be \code{NULL}. Coerced to numeric by the body, with
#' \code{as.numeric}.
#' @param fmin Optional; may be \code{NULL}. Passed to \code{is.null}.
#' @param fmax Optional; may be \code{NULL}. Passed to \code{is.null}.
#' @return A list with \code{entropy}, \code{units}, \code{max_entropy},
#' \code{normalized}, \code{n_bins}, \code{probabilities}, \code{method}.
#' @export
SpecEntropy <- function(psd, freqs = NULL, fmin = NULL, fmax = NULL) {
  # eq (3.11) applied to the PSD normalized to unit mass.  Rangayyan
  # defines the spectral MOMENTS of Section 6.4.4 but prints no
  # spectral-entropy equation, so this is said to be eq (3.11) applied
  # to the PSD rather than quoted from the book.
  p <- as.numeric(psd)
  if (!length(p)) stop("need at least one bin")
  if (any(p < 0)) stop("a PSD cannot be negative")
  if (!is.null(freqs)) {
    f <- as.numeric(freqs)
    if (length(f) != length(p)) {
      stop("psd and freqs must have the same length")
    }
    keep <- rep(TRUE, length(p))
    if (!is.null(fmin)) keep <- keep & f >= fmin
    if (!is.null(fmax)) keep <- keep & f <= fmax
    if (!any(keep)) stop("the band retains no bins")
    p <- p[keep]
  }
  total <- .morie_fsum(p)
  if (total <= 0) stop("the PSD has zero total power")
  probs <- p / total
  nz <- probs[probs > 0]
  h <- -.morie_fsum(nz * log(nz) / log(2))
  k <- length(probs)
  hmax <- if (k > 1L) log(k) / log(2) else 0
  list(
    entropy = h, units = "bits", max_entropy = hmax,
    normalized = if (hmax > 0) h / hmax else 0, n_bins = k,
    probabilities = probs,
    method = "Rangayyan (2024) eq. (3.11) applied to the PSD"
  )
}

#' MFR = 1/mean(IDI), CV = SD(IDI)/mean(IDI).  MFR is the RECIPROCAL OF
#'
#' THE MEAN interval, not the mean of the reciprocals; the two differ
#' whenever the intervals vary, and only the former equals discharges
#' per unit time.  Both are returned.
#'
#' @param times Coerced to numeric by the body, with \code{as.numeric}.
#' @param fs Optional; may be \code{NULL}. Coerced to numeric by the body, with \code{as.numeric}.
#' @return A list with \code{mfr}, \code{mean_idi}, \code{sd_idi}, \code{cv_idi},
#' \code{idi}, \code{n_discharges}, \code{mean_instantaneous_rate}, \code{duration},
#' \code{method}.
#' @export
FiringRate <- function(times, fs = NULL) {
  # MFR = 1/mean(IDI), CV = SD(IDI)/mean(IDI).  MFR is the RECIPROCAL OF
  # THE MEAN interval, not the mean of the reciprocals; the two differ
  # whenever the intervals vary, and only the former equals discharges
  # per unit time.  Both are returned.
  ts <- as.numeric(times)
  if (length(ts) < 2L) {
    stop("need at least two discharges to form an interval")
  }
  if (!is.null(fs)) {
    if (fs <= 0) stop("fs must be positive")
    ts <- ts / as.numeric(fs)
  }
  if (any(diff(ts) <= 0)) {
    stop("discharge instants must be strictly increasing")
  }
  idi <- diff(ts)
  m <- .morie_fsum(idi) / length(idi)
  sd <- sqrt(.morie_fsum((idi - m)^2) / length(idi))
  list(
    mfr = 1 / m, mean_idi = m, sd_idi = sd, cv_idi = sd / m, idi = idi,
    n_discharges = length(ts),
    mean_instantaneous_rate = .morie_fsum(1 / idi) / length(idi),
    duration = ts[length(ts)] - ts[1],
    method = "Rangayyan (2024) Sections 4.2, 5.x (motor-unit discharge statistics)"
  )
}

#' The descriptors Rangayyan uses across Chapters 3, 5 and 6, each
#'
#' computed by the function that owns its definition so the vector
#' cannot disagree with the individual measures.  threshold defaults to
#' 0 (every direction change counts); pass the book\'s 100 microvolts
#' for a real EMG record or the count is dominated by noise.
#'
#' @param x Coerced to numeric by the body, with \code{as.numeric}.
#' @param fs Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{1}.
#' @param threshold Passed to \code{TurnsCount}. Defaults to \code{0}.
#' @return A list with \code{mean}, \code{sd}, \code{rms}, \code{zero_crossings},
#' \code{zcr}, \code{turns}, \code{activity}, \code{mobility}, \code{form_factor},
#' \code{spectral_centroid}, \code{spectral_bandwidth}, \code{spectral_entropy},
#' \code{n}, \code{fs}, \code{method}.
#' @export
SigFeatures <- function(x, fs = 1, threshold = 0) {
  # The descriptors Rangayyan uses across Chapters 3, 5 and 6, each
  # computed by the function that owns its definition so the vector
  # cannot disagree with the individual measures.  threshold defaults to
  # 0 (every direction change counts); pass the book's 100 microvolts
  # for a real EMG record or the count is dominated by noise.
  xs <- as.numeric(x)
  if (length(xs) < 8L) stop("need at least eight samples")
  fsv <- as.numeric(fs)
  if (fsv <= 0) stop("fs must be positive")
  n <- length(xs)
  mu <- .morie_fsum(xs) / n
  sd <- sqrt(.morie_fsum((xs - mu)^2) / n)
  hj <- FormFactor(xs)
  zc <- sum((xs[-n] < 0 & xs[-1] >= 0) | (xs[-n] >= 0 & xs[-1] < 0))
  pg <- .morie_rg_periodogram(xs, fsv)
  tot <- .morie_fsum(pg$psd)
  centroid <- if (tot > 0) .morie_fsum(pg$freqs * pg$psd) / tot else 0
  bw <- if (tot > 0) {
    sqrt(.morie_fsum((pg$freqs - centroid)^2 * pg$psd) / tot)
  } else {
    0
  }
  list(
    mean = mu, sd = sd, rms = sqrt(.morie_fsum(xs * xs) / n),
    zero_crossings = zc, zcr = zc * fsv / n,
    turns = TurnsCount(xs, threshold = threshold)$turns,
    activity = hj$activity, mobility = hj$mobility,
    form_factor = hj$form_factor,
    spectral_centroid = centroid, spectral_bandwidth = bw,
    spectral_entropy = SpecEntropy(pg$psd)$entropy,
    n = n, fs = fsv,
    method = "Rangayyan (2024) Chapters 3, 5, 6 feature set"
  )
}

# pre-policy spellings
morie_rms <- Rms
morie_form_factor <- FormFactor
morie_turns_count <- TurnsCount
morie_snr <- Snr
morie_signal_to_noise <- SnrFilt
morie_sync_average <- SyncAvg
morie_ch3_observed_realization <- ObsReal
morie_fd_psd_slope <- FdPsd
morie_fractal_vag <- FdVag
morie_katz_fd <- KatzFd
morie_spectral_entropy <- SpecEntropy
morie_muap_firing_rate <- FiringRate
morie_signal_features <- SigFeatures

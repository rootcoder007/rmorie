# Rangayyan ECG event detection and rate analysis: QRS detectors, P and T
# waves, heart rate, HRV.  Mirror of the Python bsaqrs module.
#
# Equation numbers verified in the PDF: 3.132-3.133, 3.150, 4.1-4.23.
#
# Index convention: every local index variable in this file is ZERO-based so
# the arithmetic reads exactly as the Python arm does; element access is
# therefore always written x[i + 1L].  Sample positions in the payloads are
# zero-based for the same reason -- they must compare equal across the arms.

#' .morie_qrs_pad
#'
#' A step of the rangayyan_qrs implementation. Called by \code{QrsDeriv}, \code{QrsDeriv1}, \code{QrsDeriv2} and 1 others in the module.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param v See Usage.
#' @param k A count; the body uses it as \code{rep(...)}.
#' @return A vector, from \code{c}.
#' @export
.morie_qrs_pad <- function(v, k) c(rep(0, k), v)

#' Causal m-point moving average; the first m-1 outputs use a short
#' window
#'
#' The running sum is accumulated in the same order as the Python arm,
#' so the rounding sequence matches term for term.
#'
#' @param x A vector; its length is taken and its elements indexed.
#' @param m Numeric; passed to \code{min}.
#' @return The value of \code{out}, as built in the body.
#' @export
.morie_qrs_mavg <- function(x, m) {
  # Causal m-point moving average; the first m-1 outputs use a short window.
  # The running sum is accumulated in the same order as the Python arm, so
  # the rounding sequence matches term for term.
  m <- as.integer(m)
  if (m < 1L) stop("moving-average length must be >= 1")
  n <- length(x)
  out <- numeric(n)
  run <- 0
  for (i in seq_len(n)) {
    run <- run + x[i]
    if (i - 1L >= m) run <- run - x[i - m]
    out[i] <- run / min(i, m)
  }
  out
}

#' Plain O(n^2) DFT.  The angle is formed as w * j with w = -2 pi k / n,
#'
#' NOT as -2 pi k j / n: the two differ in the last bits and the Python
#' arm uses the former.
#'
#' @param x A vector; its length is taken.
#' @return A list with \code{re}, \code{im}.
#' @export
.morie_qrs_dft <- function(x) {
  # Plain O(n^2) DFT.  The angle is formed as w * j with w = -2 pi k / n,
  # NOT as -2 pi k j / n: the two differ in the last bits and the Python arm
  # uses the former.
  n <- length(x)
  if (n < 2L) stop("DFT needs at least two samples")
  j <- seq_len(n) - 1
  re <- numeric(n)
  im <- numeric(n)
  for (k in seq_len(n) - 1L) {
    w <- -2 * pi * k / n
    re[k + 1L] <- .morie_fsum(x * cos(w * j))
    im[k + 1L] <- .morie_fsum(x * sin(w * j))
  }
  list(re = re, im = im)
}

#' One-sided periodogram; power in units^2/Hz
#'
#' A step of the rangayyan_qrs implementation. Called by \code{EcgEmgCpl}, \code{EdrSignal}, \code{HrvFreq} and 1 others in the module.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param x A vector; its length is taken.
#' @param fs Numeric; combined arithmetically in the body.
#' @return A list with \code{freqs}, \code{power}.
#' @export
.morie_qrs_psd <- function(x, fs) {
  # One-sided periodogram; power in units^2/Hz.
  n <- length(x)
  sp <- .morie_qrs_dft(x)
  nyq <- n %/% 2L
  ks <- 0:nyq
  power <- numeric(length(ks))
  for (kk in ks) {
    re <- sp$re[kk + 1L]
    im <- sp$im[kk + 1L]
    p <- (re * re + im * im) / (fs * n)
    if (kk > 0L && kk < n - kk) p <- p * 2
    power[kk + 1L] <- p
  }
  list(freqs = fs * ks / n, power = power)
}

#' Peak set {p} of eq (4.6): above th and strictly greater than the m
#'
#' neighbours on each side.
#'
#' @param g A vector; its length is taken and its elements indexed.
#' @param th See Usage.
#' @param m Numeric; combined arithmetically in the body.
#' @return The value of \code{out}, as built in the body.
#' @export
.morie_qrs_peaks <- function(g, th, m) {
  # Peak set {p} of eq (4.6): above th and strictly greater than the m
  # neighbours on each side.
  n <- length(g)
  m <- as.integer(m)
  out <- integer(0)
  for (i in seq_len(n) - 1L) {
    gi <- g[i + 1L]
    if (gi <= th) next
    lo <- max(0L, i - m)
    hi <- min(n, i + m + 1L)
    ok <- TRUE
    if (lo <= i - 1L) ok <- all(gi > g[(lo + 1L):i])
    if (ok && (i + 1L) <= (hi - 1L)) ok <- all(gi > g[(i + 2L):hi])
    if (ok) out <- c(out, i)
  }
  out
}

#' Pan-Tompkins bandpass: eq (4.8) lowpass then eq (4.13) highpass.  The
#'
#' coefficients are integers tied to fs = 200 Hz (book, Section 4.3.2)
#' and are NOT rescaled here.
#'
#' @param x A vector; its length is taken and its elements indexed.
#' @return The value of \code{p}, as built in the body.
#' @export
.morie_qrs_ptbp <- function(x) {
  # Pan-Tompkins bandpass: eq (4.8) lowpass then eq (4.13) highpass.  The
  # coefficients are integers tied to fs = 200 Hz (book, Section 4.3.2) and
  # are NOT rescaled here.
  n <- length(x)
  y <- numeric(n)
  for (i in seq_len(n) - 1L) {
    a <- if (i >= 1L) 2 * y[i] else 0
    b <- if (i >= 2L) y[i - 1L] else 0
    cc <- x[i + 1L] - (if (i >= 6L) 2 * x[i - 5L] else 0) +
      (if (i >= 12L) x[i - 11L] else 0)
    y[i + 1L] <- a - b + cc / 32
  }
  p <- numeric(n)
  for (i in seq_len(n) - 1L) {
    prev <- if (i >= 1L) p[i] else 0
    p[i + 1L] <- prev - y[i + 1L] / 32 +
      (if (i >= 16L) y[i - 15L] else 0) -
      (if (i >= 17L) y[i - 16L] else 0) +
      (if (i >= 32L) y[i - 31L] / 32 else 0)
  }
  p
}

#' Pan-Tompkins derivative, eq (4.14)
#'
#' A step of the rangayyan_qrs implementation. Called by \code{.morie_qrs_chain}, \code{QrsDerivOp}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param x A vector; its length is taken and its elements indexed.
#' @return The value of \code{out}, as built in the body.
#' @export
.morie_qrs_ptderiv <- function(x) {
  # Pan-Tompkins derivative, eq (4.14).
  n <- length(x)
  out <- numeric(n)
  for (i in seq_len(n) - 1L) {
    out[i + 1L] <- (2 * x[i + 1L] + (if (i >= 1L) x[i] else 0) -
      (if (i >= 3L) x[i - 2L] else 0) -
      2 * (if (i >= 4L) x[i - 3L] else 0)) / 8
  }
  out
}

#' Full Pan-Tompkins front end: bandpass, derivative, square, integrate
#'
#' A step of the rangayyan_qrs implementation. Called by \code{QrsDetect}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param x Passed to \code{.morie_qrs_ptbp}.
#' @param fs Numeric; combined arithmetically in the body.
#' @return A list with \code{bp}, \code{dv}, \code{sq}, \code{ig}, \code{w}.
#' @export
.morie_qrs_chain <- function(x, fs) {
  # Full Pan-Tompkins front end: bandpass, derivative, square, integrate.
  bp <- .morie_qrs_ptbp(x)
  dv <- .morie_qrs_ptderiv(bp)
  sq <- dv * dv
  w <- max(1L, as.integer(round(0.150 * fs)))
  list(bp = bp, dv = dv, sq = sq, ig = .morie_qrs_mavg(sq, w), w = w)
}

#' .morie_qrs_corr
#'
#' A step of the rangayyan_qrs implementation. Called by \code{EcgEmgCpl}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param a A vector; its length is taken.
#' @param b A vector; its length is taken.
#' @return A numeric value.
#' @export
.morie_qrs_corr <- function(a, b) {
  n <- length(a)
  if (n != length(b) || n < 2L) {
    stop("correlation needs two equal-length series of length >= 2")
  }
  ma <- .morie_fsum(a) / n
  mb <- .morie_fsum(b) / n
  sa <- .morie_fsum((a - ma)^2)
  sb <- .morie_fsum((b - mb)^2)
  if (sa <= 0 || sb <= 0) {
    return(0)
  }
  .morie_fsum((a - ma) * (b - mb)) / sqrt(sa * sb)
}

#' .morie_qrs_check
#'
#' A step of the rangayyan_qrs implementation. Called by \code{ApneaEdr}, \code{CPulseFeat}, \code{DicNotch} and 29 others in the module.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param x Optional; may be \code{NULL}. Coerced to numeric by the body, with \code{as.numeric}.
#' @param least Defaults to \code{1L}.
#' @param what Defaults to \code{"signal"}.
#' @return The value of \code{v}, as built in the body.
#' @export
.morie_qrs_check <- function(x, least = 1L, what = "signal") {
  v <- if (is.null(x)) numeric(0) else as.numeric(x)
  if (length(v) < least) {
    stop(sprintf("%s needs at least %d samples, got %d", what, least, length(v)))
  }
  v
}

#' .morie_qrs_fs
#'
#' A step of the rangayyan_qrs implementation. Called by \code{ApneaEdr}, \code{CPulseFeat}, \code{DicNotch} and 20 others in the module.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param fs A vector; its length is taken.
#' @return The value of \code{fs}, as built in the body.
#' @export
.morie_qrs_fs <- function(fs) {
  fs <- as.numeric(fs)
  if (!(length(fs) == 1L && is.finite(fs) && fs > 0)) stop("fs must be positive")
  fs
}

#' .morie_qrs_median
#'
#' A step of the rangayyan_qrs implementation. Called by \code{MotionArt}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param v Numeric; passed to \code{sort}.
#' @return One of two values, depending on the branch taken.
#' @export
.morie_qrs_median <- function(v) {
  s <- sort(v)
  m <- length(s)
  if (m %% 2L) s[m %/% 2L + 1L] else 0.5 * (s[m %/% 2L] + s[m %/% 2L + 1L])
}

#' .morie_qrs_mean
#'
#' A step of the rangayyan_qrs implementation. Called by \code{CPulseFeat}, \code{EcgFeat}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param v A vector; its length is taken.
#' @return One of two values, depending on the branch taken.
#' @export
.morie_qrs_mean <- function(v) if (length(v)) .morie_fsum(v) / length(v) else NULL

#' 0-based argmax over the half-open range [lo, hi); first max on ties,
#'
#' exactly as Python\'s max(range(...), key=...).
#'
#' @param v A vector; indexed elementwise.
#' @param lo Numeric; combined arithmetically in the body.
#' @param hi See Usage.
#' @return A numeric value.
#' @export
.morie_qrs_argmax <- function(v, lo, hi) {
  # 0-based argmax over the half-open range [lo, hi); first max on ties,
  # exactly as Python's max(range(...), key=...).
  lo + which.max(v[(lo + 1L):hi]) - 1L
}

#' .morie_qrs_argmin
#'
#' A step of the rangayyan_qrs implementation. Called by \code{DicNotch}, \code{EcgFeat}, \code{PpgFeat}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param v A vector; indexed elementwise.
#' @param lo Numeric; combined arithmetically in the body.
#' @param hi See Usage.
#' @return A numeric value.
#' @export
.morie_qrs_argmin <- function(v, lo, hi) {
  lo + which.min(v[(lo + 1L):hi]) - 1L
}


#' Eqs (3.132)-(3.133): H(z) = (1/T)(1 - z^-1)/(1 - 0.995 z^-1).  The
#' zero at
#'
#' z = 1 kills DC (which is what baseline wander is); the pole close to
#' it restores the gain to roughly unity by about 0.5 Hz so the QRS
#' survives. The 1/T factor is applied as written, so the output is
#' scaled by fs.
#'
#' @param ecg Optional; may be \code{NULL}. Coerced to numeric by the body, with \code{as.numeric}.
#' @param fs Coerced to numeric by the body, with \code{as.numeric}.
#' @param pole Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{0.995}.
#' @return A list with \code{ecg_detrended}, \code{n}, \code{fs}, \code{pole}, \code{gain_dc}, \code{gain_at_half_hz}, \code{gain_at_nyquist}, \code{gain_relative_at_half_hz}, \code{dc_is_rejected}, \code{zero_at_z_equals_one}, \code{pole_restores_gain_above_the_wander_band}, \code{differentiates_by_the_one_over_T_factor}, \code{method}.
#' @export
BlWander <- function(ecg, fs, pole = 0.995) {
  # eqs (3.132)-(3.133): H(z) = (1/T)(1 - z^-1)/(1 - 0.995 z^-1).  The zero at
  # z = 1 kills DC (which is what baseline wander is); the pole close to it
  # restores the gain to roughly unity by about 0.5 Hz so the QRS survives.
  # The 1/T factor is applied as written, so the output is scaled by fs.
  xs <- if (is.null(ecg)) numeric(0) else as.numeric(ecg)
  n <- length(xs)
  if (n < 2L) stop("need at least two samples")
  fsv <- as.numeric(fs)
  if (fsv <= 0) stop("fs must be positive")
  a <- as.numeric(pole)
  if (!(a >= 0 && a < 1)) {
    stop(sprintf("the pole must lie inside the unit circle, 0 <= pole < 1; got %g", a))
  }
  y <- numeric(n)
  prev <- 0
  for (i in seq_len(n - 1L) + 1L) {
    prev <- a * prev + fsv * (xs[i] - xs[i - 1L])
    y[i] <- prev
  }
  gain <- function(f) {
    w <- 2 * pi * f / fsv
    num <- complex(real = 1 - cos(w), imaginary = sin(w))
    den <- complex(real = 1 - a * cos(w), imaginary = a * sin(w))
    if (Mod(den) == 0) {
      return(Inf)
    }
    Mod(num / den) * fsv
  }
  g0 <- gain(0)
  ghalf <- gain(0.5)
  gnyq <- gain(fsv / 2)
  list(
    ecg_detrended = y, n = n, fs = fsv, pole = a,
    gain_dc = g0, gain_at_half_hz = ghalf, gain_at_nyquist = gnyq,
    gain_relative_at_half_hz = if (gnyq > 0) ghalf / gnyq else NULL,
    dc_is_rejected = g0 < 1e-12,
    zero_at_z_equals_one = TRUE,
    pole_restores_gain_above_the_wander_band = TRUE,
    differentiates_by_the_one_over_T_factor = TRUE,
    method = paste0(
      "Rangayyan (2024) eqs. (3.132)-(3.133), modified ",
      "first-order difference for baseline-wander removal"
    )
  )
}


#' Lehner and Rangayyan, Section 4.3.5.  The noncausal least-squares
#' second
#'
#' derivative of eq (4.22), squared and smoothed by eq (4.23).  The
#' second derivative is used because the notch rides on the falling limb
#' of the pulse, which a first derivative cannot separate from the limb
#' itself.
#'
#' @param cp A vector; its length is taken and its elements indexed.
#' @param fs Numeric; combined arithmetically in the body.
#' @param qrs Optional; may be \code{NULL}. Coerced to integer by the body, with \code{as.integer}.
#' @param mwin Numeric; combined arithmetically in the body. Defaults to \code{16}.
#' @return A list with \code{notch}, \code{upstroke}, \code{s}, \code{p}, \code{mwin}, \code{fs}, \code{tolerancems}, \code{method}.
#' @export
DicNotch <- function(cp, fs, qrs = NULL, mwin = 16) {
  # Lehner and Rangayyan, Section 4.3.5.  The noncausal least-squares second
  # derivative of eq (4.22), squared and smoothed by eq (4.23).  The second
  # derivative is used because the notch rides on the falling limb of the
  # pulse, which a first derivative cannot separate from the limb itself.
  fs <- .morie_qrs_fs(fs)
  cp <- .morie_qrs_check(cp, 9L, "carotid pulse")
  mwin <- as.integer(mwin)
  if (mwin < 1L) stop("mwin must be >= 1")
  n <- length(cp)
  p <- numeric(n)
  for (i in 2:(n - 3L)) {
    p[i + 1L] <- 2 * cp[i - 1L] - cp[i] - 2 * cp[i + 1L] -
      cp[i + 2L] + 2 * cp[i + 3L]
  }
  s <- numeric(n)
  for (i in seq_len(n) - 1L) {
    kk <- 1:mwin
    kk <- kk[i - kk + 1L >= 0L]
    s[i + 1L] <- if (length(kk)) {
      .morie_fsum(p[i - kk + 2L]^2 * (mwin - kk + 1L))
    } else {
      0
    }
  }
  tol <- max(1L, as.integer(round(0.020 * fs)))
  guard <- max(1L, as.integer(round(0.050 * fs)))
  localmin <- function(centre) {
    lo <- max(0L, centre - tol)
    hi <- min(n, centre + tol + 1L)
    .morie_qrs_argmin(cp, lo, hi)
  }
  notch <- integer(0)
  upstroke <- integer(0)
  if (!is.null(qrs)) {
    span <- as.integer(round(0.500 * fs))
    for (q in as.integer(qrs)) {
      lo <- max(0L, q)
      hi <- min(n, q + span)
      if (hi - lo < 3L * guard) next
      sseg <- s[(lo + 1L):hi]
      pk <- .morie_qrs_peaks(sseg, 0.25 * max(sseg), guard)
      if (length(pk) < 2L) next
      upstroke <- c(upstroke, lo + pk[1L])
      notch <- c(notch, localmin(lo + pk[2L]))
    }
  } else {
    pk <- .morie_qrs_peaks(s, 0.25 * max(s), guard)
    for (j in seq_along(pk)) {
      if ((j - 1L) %% 2L == 0L) {
        upstroke <- c(upstroke, pk[j])
      } else {
        notch <- c(notch, localmin(pk[j]))
      }
    }
  }
  list(
    notch = notch, upstroke = upstroke, s = s, p = p,
    mwin = mwin, fs = fs, tolerancems = 20,
    method = paste0(
      "dicrotic notch detection, Rangayyan (2024) Section ",
      "4.3.5, Eqs 4.22 and 4.23 (Lehner and Rangayyan)"
    )
  )
}


#' Carotid pulse landmarks (Section 1.2.10) and the systolic time
#' intervals
#'
#' of Section 4.9.  PEPC = PEP + 0.4 HR and ETC = ET + 1.6 HR are the
#' rate corrections that make the intervals comparable between subjects.
#'
#' @param cp A vector; its length is taken.
#' @param fs Numeric; combined arithmetically in the body.
#' @param qrs Coerced to integer by the body, with \code{as.integer}.
#' @param hr Optional; may be \code{NULL}. Numeric; combined arithmetically in the body.
#' @return A list with \code{upstroke}, \code{percussion}, \code{notch}, \code{dicwave}, \code{pep}, \code{et}, \code{pepc}, \code{etc}, \code{peppmean}, \code{etmean}, \code{pepcmean}, \code{etcmean}, \code{hr}, \code{fs}, \code{normpepc}, \code{normetcmale}, \code{normetcfemale}, \code{method}.
#' @export
CPulseFeat <- function(cp, fs, qrs, hr = NULL) {
  # Carotid pulse landmarks (Section 1.2.10) and the systolic time intervals
  # of Section 4.9.  PEPC = PEP + 0.4 HR and ETC = ET + 1.6 HR are the
  # rate corrections that make the intervals comparable between subjects.
  fs <- .morie_qrs_fs(fs)
  cp <- .morie_qrs_check(cp, 16L, "carotid pulse")
  q <- as.integer(qrs)
  if (length(q) < 1L) stop("need at least one QRS position")
  if (is.null(hr)) {
    if (length(q) < 2L) {
      stop("hr must be given when fewer than two QRS positions are supplied")
    }
    hr <- 60 * fs / (.morie_fsum(diff(q)) / (length(q) - 1L))
  }
  hr <- as.numeric(hr)
  if (!(hr > 0)) stop("hr must be positive")

  dn <- DicNotch(cp, fs, qrs = q)
  notch <- dn$notch
  ups <- dn$upstroke
  n <- length(cp)
  span <- as.integer(round(0.500 * fs))

  perc <- integer(0)
  dicw <- integer(0)
  pep <- numeric(0)
  et <- numeric(0)
  kmax <- min(length(notch), length(ups))
  for (k in seq_len(kmax)) {
    u <- ups[k]
    d <- notch[k]
    if (d <= u) next
    perc <- c(perc, .morie_qrs_argmax(cp, u, d))
    hi <- min(n, d + as.integer(round(0.200 * fs)))
    dicw <- c(dicw, if (hi > d + 1L) .morie_qrs_argmax(cp, d, hi) else d)
    prior <- q[q <= u & u - q <= span]
    if (length(prior)) pep <- c(pep, 1000 * (u - prior[length(prior)]) / fs)
    et <- c(et, 1000 * (d - u) / fs)
  }
  pepc <- pep + 0.4 * hr
  etc <- et + 1.6 * hr
  list(
    upstroke = ups, percussion = perc, notch = notch, dicwave = dicw,
    pep = pep, et = et, pepc = pepc, etc = etc,
    peppmean = .morie_qrs_mean(pep), etmean = .morie_qrs_mean(et),
    pepcmean = .morie_qrs_mean(pepc), etcmean = .morie_qrs_mean(etc),
    hr = hr, fs = fs,
    normpepc = c(131, 13), normetcmale = c(395, 13),
    normetcfemale = c(415, 11),
    method = paste0(
      "carotid pulse features and systolic time intervals, ",
      "Rangayyan (2024) Sections 1.2.10 and 4.9"
    )
  )
}


#' Balda et al., Section 4.3.1: y0 by eq (4.1), y1 by eq (4.2), combined
#' as
#'
#' y2 = 1.3 y0 + 1.1 y1 (eq 4.3), scanned with a threshold of 1.0 on a
#' maximum-normalised ECG.  Six of eight successive samples must pass,
#' which is what makes the decision robust to single-sample derivative
#' spikes.
#'
#' @param x Numeric; passed to \code{abs}.
#' @param fs Numeric; combined arithmetically in the body.
#' @param thresh Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{1}.
#' @return A list with \code{qrs}, \code{y0}, \code{y1}, \code{y2}, \code{y3}, \code{mask}, \code{thresh}, \code{fs}, \code{hr}, \code{method}.
#' @export
QrsDeriv <- function(x, fs, thresh = 1) {
  # Balda et al., Section 4.3.1: y0 by eq (4.1), y1 by eq (4.2), combined as
  # y2 = 1.3 y0 + 1.1 y1 (eq 4.3), scanned with a threshold of 1.0 on a
  # maximum-normalised ECG.  Six of eight successive samples must pass, which
  # is what makes the decision robust to single-sample derivative spikes.
  fs <- .morie_qrs_fs(fs)
  x <- .morie_qrs_check(x, 16L, "ECG")
  thresh <- as.numeric(thresh)
  if (!(thresh > 0)) stop("thresh must be positive")
  peak <- max(abs(x))
  if (peak <= 0) stop("ECG is identically zero")
  xn <- x / peak
  n <- length(xn)
  y0 <- .morie_qrs_pad(abs(xn[3:n] - xn[1:(n - 2L)]), 2L)
  y1 <- .morie_qrs_pad(abs(xn[5:n] - 2 * xn[3:(n - 2L)] + xn[1:(n - 4L)]), 4L)
  y2 <- 1.3 * y0 + 1.1 * y1
  y3 <- .morie_qrs_mavg(y2, 8L)

  mask <- rep(FALSE, n)
  i <- 0L
  while (i < n) {
    if (y2[i + 1L] > thresh) {
      seg <- y2[(i + 1L):min(n, i + 8L)]
      if (sum(seg > thresh) >= 6L) {
        mask[(i + 1L):min(n, i + 8L)] <- TRUE
        i <- i + 8L
        next
      }
    }
    i <- i + 1L
  }

  qrs <- integer(0)
  i <- 0L
  delay <- 8L
  while (i < n) {
    if (mask[i + 1L]) {
      j <- i
      while (j < n && mask[j + 1L]) j <- j + 1L
      best <- .morie_qrs_argmax(y3, i, j)
      qrs <- c(qrs, max(0L, best - delay))
      i <- j
    } else {
      i <- i + 1L
    }
  }
  list(
    qrs = qrs, y0 = y0, y1 = y1, y2 = y2, y3 = y3, mask = mask,
    thresh = thresh, fs = fs,
    hr = if (length(qrs)) 60 * length(qrs) / (n / fs) else 0,
    method = paste0(
      "derivative-based QRS detection, Rangayyan (2024) ",
      "Section 4.3.1, Eqs 4.1-4.3 with the 8-point MA of ",
      "Eq 3.108 (Balda et al.)"
    )
  )
}


#' Section 2.2.6 (EMG RMS and mean frequency rise with contraction)
#' meets
#'
#' Section 2.2.5 (rate rises with effort).  Both series are sampled on
#' the same beat grid so the correlation between them is well defined.
#'
#' @param ecg A vector; its length is taken.
#' @param emg A vector; its length is taken and its elements indexed.
#' @param qrs Coerced to integer by the body, with \code{as.integer}.
#' @param fs Numeric; combined arithmetically in the body.
#' @return A list with \code{hr}, \code{rms}, \code{meanfreq}, \code{rrms}, \code{rmnf}, \code{nbeats}, \code{fs}, \code{method}.
#' @export
EcgEmgCpl <- function(ecg, emg, qrs, fs) {
  # Section 2.2.6 (EMG RMS and mean frequency rise with contraction) meets
  # Section 2.2.5 (rate rises with effort).  Both series are sampled on the
  # same beat grid so the correlation between them is well defined.
  fs <- .morie_qrs_fs(fs)
  ecg <- .morie_qrs_check(ecg, 16L, "ECG")
  emg <- .morie_qrs_check(emg, 16L, "EMG")
  if (length(ecg) != length(emg)) {
    stop("ECG and EMG must have the same length and rate")
  }
  q <- as.integer(qrs)
  if (length(q) < 3L) stop("need at least three QRS positions")
  hr <- numeric(0)
  rms <- numeric(0)
  mnf <- numeric(0)
  for (k in seq_len(length(q) - 1L) + 1L) {
    a <- q[k - 1L]
    b <- q[k]
    if (b - a < 8L) next
    seg <- emg[(a + 1L):b]
    mu <- .morie_fsum(seg) / length(seg)
    seg <- seg - mu
    hr <- c(hr, 60 * fs / (b - a))
    rms <- c(rms, sqrt(.morie_fsum(seg * seg) / length(seg)))
    sp <- .morie_qrs_psd(seg, fs)
    tot <- .morie_fsum(sp$power)
    mnf <- c(mnf, if (tot > 0) {
      .morie_fsum(sp$freqs * sp$power) / tot
    } else {
      0
    })
  }
  if (length(hr) < 2L) {
    stop("too few usable cardiac cycles for a coupling estimate")
  }
  list(
    hr = hr, rms = rms, meanfreq = mnf,
    rrms = .morie_qrs_corr(hr, rms), rmnf = .morie_qrs_corr(hr, mnf),
    nbeats = length(hr), fs = fs,
    method = paste0(
      "per-cycle EMG RMS and mean frequency correlated with ",
      "instantaneous heart rate, Rangayyan (2024) Sections ",
      "2.2.5 and 2.2.6"
    )
  )
}


#' Section 1.2.4 waves and intervals.  Amplitudes are measured against
#' the
#'
#' PQ segment, not against zero: the PQ segment is the isoelectric
#' reference the book names, and it absorbs any residual baseline
#' offset.
#'
#' @param x A vector; its length is taken and its elements indexed.
#' @param qrs Coerced to integer by the body, with \code{as.integer}.
#' @param fs Numeric; combined arithmetically in the body.
#' @return A list with \code{pamp}, \code{qamp}, \code{ramp}, \code{samp}, \code{tamp}, \code{qrsdur}, \code{pdur}, \code{tdur}, \code{prdur}, \code{qtdur}, \code{qrsdurmean}, \code{prdurmean}, \code{qtdurmean}, \code{rampmean}, \code{nbeats}, \code{fs}, \code{method}.
#' @export
EcgFeat <- function(x, qrs, fs) {
  # Section 1.2.4 waves and intervals.  Amplitudes are measured against the
  # PQ segment, not against zero: the PQ segment is the isoelectric reference
  # the book names, and it absorbs any residual baseline offset.
  fs <- .morie_qrs_fs(fs)
  x <- .morie_qrs_check(x, 16L, "ECG")
  q <- as.integer(qrs)
  if (!length(q)) stop("need at least one QRS position")
  n <- length(x)
  pqa <- max(1L, as.integer(round(0.080 * fs)))
  pqb <- max(1L, as.integer(round(0.040 * fs)))
  half <- max(1L, as.integer(round(0.060 * fs)))
  pwin <- max(2L, as.integer(round(0.200 * fs)))
  twinlo <- max(1L, as.integer(round(0.100 * fs)))
  twinhi <- max(2L, as.integer(round(0.450 * fs)))

  pamp <- numeric(0)
  ramp <- numeric(0)
  qamp <- numeric(0)
  samp <- numeric(0)
  tamp <- numeric(0)
  qrsdur <- numeric(0)
  pdur <- numeric(0)
  tdur <- numeric(0)
  prdur <- numeric(0)
  qtdur <- numeric(0)

  for (pos in q) {
    a <- max(0L, pos - pqa)
    b <- max(a + 1L, pos - pqb)
    ref <- .morie_fsum(x[(a + 1L):b]) / (b - a)
    lo <- max(0L, pos - half)
    hi <- min(n, pos + half)
    if (hi - lo < 3L) next
    r <- .morie_qrs_argmax(x, lo, hi)
    qi <- if (r > lo) .morie_qrs_argmin(x, lo, r + 1L) else r
    si <- if (hi > r + 1L) .morie_qrs_argmin(x, r, hi) else r
    ramp <- c(ramp, x[r + 1L] - ref)
    qamp <- c(qamp, x[qi + 1L] - ref)
    samp <- c(samp, x[si + 1L] - ref)

    amp <- max(abs(x[r + 1L] - ref), 1e-12)
    tol <- 0.05 * amp
    qs <- qi
    while (qs > lo && abs(x[qs + 1L] - ref) > tol) qs <- qs - 1L
    qe <- si
    while (qe < hi - 1L && abs(x[qe + 1L] - ref) > tol) qe <- qe + 1L
    qrsdur <- c(qrsdur, (qe - qs) / fs)

    pa <- max(0L, qs - pwin)
    pb <- qs
    if (pb - pa >= 2L) {
      pidx <- pa + which.max(abs(x[(pa + 1L):pb] - ref)) - 1L
      pamp <- c(pamp, x[pidx + 1L] - ref)
      pmag <- max(abs(x[pidx + 1L] - ref), 1e-12)
      ps <- pidx
      pe <- pidx
      while (ps > pa && abs(x[ps + 1L] - ref) > 0.1 * pmag) ps <- ps - 1L
      while (pe < pb - 1L && abs(x[pe + 1L] - ref) > 0.1 * pmag) pe <- pe + 1L
      pdur <- c(pdur, (pe - ps) / fs)
      prdur <- c(prdur, (qs - ps) / fs)
    }

    ta <- min(n - 1L, pos + twinlo)
    tb <- min(n, pos + twinhi)
    if (tb - ta >= 2L) {
      ti <- ta + which.max(abs(x[(ta + 1L):tb] - ref)) - 1L
      tamp <- c(tamp, x[ti + 1L] - ref)
      tmag <- max(abs(x[ti + 1L] - ref), 1e-12)
      ts <- ti
      te <- ti
      while (ts > ta && abs(x[ts + 1L] - ref) > 0.1 * tmag) ts <- ts - 1L
      while (te < tb - 1L && abs(x[te + 1L] - ref) > 0.1 * tmag) te <- te + 1L
      tdur <- c(tdur, (te - ts) / fs)
      qtdur <- c(qtdur, (te - qs) / fs)
    }
  }
  list(
    pamp = pamp, qamp = qamp, ramp = ramp, samp = samp, tamp = tamp,
    qrsdur = qrsdur, pdur = pdur, tdur = tdur,
    prdur = prdur, qtdur = qtdur,
    qrsdurmean = .morie_qrs_mean(qrsdur), prdurmean = .morie_qrs_mean(prdur),
    qtdurmean = .morie_qrs_mean(qtdur), rampmean = .morie_qrs_mean(ramp),
    nbeats = length(q), fs = fs,
    method = paste0(
      "ECG wave amplitudes and durations against the PQ ",
      "isoelectric reference, Rangayyan (2024) Section 1.2.4"
    )
  )
}


#' EcgWaveShp
#'
#' A step of the rangayyan_qrs implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param qrsdur Numeric; combined arithmetically in the body.
#' @param stdev Coerced to numeric by the body, with \code{as.numeric}.
#' @param rdur Optional; may be \code{NULL}. Coerced to numeric by the body, with \code{as.numeric}.
#' @param sdur Optional; may be \code{NULL}. Coerced to numeric by the body, with \code{as.numeric}.
#' @param qpresent Optional; may be \code{NULL}. Coerced to logical by the body, with \code{as.logical}.
#' @return A list with \code{qrsdurms}, \code{qrswide}, \code{lbbbdur}, \code{rbbbdur}, \code{sdurok}, \code{rdurok}, \code{qabsent}, \code{stdev}, \code{stfinding}, \code{required}, \code{method}.
#' @export
EcgWaveShp <- function(qrsdur, stdev, rdur = NULL, sdur = NULL,
                       qpresent = NULL) {
  # Sections 1.2.4 (ST deviation against the PQ reference) and 10.2.1 (the
  # incomplete LBBB/RBBB duration rules).  Only the duration-based parts of
  # the book's rules are computable from one measured QRS duration; the
  # lead-specific conditions are RETURNED as still-required checks rather
  # than silently assumed.
  qrsdur <- as.numeric(qrsdur)
  stdev <- as.numeric(stdev)
  if (!(qrsdur > 0)) stop("qrsdur must be positive")
  ms <- qrsdur * 1000
  st <- "isoelectric"
  if (stdev >= 0.1) st <- "elevated" else if (stdev <= -0.1) st <- "depressed"
  req <- character(0)
  if (ms >= 105 && ms <= 120) {
    req <- c(req, paste0(
      "LBBB also needs: negative QRS in V1,V2; Q or S >= ",
      "80 ms in V1,V2; no Q in any two of I,V5,V6; R > 60 ",
      "ms in any two of I,aVL,V5,V6"
    ))
  }
  if (ms >= 91 && ms <= 120) {
    req <- c(req, paste0(
      "RBBB also needs: S >= 40 ms in any two of ",
      "I,aVL,V4,V5,V6; and in V1 or V2 either R (or R') > ",
      "30 ms with amplitude > 100 uV and no S (or S')"
    ))
  }
  list(
    qrsdurms = ms,
    qrswide = ms > 120,
    lbbbdur = ms >= 105 && ms <= 120,
    rbbbdur = ms >= 91 && ms <= 120,
    sdurok = if (is.null(sdur)) NULL else as.numeric(sdur) * 1000 >= 40,
    rdurok = if (is.null(rdur)) NULL else as.numeric(rdur) * 1000 > 60,
    qabsent = if (is.null(qpresent)) NULL else !isTRUE(as.logical(qpresent)),
    stdev = stdev, stfinding = st, required = req,
    method = paste0(
      "ECG waveshape rules for ischemia and bundle-branch ",
      "block, Rangayyan (2024) Sections 1.2.4 and 10.2.1"
    )
  )
}


#' ST level at J + jofs relative to the PQ isoelectric level, and the
#' slope
#'
#' of the segment that follows (Section 1.2.4).  The 0.1 mV threshold is
#' the conventional clinical figure, NOT a book value -- the payload
#' says so.
#'
#' @param x A vector; its length is taken and its elements indexed.
#' @param qrs Coerced to integer by the body, with \code{as.integer}.
#' @param fs Numeric; combined arithmetically in the body.
#' @param jofs Numeric; combined arithmetically in the body. Defaults to \code{0.06}.
#' @param thresh Defaults to \code{0.1}.
#' @return A list with \code{stdev}, \code{stslope}, \code{pattern}, \code{stdevmean}, \code{stslopemean}, \code{flagged}, \code{thresh}, \code{threshnote}, \code{jofs}, \code{fs}, \code{method}.
#' @export
ExerEcgSt <- function(x, qrs, fs, jofs = 0.060, thresh = 0.1) {
  # ST level at J + jofs relative to the PQ isoelectric level, and the slope
  # of the segment that follows (Section 1.2.4).  The 0.1 mV threshold is the
  # conventional clinical figure, NOT a book value -- the payload says so.
  fs <- .morie_qrs_fs(fs)
  x <- .morie_qrs_check(x, 16L, "ECG")
  q <- as.integer(qrs)
  if (!length(q)) stop("need at least one QRS position")
  jofs <- as.numeric(jofs)
  if (!(jofs > 0)) stop("jofs must be positive")
  n <- length(x)
  pqa <- max(1L, as.integer(round(0.080 * fs)))
  pqb <- max(1L, as.integer(round(0.040 * fs)))
  jpt <- max(1L, as.integer(round(0.040 * fs)))
  off <- max(1L, as.integer(round(jofs * fs)))
  span <- max(2L, as.integer(round(0.040 * fs)))

  dev <- numeric(0)
  slope <- numeric(0)
  pattern <- character(0)
  for (pos in q) {
    a <- max(0L, pos - pqa)
    b <- max(a + 1L, pos - pqb)
    ref <- .morie_fsum(x[(a + 1L):b]) / (b - a)
    m <- pos + jpt + off
    if (m + span >= n) next
    lvl <- x[m + 1L] - ref
    sl <- (x[m + span + 1L] - x[m + 1L]) * fs / span
    dev <- c(dev, lvl)
    slope <- c(slope, sl)
    pattern <- c(pattern, if (sl > 0.05) {
      "upsloping"
    } else if (sl < -0.05) "downsloping" else "horizontal")
  }
  if (!length(dev)) {
    stop("no beat had enough samples after the J point for an ST measurement")
  }
  list(
    stdev = dev, stslope = slope, pattern = pattern,
    stdevmean = .morie_fsum(dev) / length(dev),
    stslopemean = .morie_fsum(slope) / length(slope),
    flagged = sum(abs(dev) >= thresh),
    thresh = thresh,
    threshnote = paste0(
      "0.1 mV is a conventional clinical figure, not ",
      "from Rangayyan (2024); no primary source ",
      "verified here"
    ),
    jofs = jofs, fs = fs,
    method = paste0(
      "ST level and slope against the PQ isoelectric ",
      "reference, Rangayyan (2024) Section 1.2.4"
    )
  )
}


#' Section 8.12.  Task Force bands by default; Bianchi et al. bands on
#'
#' request.  The RR series is a series of EVENTS, so it is interpolated
#' onto a uniform grid before the periodogram; the mean is removed first
#' so DC does not leak into VLF.
#'
#' @param rr A vector; its length is taken and its elements indexed.
#' @param fsr Numeric; combined arithmetically in the body. Defaults to \code{4}.
#' @param bands Defaults to \code{"taskforce"}.
#' @return A list with \code{vlf}, \code{lf}, \code{hf}, \code{total}, \code{vlfpct}, \code{lfpct}, \code{hfpct}, \code{lfhf}, \code{bands}, \code{limits}, \code{fsr}, \code{n}, \code{method}.
#' @export
HrvFreq <- function(rr, fsr = 4, bands = "taskforce") {
  # Section 8.12.  Task Force bands by default; Bianchi et al. bands on
  # request.  The RR series is a series of EVENTS, so it is interpolated onto
  # a uniform grid before the periodogram; the mean is removed first so DC
  # does not leak into VLF.
  rr <- .morie_qrs_check(rr, 8L, "RR series")
  if (any(rr <= 0)) stop("RR intervals must be positive")
  fsr <- .morie_qrs_fs(fsr)
  if (identical(bands, "taskforce")) {
    lim <- list(vlf = c(0, 0.04), lf = c(0.04, 0.15), hf = c(0.15, 0.4))
  } else if (identical(bands, "bianchi")) {
    lim <- list(vlf = c(0, 0.03), lf = c(0.03, 0.15), hf = c(0.18, 0.4))
  } else {
    stop("bands must be 'taskforce' or 'bianchi'")
  }
  tt <- cumsum(rr)
  total <- tt[length(tt)]
  m <- as.integer(trunc(total * fsr))
  if (m < 8L) stop("RR series too short for spectral analysis at this fsr")
  grid <- numeric(m)
  j <- 0L
  for (k in seq_len(m) - 1L) {
    tk <- k / fsr
    while (j < length(tt) - 1L && tt[j + 2L] < tk) j <- j + 1L
    if (j >= length(tt) - 1L) {
      grid[k + 1L] <- rr[length(rr)]
      next
    }
    t0 <- tt[j + 1L]
    t1 <- tt[j + 2L]
    w <- if (t1 <= t0) 0 else (tk - t0) / (t1 - t0)
    w <- min(1, max(0, w))
    grid[k + 1L] <- rr[j + 1L] * (1 - w) + rr[j + 2L] * w
  }
  mu <- .morie_fsum(grid) / length(grid)
  grid <- grid - mu
  sp <- .morie_qrs_psd(grid, fsr)
  df <- if (length(sp$freqs) > 1L) sp$freqs[2L] - sp$freqs[1L] else 0
  band <- function(b) {
    sel <- sp$freqs > b[1L] & sp$freqs <= b[2L]
    if (!any(sel)) 0 else .morie_fsum(sp$power[sel] * df)
  }
  vlf <- band(lim$vlf)
  lf <- band(lim$lf)
  hf <- band(lim$hf)
  tot <- vlf + lf + hf
  pct <- function(v) if (tot > 0) 100 * v / tot else 0
  list(
    vlf = vlf, lf = lf, hf = hf, total = tot,
    vlfpct = pct(vlf), lfpct = pct(lf), hfpct = pct(hf),
    lfhf = if (hf > 0) lf / hf else NULL,
    bands = bands, limits = lim, fsr = fsr, n = length(rr),
    method = paste0(
      "frequency-domain HRV bands, Rangayyan (2024) Section ",
      "8.12 (Bianchi et al. bands, and the Task Force bands ",
      "quoted there)"
    )
  )
}


#' SDNN, RMSSD and pNN50.  Section 2.2.5 motivates these but does not
#' define
#'
#' them; the definitions are the Task Force ones, Circulation
#' 93(5):1043- 1065, 1996, reference [84] of the book\'s Chapter 8.
#'
#' @param rr Numeric; combined arithmetically in the body.
#' @return A list with \code{sdnn}, \code{rmssd}, \code{nn50}, \code{pnn50}, \code{meannn}, \code{meanhr}, \code{n}, \code{units}, \code{method}.
#' @export
HrvTime <- function(rr) {
  # SDNN, RMSSD and pNN50.  Section 2.2.5 motivates these but does not define
  # them; the definitions are the Task Force ones, Circulation 93(5):1043-
  # 1065, 1996, reference [84] of the book's Chapter 8.
  rr <- .morie_qrs_check(rr, 2L, "RR series")
  if (any(rr <= 0)) stop("RR intervals must be positive")
  ms <- rr * 1000
  n <- length(ms)
  mu <- .morie_fsum(ms) / n
  sdnn <- sqrt(.morie_fsum((ms - mu)^2) / (n - 1L))
  d <- diff(ms)
  rmssd <- sqrt(.morie_fsum(d * d) / length(d))
  nn50 <- sum(abs(d) > 50)
  list(
    sdnn = sdnn, rmssd = rmssd, nn50 = nn50,
    pnn50 = 100 * nn50 / length(d),
    meannn = mu, meanhr = 60000 / mu, n = n, units = "ms",
    method = paste0(
      "time-domain HRV (SDNN, RMSSD, pNN50), Task Force of ",
      "the ESC and NASPE, Circulation 93(5):1043-1065, 1996; ",
      "Rangayyan (2024) Section 2.2.5 motivates but does not ",
      "define these"
    )
  )
}


#' Section 4.9.  S1 at the QRS onset; S2 from the dicrotic notch minus
#' the
#'
#' standardised 52.6 ms (mean + 2 SD of the 42.6 +/- 5 ms Lehner and
#' Rangayyan measurement).  The notch lags the sound because it is the
#' same aortic-valve closure observed after propagation up the arterial
#' tree.
#'
#' @param ecg A vector; its length is taken.
#' @param cp A vector; its length is taken.
#' @param fs Numeric; combined arithmetically in the body.
#' @return A list with \code{s1}, \code{s2}, \code{notch}, \code{qrs}, \code{s2delayms}, \code{s2delaymeasured}, \code{searchwindowms}, \code{fs}, \code{method}.
#' @export
HSoundId <- function(ecg, cp, fs) {
  # Section 4.9.  S1 at the QRS onset; S2 from the dicrotic notch minus the
  # standardised 52.6 ms (mean + 2 SD of the 42.6 +/- 5 ms Lehner and
  # Rangayyan measurement).  The notch lags the sound because it is the same
  # aortic-valve closure observed after propagation up the arterial tree.
  fs <- .morie_qrs_fs(fs)
  ecg <- .morie_qrs_check(ecg, 40L, "ECG")
  cp <- .morie_qrs_check(cp, 40L, "carotid pulse")
  if (length(ecg) != length(cp)) {
    stop("ECG and carotid pulse must have the same length")
  }
  q <- QrsDetect(ecg, fs)$qrs
  if (!length(q)) stop("no QRS complexes detected; cannot place S1 or S2")
  notch <- DicNotch(cp, fs, qrs = q)$notch
  lag <- as.integer(round(0.0526 * fs))
  s2 <- pmax(0L, notch - lag)
  list(
    s1 = q, s2 = s2, notch = notch, qrs = q,
    s2delayms = 52.6, s2delaymeasured = c(42.6, 5), searchwindowms = 500,
    fs = fs,
    method = paste0(
      "S1/S2 identification from ECG and carotid pulse ",
      "timing, Rangayyan (2024) Section 4.9 (Lehner and ",
      "Rangayyan)"
    )
  )
}


#' Sections 3.3.5 and 9.7.2: the thoracic lead is the reference input of
#' an
#'
#' adaptive noise canceller whose primary input is the abdominal lead,
#' so the residual error is the fetal ECG.  Normalised LMS, weights
#' start at zero, no random initialisation.
#'
#' @param abd A vector; its length is taken and its elements indexed.
#' @param thor A vector; its length is taken and its elements indexed.
#' @param order A count; the body uses it as \code{seq_len(...)}. Defaults to \code{16}.
#' @param mu Numeric; combined arithmetically in the body. Defaults to \code{0.01}.
#' @return A list with \code{fetal}, \code{maternal}, \code{weights}, \code{order}, \code{mu}, \code{n}, \code{method}.
#' @export
MEcgFilt <- function(abd, thor, order = 16, mu = 0.01) {
  # Sections 3.3.5 and 9.7.2: the thoracic lead is the reference input of an
  # adaptive noise canceller whose primary input is the abdominal lead, so the
  # residual error is the fetal ECG.  Normalised LMS, weights start at zero,
  # no random initialisation.
  abd <- .morie_qrs_check(abd, 4L, "abdominal ECG")
  thor <- .morie_qrs_check(thor, 4L, "thoracic ECG")
  if (length(abd) != length(thor)) {
    stop("abdominal and thoracic signals must have the same length")
  }
  order <- as.integer(order)
  if (order < 1L) stop("order must be >= 1")
  if (length(abd) <= order) stop("signals must be longer than the filter order")
  mu <- as.numeric(mu)
  if (!(mu > 0 && mu < 2)) stop("mu must lie in (0, 2)")
  n <- length(abd)
  # Regularise by a fraction of the reference's own mean power: a fixed tiny
  # constant makes the weights diverge on the quiet stretches between
  # maternal complexes, which is exactly where the fetal ECG lives.
  eps <- 1e-3 * order * (.morie_fsum(thor * thor) / n)
  if (eps <= 0) stop("thoracic reference is identically zero")
  w <- numeric(order)
  est <- numeric(n)
  err <- numeric(n)
  for (i in seq_len(n) - 1L) {
    kk <- seq_len(order) - 1L
    xv <- ifelse(i - kk >= 0L, thor[pmax(1L, i - kk + 1L)], 0)
    yv <- .morie_fsum(w * xv)
    e <- abd[i + 1L] - yv
    est[i + 1L] <- yv
    err[i + 1L] <- e
    p <- .morie_fsum(xv * xv)
    g <- mu / (p + eps)
    w <- w + g * e * xv
  }
  list(
    fetal = err, maternal = est, weights = w,
    order = order, mu = mu, n = n,
    method = paste0(
      "adaptive cancellation of the maternal ECG with a ",
      "thoracic reference, Rangayyan (2024) Sections 3.3.5 ",
      "and 9.7.2 (normalised LMS)"
    )
  )
}


#' Section 1.2.11 characterises motion artifact but gives no detection
#'
#' equation, so none is attributed to the book.  Window range and window
#' activity are both compared against their MEDIANS, because the mean is
#' dragged upward by the very segments being detected.
#'
#' @param x A vector; its length is taken and its elements indexed.
#' @param fs Numeric; combined arithmetically in the body.
#' @param win Numeric; combined arithmetically in the body. Defaults to \code{1}.
#' @param factor Numeric; combined arithmetically in the body. Defaults to \code{4}.
#' @return A list with \code{clean}, \code{artifact}, \code{nsegments}, \code{fraction}, \code{win}, \code{factor}, \code{fs}, \code{method}.
#' @export
MotionArt <- function(x, fs, win = 1, factor = 4) {
  # Section 1.2.11 characterises motion artifact but gives no detection
  # equation, so none is attributed to the book.  Window range and window
  # activity are both compared against their MEDIANS, because the mean is
  # dragged upward by the very segments being detected.
  fs <- .morie_qrs_fs(fs)
  x <- .morie_qrs_check(x, 16L, "signal")
  win <- as.numeric(win)
  factor <- as.numeric(factor)
  if (!(win > 0)) stop("win must be positive")
  if (!(factor > 1)) stop("factor must be greater than 1")
  w <- max(4L, as.integer(round(win * fs)))
  n <- length(x)
  starts <- seq(0L, n - 1L, by = w)
  rng <- numeric(length(starts))
  act <- numeric(length(starts))
  for (si in seq_along(starts)) {
    a <- starts[si]
    seg <- x[(a + 1L):min(n, a + w)]
    rng[si] <- max(seg) - min(seg)
    act[si] <- if (length(seg) > 1L) {
      .morie_fsum(abs(diff(seg))) / max(1L, length(seg) - 1L)
    } else {
      0
    }
  }
  mr <- .morie_qrs_median(rng)
  ma <- .morie_qrs_median(act)
  bad <- (rng > factor * mr & mr > 0) | (act > factor * ma & ma > 0)

  segs <- list()
  i <- 1L
  while (i <= length(starts)) {
    if (bad[i]) {
      j <- i
      while (j <= length(starts) && bad[j]) j <- j + 1L
      segs[[length(segs) + 1L]] <- c(starts[i], min(n, starts[j - 1L] + w))
      i <- j
    } else {
      i <- i + 1L
    }
  }
  clean <- x
  for (sg in segs) {
    a <- sg[1L]
    b <- sg[2L]
    lv <- if (a > 0L) x[a] else (if (b < n) x[b + 1L] else 0)
    rv <- if (b < n) x[b + 1L] else lv
    span <- max(1L, b - a)
    kk <- a
    while (kk < min(n, b)) {
      t <- (kk - a + 1L) / (span + 1)
      clean[kk + 1L] <- lv * (1 - t) + rv * t
      kk <- kk + 1L
    }
  }
  flagged <- if (length(segs)) {
    .morie_fsum(vapply(segs, function(s) min(n, s[2L]) - s[1L], numeric(1)))
  } else {
    0
  }
  list(
    clean = clean, artifact = segs, nsegments = length(segs),
    fraction = flagged / n, win = win, factor = factor, fs = fs,
    method = paste0(
      "motion-artifact detection by window range and ",
      "activity against their medians; Rangayyan (2024) ",
      "Section 1.2.11 characterises the artifact but gives ",
      "no detection equation"
    )
  )
}


#' Pan-Tompkins, Section 4.3.2, eqs (4.8)-(4.18) plus search-back.  The
#'
#' filter coefficients are integers designed for fs = 200 Hz and DO NOT
#' rescale; the timing constants (150 ms integrator, 200 ms refractory)
#' do.
#'
#' @param x A vector; its length is taken.
#' @param fs Numeric; combined arithmetically in the body. Defaults to \code{200}.
#' @return A list with \code{qrs}, \code{rr}, \code{hr}, \code{integrated}, \code{bandpass}, \code{delay}, \code{spki}, \code{npki}, \code{thresh1}, \code{thresh2}, \code{searchback}, \code{fs}, \code{fsnote}, \code{method}.
#' @export
QrsDetect <- function(x, fs = 200) {
  # Pan-Tompkins, Section 4.3.2, eqs (4.8)-(4.18) plus search-back.  The
  # filter coefficients are integers designed for fs = 200 Hz and DO NOT
  # rescale; the timing constants (150 ms integrator, 200 ms refractory) do.
  fs <- .morie_qrs_fs(fs)
  x <- .morie_qrs_check(x, 40L, "ECG")
  chain <- .morie_qrs_chain(x, fs)
  ig <- chain$ig
  wint <- chain$w
  n <- length(ig)
  # cumulative group delay: 5 samples (eq 4.8 at 200 Hz), 16 for the allpass
  # branch of eq 4.13, 2 for the derivative, half the integrator window.
  delay <- as.integer(round(5 * fs / 200)) + as.integer(round(16 * fs / 200)) +
    2L + wint %/% 2L
  refrac <- max(1L, as.integer(round(0.200 * fs)))

  idx <- 1:(n - 2L)
  cand <- idx[ig[idx + 1L] > ig[idx] & ig[idx + 1L] >= ig[idx + 2L]]

  learn <- min(n, max(2L * wint, as.integer(round(2 * fs))))
  seg <- ig[seq_len(learn)]
  spki <- max(seg) / 3
  npki <- (.morie_fsum(seg) / length(seg)) / 2
  t1 <- npki + 0.25 * (spki - npki)
  t2 <- 0.5 * t1

  qrs <- integer(0)
  peaks <- numeric(0)
  rr1 <- numeric(0)
  rr2 <- numeric(0)
  rrave2 <- NULL
  searchback <- 0L

  for (i in cand) {
    pk <- ig[i + 1L]
    nq <- length(qrs)
    if (nq && (i - qrs[nq]) < refrac) {
      if (pk > peaks[nq]) {
        qrs[nq] <- i
        peaks[nq] <- pk
      }
      next
    }
    lo <- NULL
    hi <- NULL
    missed <- NULL
    if (!is.null(rrave2)) {
      lo <- 0.92 * rrave2
      hi <- 1.16 * rrave2
      missed <- 1.66 * rrave2
    }
    if (nq && !is.null(missed) && (i - qrs[nq]) / fs > missed) {
      wsel <- cand[cand >= qrs[nq] + refrac & cand < i & ig[cand + 1L] > t2]
      if (length(wsel)) {
        best <- wsel[which.max(ig[wsel + 1L])]
        qrs <- c(qrs, best)
        peaks <- c(peaks, ig[best + 1L])
        spki <- 0.25 * ig[best + 1L] + 0.75 * spki # eq (4.18)
        searchback <- searchback + 1L
        t1 <- npki + 0.25 * (spki - npki)
        t2 <- 0.5 * t1
      }
    }
    if (pk > t1) {
      qrs <- c(qrs, i)
      peaks <- c(peaks, pk)
      spki <- 0.125 * pk + 0.875 * spki # eq (4.16)
      if (length(qrs) > 1L) {
        m <- length(qrs)
        rr <- (qrs[m] - qrs[m - 1L]) / fs
        rr1 <- c(rr1, rr)
        if (length(rr1) > 8L) rr1 <- rr1[-1L]
        if (is.null(lo) || (rr >= lo && rr <= hi)) {
          rr2 <- c(rr2, rr)
          if (length(rr2) > 8L) rr2 <- rr2[-1L]
        }
        if (length(rr2)) {
          rrave2 <- .morie_fsum(rr2) / length(rr2)
        } else if (length(rr1)) rrave2 <- .morie_fsum(rr1) / length(rr1)
      }
    } else {
      npki <- 0.125 * pk + 0.875 * npki # eq (4.16)
    }
    t1 <- npki + 0.25 * (spki - npki) # eq (4.17)
    t2 <- 0.5 * t1
  }
  loc <- sort(unique(pmax(0L, qrs - delay)))
  rr <- if (length(loc) > 1L) diff(loc) / fs else numeric(0)
  hr <- if (length(loc)) 60 * length(loc) / (length(x) / fs) else 0
  list(
    qrs = loc, rr = rr, hr = hr, integrated = ig, bandpass = chain$bp,
    delay = delay, spki = spki, npki = npki, thresh1 = t1, thresh2 = t2,
    searchback = searchback, fs = fs,
    fsnote = paste0(
      "filter coefficients are integers fixed for fs = 200 ",
      "Hz; timing constants scale with fs"
    ),
    method = paste0(
      "Pan-Tompkins QRS detection, Rangayyan (2024) Section ",
      "4.3.2, Eqs 4.8-4.18; Pan and Tompkins, IEEE TBME ",
      "32(3):230-236, 1985"
    )
  )
}


#' The six-step procedure of Section 4.9.  Timing is imported from the
#' ECG
#'
#' and the carotid pulse because S1 and S2 are not reliably the loudest
#' events in a PCG once murmurs are present.
#'
#' @param pcg A vector; its length is taken and its elements indexed.
#' @param ecg A vector; its length is taken.
#' @param cp A vector; its length is taken.
#' @param fs Passed to \code{.morie_qrs_fs}.
#' @return A list with \code{s1}, \code{s2}, \code{systole}, \code{diastole}, \code{systolerms}, \code{diastolerms}, \code{fs}, \code{method}.
#' @export
PcgParts <- function(pcg, ecg, cp, fs) {
  # The six-step procedure of Section 4.9.  Timing is imported from the ECG
  # and the carotid pulse because S1 and S2 are not reliably the loudest
  # events in a PCG once murmurs are present.
  fs <- .morie_qrs_fs(fs)
  pcg <- .morie_qrs_check(pcg, 40L, "PCG")
  ecg <- .morie_qrs_check(ecg, 40L, "ECG")
  cp <- .morie_qrs_check(cp, 40L, "carotid pulse")
  if (!(length(pcg) == length(ecg) && length(ecg) == length(cp))) {
    stop("PCG, ECG and carotid pulse must have the same length")
  }
  ids <- HSoundId(ecg, cp, fs)
  s1 <- ids$s1
  s2 <- ids$s2
  if (!length(s1) || !length(s2)) {
    stop("could not locate both S1 and S2 events")
  }
  rms <- function(a, b) {
    if (b > a) {
      sqrt(.morie_fsum(pcg[(a + 1L):b]^2) / (b - a))
    } else {
      0
    }
  }
  systole <- list()
  diastole <- list()
  for (a in s1) {
    later <- s2[s2 > a]
    if (!length(later)) next
    b <- later[1L]
    systole[[length(systole) + 1L]] <- c(a, b)
    nxt <- s1[s1 > b]
    if (length(nxt)) diastole[[length(diastole) + 1L]] <- c(b, nxt[1L])
  }
  list(
    s1 = s1, s2 = s2, systole = systole, diastole = diastole,
    systolerms = vapply(systole, function(p) rms(p[1L], p[2L]), numeric(1)),
    diastolerms = vapply(diastole, function(p) rms(p[1L], p[2L]), numeric(1)),
    fs = fs,
    method = paste0(
      "PCG segmentation into systole and diastole, ",
      "Rangayyan (2024) Section 4.9"
    )
  )
}


#' Eq (3.150): H(z) = 1 - 2 cos(wo) z^-1 + z^-2, divided by its DC gain
#'
#' H(1) = 2 - 2 cos(wo) so the passband gain at DC is unity.  Extra
#' zeros at n fo / fs make it a comb for the harmonics of a
#' non-sinusoidal mains waveform.
#'
#' @param x Passed to \code{.morie_qrs_check}.
#' @param fs Numeric; combined arithmetically in the body.
#' @param f0 Numeric; combined arithmetically in the body. Defaults to \code{60}.
#' @param harmonics A count; the body uses it as \code{seq_len(...)}. Defaults to \code{1}.
#' @return A list with \code{y}, \code{coeffs}, \code{notched}, \code{f0}, \code{fs}, \code{n}, \code{method}.
#' @export
PLineNotch <- function(x, fs, f0 = 60, harmonics = 1) {
  # eq (3.150): H(z) = 1 - 2 cos(wo) z^-1 + z^-2, divided by its DC gain
  # H(1) = 2 - 2 cos(wo) so the passband gain at DC is unity.  Extra zeros at
  # n fo / fs make it a comb for the harmonics of a non-sinusoidal mains
  # waveform.
  fs <- .morie_qrs_fs(fs)
  x <- .morie_qrs_check(x, 3L, "signal")
  f0 <- as.numeric(f0)
  harmonics <- as.integer(harmonics)
  if (!(f0 > 0)) stop("f0 must be positive")
  if (harmonics < 1L) stop("harmonics must be >= 1")
  y <- x
  coeffs <- list()
  notched <- numeric(0)
  for (h in seq_len(harmonics)) {
    f <- h * f0
    if (f >= fs / 2) break
    w <- 2 * pi * f / fs
    b1 <- -2 * cos(w)
    gain <- 1 + b1 + 1
    if (abs(gain) < 1e-12) {
      stop(sprintf("notch at %g Hz has zero DC gain; choose another frequency", f))
    }
    b <- c(1 / gain, b1 / gain, 1 / gain)
    coeffs[[length(coeffs) + 1L]] <- b
    notched <- c(notched, f)
    z <- numeric(length(y))
    for (i in seq_len(length(y)) - 1L) {
      z[i + 1L] <- b[1L] * y[i + 1L] +
        b[2L] * (if (i >= 1L) y[i] else 0) +
        b[3L] * (if (i >= 2L) y[i - 1L] else 0)
    }
    y <- z
  }
  if (!length(notched)) {
    stop("f0 is at or above the Nyquist frequency; nothing to notch")
  }
  list(
    y = y, coeffs = coeffs, notched = notched, f0 = f0, fs = fs,
    n = length(y),
    method = "unit-circle comb notch filter, Rangayyan (2024) Eq 3.150"
  )
}


#' Section 1.2.11.  The notch is located with the SAME machinery the
#' book
#'
#' gives for the carotid pulse in Section 4.3.5 (eqs 4.22, 4.23),
#' because the PPG pulse shape mirrors the carotid pulse.  The perfusion
#' index says how much of the reading is pulsatile blood volume rather
#' than static tissue absorption.
#'
#' @param ppg A vector; its length is taken and its elements indexed.
#' @param fs Numeric; combined arithmetically in the body.
#' @param mwin Defaults to \code{16}.
#' @return A list with \code{systolic}, \code{notch}, \code{diastolic}, \code{onset}, \code{amplitude}, \code{ac}, \code{dc}, \code{pi}, \code{rate}, \code{fs}, \code{method}.
#' @export
PpgFeat <- function(ppg, fs, mwin = 16) {
  # Section 1.2.11.  The notch is located with the SAME machinery the book
  # gives for the carotid pulse in Section 4.3.5 (eqs 4.22, 4.23), because
  # the PPG pulse shape mirrors the carotid pulse.  The perfusion index says
  # how much of the reading is pulsatile blood volume rather than static
  # tissue absorption.
  fs <- .morie_qrs_fs(fs)
  ppg <- .morie_qrs_check(ppg, 32L, "PPG")
  notch <- DicNotch(ppg, fs, mwin = mwin)$notch
  if (!length(notch)) {
    stop("no dicrotic notch found; cannot delineate PPG pulses")
  }
  n <- length(ppg)
  back <- max(2L, as.integer(round(0.400 * fs)))
  sysp <- integer(0)
  diap <- integer(0)
  feet <- integer(0)
  amp <- numeric(0)
  for (d in notch) {
    a <- max(0L, d - back)
    if (d - a < 3L) next
    s <- .morie_qrs_argmax(ppg, a, d)
    if (s <= a) next
    f <- .morie_qrs_argmin(ppg, a, s)
    sysp <- c(sysp, s)
    feet <- c(feet, f)
    hi <- min(n, d + as.integer(round(0.250 * fs)))
    diap <- c(diap, if (hi > d + 1L) .morie_qrs_argmax(ppg, d, hi) else d)
    amp <- c(amp, ppg[s + 1L] - ppg[f + 1L])
  }
  if (!length(amp)) stop("could not delineate any complete PPG pulse")
  dc <- .morie_fsum(ppg) / n
  ac <- .morie_fsum(amp) / length(amp)
  list(
    systolic = sysp, notch = notch, diastolic = diap, onset = feet,
    amplitude = amp, ac = ac, dc = dc,
    pi = if (dc != 0) 100 * ac / dc else NULL,
    rate = 60 * length(sysp) / (n / fs), fs = fs,
    method = paste0(
      "PPG pulse features with the carotid dicrotic-notch ",
      "operator, Rangayyan (2024) Sections 1.2.11 and 4.3.5 ",
      "(Eqs 4.22, 4.23)"
    )
  )
}


#' Hengeveld and van Bemmel, Section 4.3.3.  The P wave is never
#' searched for
#'
#' directly: the QRS is deleted, the residue bandpassed 3-11 Hz,
#' ternarised at 50% and 75% of the interval maximum, and matched
#' against a ternary template.  Matching a TERNARY version is what makes
#' it tolerant of P-wave amplitude and shape variation.
#'
#' @param x A vector; its length is taken and its elements indexed.
#' @param qrs Coerced to integer by the body, with \code{as.integer}.
#' @param fs Numeric; combined arithmetically in the body.
#' @param template Optional; may be \code{NULL}. A vector; its length is taken.
#' @return A list with \code{p}, \code{template}, \code{windows}, \code{bandpass}, \code{fs}, \code{method}.
#' @export
PWaveDet <- function(x, qrs, fs, template = NULL) {
  # Hengeveld and van Bemmel, Section 4.3.3.  The P wave is never searched for
  # directly: the QRS is deleted, the residue bandpassed 3-11 Hz, ternarised
  # at 50% and 75% of the interval maximum, and matched against a ternary
  # template.  Matching a TERNARY version is what makes it tolerant of P-wave
  # amplitude and shape variation.
  fs <- .morie_qrs_fs(fs)
  x <- .morie_qrs_check(x, 16L, "ECG")
  q <- as.integer(qrs)
  if (length(q) < 2L) {
    stop("need at least two QRS positions to define an RR interval")
  }
  n <- length(x)
  half <- max(1L, as.integer(round(0.050 * fs)))
  base <- max(1L, as.integer(round(0.040 * fs)))
  y <- x
  for (pos in q) {
    a <- max(0L, pos - half)
    b <- min(n, pos + half)
    ra <- max(0L, a - base)
    lvl <- if (ra < a) .morie_fsum(x[(ra + 1L):a]) / (a - ra) else 0
    if (a < b) y[(a + 1L):b] <- lvl
  }
  lo <- .morie_qrs_mavg(y, max(1L, as.integer(round(fs / 11))))
  hi <- .morie_qrs_mavg(y, max(1L, as.integer(round(fs / 3))))
  bp <- lo - hi

  wins <- vector("list", length(q) - 1L)
  for (k in seq_len(length(q) - 1L) + 1L) {
    rr <- (q[k] - q[k - 1L]) / fs
    qtmax <- 29 * rr + 0.250
    start <- max(
      q[k - 1L],
      q[k - 1L] + as.integer(round(min(qtmax, 0.75 * rr) * fs))
    )
    stop_ <- max(start + 2L, q[k] - half)
    wins[[k - 1L]] <- if (stop_ <= start || stop_ > n) NULL else c(start, stop_)
  }
  usable <- Filter(Negate(is.null), wins)
  if (!length(usable)) {
    stop("no usable P-wave search interval between the supplied QRS positions")
  }
  wlen <- min(vapply(usable, function(w) w[2L] - w[1L], numeric(1)))

  ternary <- function(seg) {
    m <- max(abs(seg))
    if (m <= 0) {
      return(rep(0, length(seg)))
    }
    r <- abs(seg) / m
    ifelse(r >= 0.75, 2, ifelse(r >= 0.50, 1, 0))
  }
  if (is.null(template)) {
    acc <- numeric(wlen)
    for (w in usable) acc <- acc + bp[(w[1L] + 1L):(w[1L] + wlen)]
    template <- ternary(acc / length(usable))
  } else {
    template <- .morie_qrs_check(template, 2L, "template")
  }
  tl <- length(template)

  ppos <- vector("list", length(wins))
  for (wi in seq_along(wins)) {
    w <- wins[[wi]]
    if (is.null(w)) {
      ppos[wi] <- list(NULL)
      next
    }
    a <- w[1L]
    b <- w[2L]
    tern <- ternary(bp[(a + 1L):b])
    if (length(tern) < tl) {
      ppos[wi] <- list(NULL)
      next
    }
    best <- 0L
    bestv <- NULL
    for (s in seq_len(length(tern) - tl + 1L) - 1L) {
      v <- .morie_fsum(tern[(s + 1L):(s + tl)] * template)
      if (is.null(bestv) || v > bestv) {
        best <- s
        bestv <- v
      }
    }
    ppos[[wi]] <- a + best + tl %/% 2L
  }
  list(
    p = ppos, template = template, windows = wins, bandpass = bp, fs = fs,
    method = paste0(
      "P-wave detection, Rangayyan (2024) Section 4.3.3 ",
      "(Hengeveld and van Bemmel)"
    )
  )
}


#' Section 2.2.4 for the physiology; the estimator is Arunachalam and
#' Brown,
#'
#' Proc. IEEE EMBC 2009, pp. 5681-5684 (reference [52] of Chapter 2).
#' Chest motion swings the cardiac electrical axis, so the R amplitude
#' sampled once per beat is a respiratory signal without a respiration
#' sensor.
#'
#' @param x A vector; its length is taken and its elements indexed.
#' @param qrs Coerced to integer by the body, with \code{as.integer}.
#' @param fs Numeric; combined arithmetically in the body.
#' @param fsr Numeric; combined arithmetically in the body. Defaults to \code{4}.
#' @return A list with \code{edr}, \code{amp}, \code{times}, \code{resprate}, \code{fsr}, \code{nbeats}, \code{method}.
#' @export
EdrSignal <- function(x, qrs, fs, fsr = 4) {
  # Section 2.2.4 for the physiology; the estimator is Arunachalam and Brown,
  # Proc. IEEE EMBC 2009, pp. 5681-5684 (reference [52] of Chapter 2).  Chest
  # motion swings the cardiac electrical axis, so the R amplitude sampled once
  # per beat is a respiratory signal without a respiration sensor.
  fs <- .morie_qrs_fs(fs)
  fsr <- .morie_qrs_fs(fsr)
  x <- .morie_qrs_check(x, 16L, "ECG")
  q <- as.integer(qrs)
  if (length(q) < 8L) stop("need at least eight beats to estimate respiration")
  n <- length(x)
  pqa <- max(1L, as.integer(round(0.080 * fs)))
  pqb <- max(1L, as.integer(round(0.040 * fs)))
  half <- max(1L, as.integer(round(0.050 * fs)))
  times <- numeric(0)
  amps <- numeric(0)
  for (pos in q) {
    a <- max(0L, pos - pqa)
    b <- max(a + 1L, pos - pqb)
    ref <- .morie_fsum(x[(a + 1L):b]) / (b - a)
    lo <- max(0L, pos - half)
    hi <- min(n, pos + half + 1L)
    if (hi - lo < 2L) next
    amps <- c(amps, max(x[(lo + 1L):hi]) - ref)
    times <- c(times, pos / fs)
  }
  if (length(amps) < 8L) stop("too few usable beats for an EDR estimate")
  span <- times[length(times)] - times[1L]
  m <- as.integer(trunc(span * fsr))
  if (m < 8L) stop("recording too short for an EDR estimate at this fsr")
  grid <- numeric(m)
  j <- 0L
  for (k in seq_len(m) - 1L) {
    tk <- times[1L] + k / fsr
    while (j < length(times) - 2L && times[j + 2L] < tk) j <- j + 1L
    t0 <- times[j + 1L]
    t1 <- times[j + 2L]
    w <- if (t1 <= t0) 0 else (tk - t0) / (t1 - t0)
    w <- min(1, max(0, w))
    grid[k + 1L] <- amps[j + 1L] * (1 - w) + amps[j + 2L] * w
  }
  mu <- .morie_fsum(grid) / length(grid)
  edr <- grid - mu
  sp <- .morie_qrs_psd(edr, fsr)
  cand <- which(sp$freqs >= 0.1 & sp$freqs <= 0.5)
  rate <- if (length(cand)) 60 * sp$freqs[cand[which.max(sp$power[cand])]] else NULL
  list(
    edr = edr, amp = amps, times = times, resprate = rate, fsr = fsr,
    nbeats = length(amps),
    method = paste0(
      "ECG-derived respiration from R-wave amplitude ",
      "modulation; Rangayyan (2024) Section 2.2.4 for the ",
      "physiology, Arunachalam and Brown, Proc. IEEE EMBC ",
      "2009, pp. 5681-5684, for the estimator"
    )
  )
}


#' Section 10.2.5 frames the problem and the AHI but gives NO detection
#'
#' algorithm, so none is attributed to it.  Requiring BOTH a respiratory
#' pause and an SpO2 desaturation is what keeps a motion-induced flat
#' stretch of EDR from being scored as an apnea.
#'
#' @param edr A vector; its length is taken.
#' @param spo2 A vector; its length is taken and its elements indexed.
#' @param fs Numeric; combined arithmetically in the body.
#' @param hours Optional; may be \code{NULL}. Numeric; combined arithmetically in the body.
#' @param mindur Numeric; combined arithmetically in the body. Defaults to \code{10}.
#' @param desat Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{3}.
#' @return A list with \code{events}, \code{nevents}, \code{ahi}, \code{desatdepth}, \code{hours}, \code{mindur}, \code{desat}, \code{fs}, \code{method}.
#' @export
ApneaEdr <- function(edr, spo2, fs, hours = NULL, mindur = 10, desat = 3) {
  # Section 10.2.5 frames the problem and the AHI but gives NO detection
  # algorithm, so none is attributed to it.  Requiring BOTH a respiratory
  # pause and an SpO2 desaturation is what keeps a motion-induced flat
  # stretch of EDR from being scored as an apnea.
  fs <- .morie_qrs_fs(fs)
  edr <- .morie_qrs_check(edr, 16L, "EDR")
  spo2 <- .morie_qrs_check(spo2, 16L, "SpO2")
  if (length(edr) != length(spo2)) {
    stop("EDR and SpO2 must have the same length and rate")
  }
  mindur <- as.numeric(mindur)
  desat <- as.numeric(desat)
  if (!(mindur > 0)) stop("mindur must be positive")
  if (!(desat > 0)) stop("desat must be positive")
  n <- length(edr)
  if (is.null(hours)) hours <- (n / fs) / 3600
  hours <- as.numeric(hours)
  if (!(hours > 0)) stop("hours must be positive")
  env <- .morie_qrs_mavg(abs(edr), max(1L, as.integer(round(2 * fs))))
  base <- .morie_qrs_mavg(env, max(1L, as.integer(round(120 * fs))))
  low <- env < 0.30 * base
  need <- max(1L, as.integer(round(mindur * fs)))
  look <- max(1L, as.integer(round(30 * fs)))
  events <- list()
  depth <- numeric(0)
  i <- 0L
  while (i < n) {
    if (low[i + 1L]) {
      j <- i
      while (j < n && low[j + 1L]) j <- j + 1L
      if (j - i >= need) {
        pa <- max(0L, i - look)
        pre <- if (pa < i) spo2[(pa + 1L):i] else spo2[i + 1L]
        lvl <- max(pre)
        after <- spo2[(i + 1L):min(n, j + look)]
        drop <- if (length(after)) lvl - min(after) else 0
        if (drop >= desat) {
          events[[length(events) + 1L]] <- c(i, j)
          depth <- c(depth, drop)
        }
      }
      i <- j
    } else {
      i <- i + 1L
    }
  }
  list(
    events = events, nevents = length(events),
    ahi = length(events) / hours, desatdepth = depth, hours = hours,
    mindur = mindur, desat = desat, fs = fs,
    method = paste0(
      "apnea event scoring from respiratory-envelope pauses ",
      "confirmed by SpO2 desaturation; Rangayyan (2024) ",
      "Section 10.2.5 frames the problem and the AHI but ",
      "gives no detection algorithm"
    )
  )
}


#' Section 8.12 and Figure 8.38.  The ratio alone, with the band powers
#' and
#'
#' the RR variance alongside it so it can be interpreted rather than
#' read bare.
#'
#' @param rr Passed to \code{.morie_qrs_check}.
#' @param fsr Defaults to \code{4}.
#' @param bands Defaults to \code{"taskforce"}.
#' @return A list with \code{lfhf}, \code{lf}, \code{hf}, \code{lfpct}, \code{hfpct}, \code{rrvar}, \code{bands}, \code{n}, \code{method}.
#' @export
LfHfRatio <- function(rr, fsr = 4, bands = "taskforce") {
  # Section 8.12 and Figure 8.38.  The ratio alone, with the band powers and
  # the RR variance alongside it so it can be interpreted rather than read
  # bare.
  res <- HrvFreq(rr, fsr = fsr, bands = bands)
  v <- .morie_qrs_check(rr, 2L, "RR series")
  mu <- .morie_fsum(v) / length(v)
  var <- .morie_fsum((v - mu)^2) / (length(v) - 1L)
  list(
    lfhf = res$lfhf, lf = res$lf, hf = res$hf,
    lfpct = res$lfpct, hfpct = res$hfpct, rrvar = var,
    bands = bands, n = length(v),
    method = paste0(
      "LF/HF spectral power ratio, Rangayyan (2024) Section ",
      "8.12 and Figure 8.38 (Bianchi et al.)"
    )
  )
}


#' Section 9.10 citing Smith et al., Circulation 77(1):110-121, 1988
#'
#' Alternation every other beat is a period of exactly two beats, so it
#' lands on 0.5 cycles per beat -- the last bin of the beat-series
#' spectrum. An EVEN beat count is required or that is not an exact bin.
#'
#' @param twaves A vector; its length is taken.
#' @param noiselo Defaults to \code{0.33}.
#' @param noisehi Defaults to \code{0.45}.
#' @return A list with \code{valt}, \code{kscore}, \code{altpower}, \code{noisemean}, \code{noisesd}, \code{nbeats}, \code{npoints}, \code{cyclesperbeat}, \code{present}, \code{method}.
#' @export
TwaSpectr <- function(twaves, noiselo = 0.33, noisehi = 0.45) {
  # Section 9.10 citing Smith et al., Circulation 77(1):110-121, 1988.
  # Alternation every other beat is a period of exactly two beats, so it
  # lands on 0.5 cycles per beat -- the last bin of the beat-series spectrum.
  # An EVEN beat count is required or that is not an exact bin.
  if (!length(twaves)) stop("need at least one T wave")
  if (!is.list(twaves)) twaves <- list(twaves)
  beats <- lapply(twaves, .morie_qrs_check, least = 2L, what = "T wave")
  m <- length(beats)
  if (m < 8L) stop("spectral TWA needs at least eight beats")
  if (m %% 2L) {
    beats <- beats[-m]
    m <- m - 1L
  }
  npts <- length(beats[[1L]])
  if (any(vapply(beats, length, integer(1)) != npts)) {
    stop("all T waves must have the same number of samples")
  }
  if (!(noiselo > 0 && noiselo < noisehi && noisehi < 0.5)) {
    stop("noise band must satisfy 0 < noiselo < noisehi < 0.5")
  }
  acc <- numeric(m %/% 2L + 1L)
  for (j in seq_len(npts)) {
    series <- vapply(beats, function(b) b[j], numeric(1))
    mu <- .morie_fsum(series) / m
    sp <- .morie_qrs_dft(series - mu)
    for (k in seq_len(m %/% 2L + 1L) - 1L) {
      re <- sp$re[k + 1L]
      im <- sp$im[k + 1L]
      p <- (re * re + im * im) / (m * m)
      if (k > 0L && k < m - k) p <- p * 2
      acc[k + 1L] <- acc[k + 1L] + p
    }
  }
  acc <- acc / npts
  cyc <- (seq_len(m %/% 2L + 1L) - 1L) / m
  kalt <- m %/% 2L
  band <- which(cyc >= noiselo & cyc <= noisehi)
  if (length(band) < 2L) {
    stop("noise band contains fewer than two spectral bins; use more beats")
  }
  nm <- .morie_fsum(acc[band]) / length(band)
  nsd <- sqrt(.morie_fsum((acc[band] - nm)^2) / (length(band) - 1L))
  altp <- acc[kalt + 1L]
  valt <- if (altp > nm) sqrt(altp - nm) else 0
  kscore <- if (nsd > 0) (altp - nm) / nsd else NULL
  list(
    valt = valt, kscore = kscore, altpower = altp, noisemean = nm,
    noisesd = nsd, nbeats = m, npoints = npts, cyclesperbeat = 0.5,
    present = !is.null(kscore) && kscore >= 3,
    method = paste0(
      "spectral method for T-wave alternans, Rangayyan ",
      "(2024) Section 9.10 citing Smith et al., Circulation ",
      "77(1):110-121, 1988"
    )
  )
}


#' Gritzali et al., Section 4.3.4.  The QRS is detected first, blanked
#' to the
#'
#' isoelectric baseline, and the length transform of eq (4.21) re-run
#' with the window at the average T duration.  Working ACROSS channels
#' is what makes T detection tractable.
#'
#' @param chans A vector; its length is taken.
#' @param qrs Coerced to integer by the body, with \code{as.integer}.
#' @param fs Numeric; combined arithmetically in the body.
#' @param tdur Numeric; combined arithmetically in the body. Defaults to \code{0.16}.
#' @return A list with \code{t}, \code{onset}, \code{offset}, \code{length}, \code{tdur}, \code{nchan}, \code{fs}, \code{method}.
#' @export
TWaveDet <- function(chans, qrs, fs, tdur = 0.160) {
  # Gritzali et al., Section 4.3.4.  The QRS is detected first, blanked to the
  # isoelectric baseline, and the length transform of eq (4.21) re-run with
  # the window at the average T duration.  Working ACROSS channels is what
  # makes T detection tractable.
  fs <- .morie_qrs_fs(fs)
  tdur <- as.numeric(tdur)
  if (!(tdur > 0)) stop("tdur must be positive")
  q <- as.integer(qrs)
  if (length(q) < 1L) stop("need at least one QRS position")
  if (!length(chans)) stop("need at least one channel")
  if (!is.list(chans)) chans <- list(chans)
  ch <- lapply(chans, .morie_qrs_check, least = 8L, what = "channel")
  n <- length(ch[[1L]])
  if (any(vapply(ch, length, integer(1)) != n)) {
    stop("all channels must have the same length")
  }
  half <- max(1L, as.integer(round(0.050 * fs)))
  base <- max(1L, as.integer(round(0.040 * fs)))
  blank <- lapply(ch, function(cvec) {
    b <- cvec
    for (pos in q) {
      a1 <- max(0L, pos - half)
      a2 <- min(n, pos + half)
      ra <- max(0L, a1 - base)
      lvl <- if (ra < a1) .morie_fsum(cvec[(ra + 1L):a1]) / (a1 - ra) else 0
      if (a1 < a2) b[(a1 + 1L):a2] <- lvl
    }
    b
  })
  lt <- LengthXfm(blank, tdur, fs)$length
  wsamp <- max(1L, as.integer(round(tdur * fs)))
  lo <- max(1L, as.integer(round(0.100 * fs)))
  hi <- as.integer(round(0.450 * fs))
  bm <- matrix(unlist(blank), nrow = n, ncol = length(blank))
  bsum <- apply(bm, 1L, .morie_fsum)
  tpos <- vector("list", length(q))
  onset <- vector("list", length(q))
  offset <- vector("list", length(q))
  for (kk in seq_along(q)) {
    pos <- q[kk]
    a <- min(n - 1L, pos + lo)
    b <- min(n, pos + hi)
    if (b - a < 2L) {
      tpos[kk] <- list(NULL)
      onset[kk] <- list(NULL)
      offset[kk] <- list(NULL)
      next
    }
    s <- .morie_qrs_argmax(lt, a, b)
    onset[[kk]] <- s
    offset[[kk]] <- min(n - 1L, s + wsamp)
    e <- min(n, s + wsamp)
    tpos[[kk]] <- s + which.max(abs(bsum[(s + 1L):e])) - 1L
  }
  list(
    t = tpos, onset = onset, offset = offset, length = lt,
    tdur = tdur, nchan = length(ch), fs = fs,
    method = paste0(
      "T-wave detection by the length transformation, ",
      "Rangayyan (2024) Section 4.3.4, Eq 4.21 (Gritzali ",
      "et al.)"
    )
  )
}


#' Sections 1.2.4 and 8.11 describe VF but give NO detector and NO
#'
#' threshold, and no external primary source was verified here.  The
#' rule below is therefore stated as what it is: a two-part heuristic
#' from the two properties the book DOES assert -- no discrete QRS
#' complexes (so the integrator crest factor is small) and
#' quasi-sinusoidal 4-7 Hz activity (so the spectral concentration is
#' high).
#'
#' @param x A vector; its length is taken and its elements indexed.
#' @param fs Numeric; combined arithmetically in the body.
#' @param win Numeric; combined arithmetically in the body. Defaults to \code{4}.
#' @param conc Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{0.6}.
#' @param crest Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{4}.
#' @return A list with \code{flag}, \code{domfreq}, \code{concentration}, \code{crest}, \code{rate}, \code{conc}, \code{crestmax}, \code{nwin}, \code{fraction}, \code{win}, \code{fs}, \code{method}.
#' @export
VfDetect <- function(x, fs, win = 4, conc = 0.60, crest = 4) {
  # Sections 1.2.4 and 8.11 describe VF but give NO detector and NO
  # threshold, and no external primary source was verified here.  The rule
  # below is therefore stated as what it is: a two-part heuristic from the
  # two properties the book DOES assert -- no discrete QRS complexes (so the
  # integrator crest factor is small) and quasi-sinusoidal 4-7 Hz activity
  # (so the spectral concentration is high).
  fs <- .morie_qrs_fs(fs)
  x <- .morie_qrs_check(x, 64L, "ECG")
  win <- as.numeric(win)
  conc <- as.numeric(conc)
  crest <- as.numeric(crest)
  if (!(win > 0)) stop("win must be positive")
  if (!(conc > 0 && conc <= 1)) stop("conc must lie in (0, 1]")
  if (!(crest > 1)) stop("crest must be greater than 1")
  wsamp <- max(32L, as.integer(round(win * fs)))
  if (length(x) < wsamp) stop("signal shorter than one analysis window")
  flags <- logical(0)
  doms <- numeric(0)
  concs <- numeric(0)
  rates <- numeric(0)
  crests <- numeric(0)
  for (a in seq(0L, length(x) - wsamp, by = wsamp)) {
    seg <- x[(a + 1L):(a + wsamp)]
    mu <- .morie_fsum(seg) / length(seg)
    seg <- seg - mu
    sp <- .morie_qrs_psd(seg, fs)
    band <- which(sp$freqs >= 0.5 & sp$freqs <= 25)
    if (!length(band)) {
      stop("analysis window too short to resolve the 0.5-25 Hz band")
    }
    kbest <- band[which.max(sp$power[band])]
    fdom <- sp$freqs[kbest]
    tot <- .morie_fsum(sp$power[band])
    near <- band[abs(sp$freqs[band] - fdom) <= 1.5]
    cval <- if (tot > 0) .morie_fsum(sp$power[near]) / tot else 0
    det <- tryCatch(QrsDetect(seg, fs), error = function(e) NULL)
    if (is.null(det)) {
      nb <- 0L
      cf <- 0
    } else {
      nb <- length(det$qrs)
      ig <- det$integrated
      mu2 <- .morie_fsum(ig) / length(ig)
      cf <- if (mu2 > 0) max(ig) / mu2 else 0
    }
    flags <- c(flags, cf <= crest && cval >= conc && fdom >= 1 && fdom <= 10)
    doms <- c(doms, fdom)
    concs <- c(concs, cval)
    rates <- c(rates, 60 * nb / (wsamp / fs))
    crests <- c(crests, cf)
  }
  list(
    flag = flags, domfreq = doms, concentration = concs, crest = crests,
    rate = rates, conc = conc, crestmax = crest, nwin = length(flags),
    fraction = sum(flags) / length(flags), win = win, fs = fs,
    method = paste0(
      "VF heuristic from QRS absence and spectral ",
      "concentration; Rangayyan (2024) Sections 1.2.4 and ",
      "8.11 describe VF but give no detector, and no ",
      "external primary source was verified for this rule"
    )
  )
}


#' Eq (4.1): y0(n) = |x(n) - x(n-2)|.  The two-sample span is what makes
#' it
#'
#' "smoothed"; the absolute value makes an inverted QRS give the same
#' response.
#'
#' @param x A vector; its length is taken and its elements indexed.
#' @return A list with \code{y0}, \code{n}, \code{method}.
#' @export
QrsDeriv1 <- function(x) {
  # eq (4.1): y0(n) = |x(n) - x(n-2)|.  The two-sample span is what makes it
  # "smoothed"; the absolute value makes an inverted QRS give the same
  # response.
  x <- .morie_qrs_check(x, 3L, "ECG")
  n <- length(x)
  list(
    y0 = .morie_qrs_pad(abs(x[3:n] - x[1:(n - 2L)]), 2L), n = n,
    method = paste0(
      "first-derivative QRS operator, Rangayyan (2024) ",
      "Eq 4.1 (Balda et al.)"
    )
  )
}


#' Eq (4.2): y1(n) = |x(n) - 2 x(n-2) + x(n-4)|.  The second difference
#'
#' removes any locally linear trend, so T-wave limbs and baseline drift
#' give nothing while the QRS curvature gives a large output.
#'
#' @param x A vector; its length is taken and its elements indexed.
#' @return A list with \code{y1}, \code{n}, \code{method}.
#' @export
QrsDeriv2 <- function(x) {
  # eq (4.2): y1(n) = |x(n) - 2 x(n-2) + x(n-4)|.  The second difference
  # removes any locally linear trend, so T-wave limbs and baseline drift give
  # nothing while the QRS curvature gives a large output.
  x <- .morie_qrs_check(x, 5L, "ECG")
  n <- length(x)
  y1 <- abs(x[5:n] - 2 * x[3:(n - 2L)] + x[1:(n - 4L)])
  list(
    y1 = .morie_qrs_pad(y1, 4L), n = n,
    method = paste0(
      "second-derivative QRS operator, Rangayyan (2024) ",
      "Eq 4.2 (Balda et al.)"
    )
  )
}


#' Eq (4.3): y2(n) = 1.3 y0(n) + 1.1 y1(n).  Slope and curvature respond
#' to
#'
#' different parts of the complex; the mix has its peak reliably inside
#' it.
#'
#' @param y0 A vector; its length is taken.
#' @param y1 A vector; its length is taken.
#' @return A list with \code{y2}, \code{w0}, \code{w1}, \code{n}, \code{method}.
#' @export
QrsDerivMx <- function(y0, y1) {
  # eq (4.3): y2(n) = 1.3 y0(n) + 1.1 y1(n).  Slope and curvature respond to
  # different parts of the complex; the mix has its peak reliably inside it.
  y0 <- .morie_qrs_check(y0, 1L, "y0")
  y1 <- .morie_qrs_check(y1, 1L, "y1")
  if (length(y0) != length(y1)) stop("y0 and y1 must have the same length")
  list(
    y2 = 1.3 * y0 + 1.1 * y1, w0 = 1.3, w1 = 1.1, n = length(y0),
    method = paste0(
      "combined derivative QRS operator, Rangayyan (2024) ",
      "Eq 4.3 (Balda et al.)"
    )
  )
}


#' Eq (4.4) (Murthy and Rangaraj): g1(n) = sum_i |x(n-i+1) - x(n-i)|^2
#'
#' (N - i + 1).  Squaring rewards the large QRS differences; the
#' linearly falling weight smooths the result without a separate filter.
#'
#' @param x A vector; its length is taken and its elements indexed.
#' @param nwin A count; the body uses it as \code{seq_len(...)}. Defaults to \code{8}.
#' @return A list with \code{g1}, \code{nwin}, \code{n}, \code{method}.
#' @export
QrsWSqDrv <- function(x, nwin = 8) {
  # eq (4.4) (Murthy and Rangaraj): g1(n) = sum_i |x(n-i+1) - x(n-i)|^2
  # (N - i + 1).  Squaring rewards the large QRS differences; the linearly
  # falling weight smooths the result without a separate filter.
  x <- .morie_qrs_check(x, 2L, "ECG")
  nwin <- as.integer(nwin)
  if (nwin < 1L) stop("nwin must be >= 1")
  n <- length(x)
  if (n <= nwin) stop("signal must be longer than the window width")
  ii <- seq_len(nwin)
  wt <- nwin - ii + 1L
  g1 <- numeric(n - nwin)
  for (nn in nwin:(n - 1L)) {
    d <- x[nn - ii + 2L] - x[nn - ii + 1L]
    g1[nn - nwin + 1L] <- .morie_fsum(d * d * wt)
  }
  list(
    g1 = .morie_qrs_pad(g1, nwin), nwin = nwin, n = n,
    method = paste0(
      "weighted squared first-derivative operator, ",
      "Rangayyan (2024) Eq 4.4 (Murthy and Rangaraj)"
    )
  )
}


#' Eq (4.5): the M-point moving average that collapses the several
#'
#' derivative peaks across the Q-R-S swings into one pulse per beat.
#'
#' @param g1 A vector; its length is taken.
#' @param mwin Passed to \code{.morie_qrs_mavg}. Defaults to \code{8}.
#' @return A list with \code{g}, \code{mwin}, \code{n}, \code{method}.
#' @export
QrsDrvSmth <- function(g1, mwin = 8) {
  # eq (4.5): the M-point moving average that collapses the several
  # derivative peaks across the Q-R-S swings into one pulse per beat.
  g1 <- .morie_qrs_check(g1, 1L, "g1")
  mwin <- as.integer(mwin)
  if (mwin < 1L) stop("mwin must be >= 1")
  list(
    g = .morie_qrs_mavg(g1, mwin), mwin = mwin, n = length(g1),
    method = paste0(
      "MA smoothing of the weighted-derivative output, ",
      "Rangayyan (2024) Eq 4.5"
    )
  )
}


#' Eq (4.7): H(z) = (1/32)(1 - z^-6)^2 / (1 - z^-1)^2, evaluated in its
#'
#' equivalent finite form (sum_{k=0}^{5} z^-k)^2 / 32 so the apparent
#' pole at z = 1 is removed exactly rather than numerically.  fc = 11
#' Hz, 5-sample delay, >35 dB at 60 Hz -- all tied to fs = 200 Hz.
#'
#' @param freq Coerced to numeric by the body, with \code{as.numeric}.
#' @param fs Numeric; combined arithmetically in the body. Defaults to \code{200}.
#' @return A list with \code{freq}, \code{mag}, \code{phase}, \code{b}, \code{a}, \code{fs}, \code{fsnote}, \code{method}.
#' @export
QrsLPassTf <- function(freq, fs = 200) {
  # eq (4.7): H(z) = (1/32)(1 - z^-6)^2 / (1 - z^-1)^2, evaluated in its
  # equivalent finite form (sum_{k=0}^{5} z^-k)^2 / 32 so the apparent pole at
  # z = 1 is removed exactly rather than numerically.  fc = 11 Hz, 5-sample
  # delay, >35 dB at 60 Hz -- all tied to fs = 200 Hz.
  fs <- .morie_qrs_fs(fs)
  fr <- as.numeric(freq)
  if (!length(fr)) stop("need at least one frequency")
  k <- 0:5
  mag <- numeric(length(fr))
  ph <- numeric(length(fr))
  for (i in seq_along(fr)) {
    w <- 2 * pi * fr[i] / fs
    sr <- .morie_fsum(cos(-w * k))
    si <- .morie_fsum(sin(-w * k))
    re <- (sr * sr - si * si) / 32
    im <- (2 * sr * si) / 32
    mag[i] <- sqrt(re * re + im * im)
    ph[i] <- atan2(im, re)
  }
  list(
    freq = fr, mag = mag, phase = ph,
    b = c(1 / 32, 0, 0, 0, 0, 0, -2 / 32, 0, 0, 0, 0, 0, 1 / 32),
    a = c(1, -2, 1), fs = fs,
    fsnote = paste0(
      "integer coefficients designed for fs = 200 Hz ",
      "(fc = 11 Hz, 5-sample delay)"
    ),
    method = paste0(
      "Pan-Tompkins lowpass transfer function, Rangayyan ",
      "(2024) Eq 4.7"
    )
  )
}


#' Eq (4.8): y(n) = 2 y(n-1) - y(n-2) + (1/32)[x(n) - 2 x(n-6) +
#' x(n-12)]
#'
#' Adds and one shift by 32 -- which is why it was chosen for real time.
#'
#' @param x A vector; its length is taken and its elements indexed.
#' @return A list with \code{y}, \code{delay}, \code{n}, \code{fsnote}, \code{method}.
#' @export
QrsLPassDf <- function(x) {
  # eq (4.8): y(n) = 2 y(n-1) - y(n-2) + (1/32)[x(n) - 2 x(n-6) + x(n-12)].
  # Adds and one shift by 32 -- which is why it was chosen for real time.
  x <- .morie_qrs_check(x, 1L, "ECG")
  n <- length(x)
  y <- numeric(n)
  for (i in seq_len(n) - 1L) {
    y[i + 1L] <- (if (i >= 1L) 2 * y[i] else 0) -
      (if (i >= 2L) y[i - 1L] else 0) +
      (x[i + 1L] - (if (i >= 6L) 2 * x[i - 5L] else 0) +
        (if (i >= 12L) x[i - 11L] else 0)) / 32
  }
  list(
    y = y, delay = 5L, n = n,
    fsnote = "coefficients fixed for fs = 200 Hz",
    method = paste0(
      "Pan-Tompkins lowpass difference equation, Rangayyan ",
      "(2024) Eq 4.8"
    )
  )
}


#' Eq (4.9): Hlp(z) = (1 - z^-32)/(1 - z^-1), a running sum of 32
#' samples
#'
#' Evaluated as sum_{k=0}^{31} z^-k so the z = 1 point is exact.
#'
#' @param freq Coerced to numeric by the body, with \code{as.numeric}.
#' @param fs Numeric; combined arithmetically in the body. Defaults to \code{200}.
#' @return A list with \code{freq}, \code{mag}, \code{phase}, \code{b}, \code{fs}, \code{method}.
#' @export
QrsHpLpTf <- function(freq, fs = 200) {
  # eq (4.9): Hlp(z) = (1 - z^-32)/(1 - z^-1), a running sum of 32 samples.
  # Evaluated as sum_{k=0}^{31} z^-k so the z = 1 point is exact.
  fs <- .morie_qrs_fs(fs)
  fr <- as.numeric(freq)
  if (!length(fr)) stop("need at least one frequency")
  k <- 0:31
  mag <- numeric(length(fr))
  ph <- numeric(length(fr))
  for (i in seq_along(fr)) {
    w <- 2 * pi * fr[i] / fs
    re <- .morie_fsum(cos(-w * k))
    im <- .morie_fsum(sin(-w * k))
    mag[i] <- sqrt(re * re + im * im)
    ph[i] <- atan2(im, re)
  }
  list(
    freq = fr, mag = mag, phase = ph, b = rep(1, 32), fs = fs,
    method = paste0(
      "lowpass component of the Pan-Tompkins highpass, ",
      "Rangayyan (2024) Eq 4.9"
    )
  )
}


#' Eq (4.10): y(n) = y(n-1) + x(n) - x(n-32).  One add and one subtract
#' per
#'
#' sample regardless of window length.
#'
#' @param x A vector; its length is taken and its elements indexed.
#' @return A list with \code{y}, \code{n}, \code{method}.
#' @export
QrsHpLpDf <- function(x) {
  # eq (4.10): y(n) = y(n-1) + x(n) - x(n-32).  One add and one subtract per
  # sample regardless of window length.
  x <- .morie_qrs_check(x, 1L, "signal")
  n <- length(x)
  y <- numeric(n)
  for (i in seq_len(n) - 1L) {
    y[i + 1L] <- (if (i >= 1L) y[i] else 0) + x[i + 1L] -
      (if (i >= 32L) x[i - 31L] else 0)
  }
  list(
    y = y, n = n,
    method = "recursive 32-point running sum, Rangayyan (2024) Eq 4.10"
  )
}


#' Eq (4.11): Hhp(z) = z^-16 - (1/32) Hlp(z).  An allpass (a pure
#' 16-sample
#'
#' delay) minus a scaled lowpass, so it shares the running sum already
#' computed.  fc = 5 Hz and 80 ms delay hold at fs = 200 Hz.
#'
#' @param freq Coerced to numeric by the body, with \code{as.numeric}.
#' @param fs Numeric; combined arithmetically in the body. Defaults to \code{200}.
#' @return A list with \code{freq}, \code{mag}, \code{phase}, \code{fs}, \code{fsnote}, \code{method}.
#' @export
QrsHPassTf <- function(freq, fs = 200) {
  # eq (4.11): Hhp(z) = z^-16 - (1/32) Hlp(z).  An allpass (a pure 16-sample
  # delay) minus a scaled lowpass, so it shares the running sum already
  # computed.  fc = 5 Hz and 80 ms delay hold at fs = 200 Hz.
  fs <- .morie_qrs_fs(fs)
  fr <- as.numeric(freq)
  if (!length(fr)) stop("need at least one frequency")
  k <- 0:31
  mag <- numeric(length(fr))
  ph <- numeric(length(fr))
  for (i in seq_along(fr)) {
    w <- 2 * pi * fr[i] / fs
    lre <- .morie_fsum(cos(-w * k)) / 32
    lim <- .morie_fsum(sin(-w * k)) / 32
    re <- cos(-w * 16) - lre
    im <- sin(-w * 16) - lim
    mag[i] <- sqrt(re * re + im * im)
    ph[i] <- atan2(im, re)
  }
  list(
    freq = fr, mag = mag, phase = ph, fs = fs,
    fsnote = "fc = 5 Hz and 80 ms delay hold at fs = 200 Hz",
    method = paste0(
      "Pan-Tompkins highpass transfer function, Rangayyan ",
      "(2024) Eq 4.11"
    )
  )
}


#' Eq (4.12): p(n) = x(n-16) - (1/32)[y(n-1) + x(n) - x(n-32)], where
#' the
#'
#' bracketed group is exactly y(n) of eq (4.10), so the running-sum
#' state is reused directly.
#'
#' @param x A vector; its length is taken and its elements indexed.
#' @return A list with \code{p}, \code{y}, \code{n}, \code{method}.
#' @export
QrsHPassDf <- function(x) {
  # eq (4.12): p(n) = x(n-16) - (1/32)[y(n-1) + x(n) - x(n-32)], where the
  # bracketed group is exactly y(n) of eq (4.10), so the running-sum state is
  # reused directly.
  x <- .morie_qrs_check(x, 1L, "signal")
  n <- length(x)
  y <- numeric(n)
  p <- numeric(n)
  for (i in seq_len(n) - 1L) {
    y[i + 1L] <- (if (i >= 1L) y[i] else 0) + x[i + 1L] -
      (if (i >= 32L) x[i - 31L] else 0)
    p[i + 1L] <- (if (i >= 16L) x[i - 15L] else 0) -
      ((if (i >= 1L) y[i] else 0) + x[i + 1L] -
        (if (i >= 32L) x[i - 31L] else 0)) / 32
  }
  list(
    p = p, y = y, n = n,
    method = paste0(
      "Pan-Tompkins highpass difference equation, Rangayyan ",
      "(2024) Eq 4.12"
    )
  )
}


#' Eq (4.13): p(n) = p(n-1) - (1/32) x(n) + x(n-16) - x(n-17) +
#'
#' (1/32) x(n-32).  Eqs (4.9)-(4.12) folded into one recursion: four
#' adds per sample and a single state variable.
#'
#' @param x A vector; its length is taken and its elements indexed.
#' @return A list with \code{p}, \code{n}, \code{delayms}, \code{fsnote}, \code{method}.
#' @export
QrsHPassIo <- function(x) {
  # eq (4.13): p(n) = p(n-1) - (1/32) x(n) + x(n-16) - x(n-17) +
  # (1/32) x(n-32).  Eqs (4.9)-(4.12) folded into one recursion: four adds
  # per sample and a single state variable.
  x <- .morie_qrs_check(x, 1L, "signal")
  n <- length(x)
  p <- numeric(n)
  for (i in seq_len(n) - 1L) {
    p[i + 1L] <- (if (i >= 1L) p[i] else 0) - x[i + 1L] / 32 +
      (if (i >= 16L) x[i - 15L] else 0) -
      (if (i >= 17L) x[i - 16L] else 0) +
      (if (i >= 32L) x[i - 31L] / 32 else 0)
  }
  list(
    p = p, n = n, delayms = 80,
    fsnote = "the 80 ms delay and 5 Hz cutoff hold at fs = 200 Hz",
    method = paste0(
      "combined Pan-Tompkins highpass relation, Rangayyan ",
      "(2024) Eq 4.13"
    )
  )
}


#' Eq (4.14): y(n) = (1/8)[2 x(n) + x(n-1) - x(n-3) - 2 x(n-4)].  The
#'
#' antisymmetric taps make it exactly zero on any constant or linear
#' baseline.
#'
#' @param x A vector; its length is taken.
#' @return A list with \code{y}, \code{b}, \code{n}, \code{fsnote}, \code{method}.
#' @export
QrsDerivOp <- function(x) {
  # eq (4.14): y(n) = (1/8)[2 x(n) + x(n-1) - x(n-3) - 2 x(n-4)].  The
  # antisymmetric taps make it exactly zero on any constant or linear
  # baseline.
  x <- .morie_qrs_check(x, 1L, "signal")
  n <- length(x)
  y <- .morie_qrs_ptderiv(x)
  list(
    y = y, b = c(2 / 8, 1 / 8, 0, -1 / 8, -2 / 8), n = n,
    fsnote = "linear up to about 30 Hz at fs = 200 Hz",
    method = paste0(
      "Pan-Tompkins derivative operator, Rangayyan (2024) ",
      "Eq 4.14"
    )
  )
}


#' Eq (4.15).  N matters: too wide merges QRS and T, too narrow leaves
#'
#' multiple peaks.  The book found N = 30 at fs = 200 Hz, i.e. 150 ms,
#' and supplying fs preserves the 150 ms window at any rate.
#'
#' @param x A vector; its length is taken.
#' @param nwin Numeric; combined arithmetically in the body. Defaults to \code{30}.
#' @param fs Optional; may be \code{NULL}. Passed to \code{.morie_qrs_fs}.
#' @return A list with \code{y}, \code{nwin}, \code{widthsec}, \code{n}, \code{method}.
#' @export
QrsMwInt <- function(x, nwin = 30, fs = NULL) {
  # eq (4.15).  N matters: too wide merges QRS and T, too narrow leaves
  # multiple peaks.  The book found N = 30 at fs = 200 Hz, i.e. 150 ms, and
  # supplying fs preserves the 150 ms window at any rate.
  x <- .morie_qrs_check(x, 1L, "signal")
  if (!is.null(fs)) nwin <- max(1L, as.integer(round(0.150 * .morie_qrs_fs(fs))))
  nwin <- as.integer(nwin)
  if (nwin < 1L) stop("nwin must be >= 1")
  list(
    y = .morie_qrs_mavg(x, nwin), nwin = nwin,
    widthsec = if (!is.null(fs)) nwin / as.numeric(fs) else NULL,
    n = length(x),
    method = paste0(
      "Pan-Tompkins moving-window integrator, Rangayyan ",
      "(2024) Eq 4.15"
    )
  )
}


#' Eqs (4.16) and (4.17).  The 0.125/0.875 split is a long-memory
#' recursive
#'
#' average, so the estimates track slow drift without being thrown by
#' one artifact.  I1 sits a quarter of the way from noise up to signal;
#' I2 is half of I1 and is reserved for search-back.
#'
#' @param peaki Numeric; combined arithmetically in the body.
#' @param spki Numeric; combined arithmetically in the body.
#' @param npki Numeric; combined arithmetically in the body.
#' @param issignal Coerced to logical by the body, with \code{as.logical}.
#' @return A list with \code{spki}, \code{npki}, \code{thresh1}, \code{thresh2}, \code{peaki}, \code{issignal}, \code{method}.
#' @export
QrsThresh <- function(peaki, spki, npki, issignal) {
  # eqs (4.16) and (4.17).  The 0.125/0.875 split is a long-memory recursive
  # average, so the estimates track slow drift without being thrown by one
  # artifact.  I1 sits a quarter of the way from noise up to signal; I2 is
  # half of I1 and is reserved for search-back.
  peaki <- as.numeric(peaki)
  spki <- as.numeric(spki)
  npki <- as.numeric(npki)
  if (isTRUE(as.logical(issignal))) {
    spki <- 0.125 * peaki + 0.875 * spki
  } else {
    npki <- 0.125 * peaki + 0.875 * npki
  }
  t1 <- npki + 0.25 * (spki - npki)
  list(
    spki = spki, npki = npki, thresh1 = t1, thresh2 = 0.5 * t1,
    peaki = peaki, issignal = isTRUE(as.logical(issignal)),
    method = paste0(
      "Pan-Tompkins adaptive thresholds, Rangayyan (2024) ",
      "Eqs 4.16 and 4.17"
    )
  )
}


#' Eq (4.18): SPKI = 0.25 PEAKI + 0.75 SPKI, replacing the 0.125/0.875
#' rule
#'
#' A beat found only by search-back was missed by the primary threshold,
#' so the running estimate is too high and the heavier weight pulls it
#' down.
#'
#' @param peaki Numeric; combined arithmetically in the body.
#' @param spki Numeric; combined arithmetically in the body.
#' @return A list with \code{spki}, \code{previous}, \code{peaki}, \code{method}.
#' @export
QrsSpkiUpd <- function(peaki, spki) {
  # eq (4.18): SPKI = 0.25 PEAKI + 0.75 SPKI, replacing the 0.125/0.875 rule.
  # A beat found only by search-back was missed by the primary threshold, so
  # the running estimate is too high and the heavier weight pulls it down.
  peaki <- as.numeric(peaki)
  spki <- as.numeric(spki)
  list(
    spki = 0.25 * peaki + 0.75 * spki, previous = spki, peaki = peaki,
    method = paste0(
      "Pan-Tompkins search-back SPKI update, Rangayyan ",
      "(2024) Eq 4.18"
    )
  )
}


#' Eq (4.19): HR = 60 NB / T.  The counting estimate averages over the
#' whole
#'
#' window, so unlike the beat-to-beat form of eq (4.20) it is
#' insensitive to a single mis-detected interval.
#'
#' @param nbeats Numeric; combined arithmetically in the body.
#' @param duration Numeric; combined arithmetically in the body.
#' @return A list with \code{hr}, \code{nbeats}, \code{duration}, \code{method}.
#' @export
HrFromCnt <- function(nbeats, duration) {
  # eq (4.19): HR = 60 NB / T.  The counting estimate averages over the whole
  # window, so unlike the beat-to-beat form of eq (4.20) it is insensitive to
  # a single mis-detected interval.
  nbeats <- as.integer(nbeats)
  duration <- as.numeric(duration)
  if (nbeats < 0L) stop("nbeats must be non-negative")
  if (!(duration > 0)) stop("duration must be positive")
  list(
    hr = 60 * nbeats / duration, nbeats = nbeats, duration = duration,
    method = paste0(
      "average heart rate from beat count, Rangayyan (2024) ",
      "Eq 4.19"
    )
  )
}


#' Eq (4.21) (Gritzali et al.).  Since (dx/dt) dt = dx, the transform is
#' the
#'
#' arc length of the MULTICHANNEL trajectory accumulated over a w-second
#' window.  Summing the squared derivative across channels before
#' integrating is the whole point: a wave well defined in only one lead
#' still contributes.
#'
#' @param chans A vector; its length is taken.
#' @param wwin Numeric; combined arithmetically in the body.
#' @param fs Numeric; combined arithmetically in the body.
#' @return A list with \code{length}, \code{nchan}, \code{wsamp}, \code{wsec}, \code{fs}, \code{n}, \code{method}.
#' @export
LengthXfm <- function(chans, wwin, fs) {
  # eq (4.21) (Gritzali et al.).  Since (dx/dt) dt = dx, the transform is the
  # arc length of the MULTICHANNEL trajectory accumulated over a w-second
  # window.  Summing the squared derivative across channels before
  # integrating is the whole point: a wave well defined in only one lead
  # still contributes.
  fs <- .morie_qrs_fs(fs)
  wwin <- as.numeric(wwin)
  if (!(wwin > 0)) stop("wwin must be positive")
  if (!length(chans)) stop("need at least one channel")
  if (!is.list(chans)) chans <- list(chans)
  ch <- lapply(chans, .morie_qrs_check, least = 2L, what = "channel")
  nlen <- length(ch[[1L]])
  if (any(vapply(ch, length, integer(1)) != nlen)) {
    stop("all channels must have the same length")
  }
  dm <- matrix(unlist(lapply(ch, diff)),
    nrow = nlen - 1L,
    ncol = length(ch)
  )
  step <- sqrt(apply(dm, 1L, function(r) .morie_fsum(r * r)))
  wsamp <- max(1L, as.integer(round(wwin * fs)))
  out <- numeric(nlen)
  run <- 0
  for (k in seq_along(step) - 1L) {
    run <- run + step[k + 1L]
    if (k >= wsamp) run <- run - step[k - wsamp + 1L]
    out[max(0L, k - wsamp + 1L) + 1L] <- run
  }
  list(
    length = out, nchan = length(ch), wsamp = wsamp, wsec = wwin,
    fs = fs, n = nlen,
    method = paste0(
      "length transformation, Rangayyan (2024) Eq 4.21 ",
      "(Gritzali et al.)"
    )
  )
}


#' Eq (4.23) (Lehner and Rangayyan): s(n) = sum_k p^2(n-k+1) (M - k + 1)
#'
#' Squaring discards the sign of the curvature; the linearly decaying
#' weight smooths while keeping the response prompt at the newest
#' sample.  s(n) has two peaks per cycle: the upstroke onset, then the
#' dicrotic notch.
#'
#' @param p A vector; its length is taken and its elements indexed.
#' @param mwin A count; the body uses it as \code{seq_len(...)}. Defaults to \code{16}.
#' @return A list with \code{s}, \code{weights}, \code{mwin}, \code{n}, \code{method}.
#' @export
DNotchSmth <- function(p, mwin = 16) {
  # eq (4.23) (Lehner and Rangayyan): s(n) = sum_k p^2(n-k+1) (M - k + 1).
  # Squaring discards the sign of the curvature; the linearly decaying weight
  # smooths while keeping the response prompt at the newest sample.  s(n) has
  # two peaks per cycle: the upstroke onset, then the dicrotic notch.
  p <- .morie_qrs_check(p, 1L, "second derivative")
  mwin <- as.integer(mwin)
  if (mwin < 1L) stop("mwin must be >= 1")
  w <- as.numeric(mwin - seq_len(mwin) + 1L)
  n <- length(p)
  s <- numeric(n)
  for (i in seq_len(n) - 1L) {
    kk <- 1:mwin
    kk <- kk[i - kk + 1L >= 0L]
    s[i + 1L] <- if (length(kk)) {
      .morie_fsum(p[i - kk + 2L]^2 * w[kk])
    } else {
      0
    }
  }
  list(
    s = s, weights = w, mwin = mwin, n = n,
    method = paste0(
      "squared weighted smoothing for dicrotic notch, ",
      "Rangayyan (2024) Eq 4.23 (Lehner and Rangayyan)"
    )
  )
}


rangayyan_baseline_wander <- BlWander
rangayyan_carotid_pulse <- CPulseFeat
rangayyan_deriv_qrs <- QrsDeriv
rangayyan_dicrotic_notch <- DicNotch
rangayyan_ecg_emg_coupling <- EcgEmgCpl
rangayyan_ecg_features <- EcgFeat
rangayyan_ecg_waveshape <- EcgWaveShp
rangayyan_exercise_ecg <- ExerEcgSt
rangayyan_hrv_freq_domain <- HrvFreq
rangayyan_hrv_time_domain <- HrvTime
rangayyan_heart_sound_id <- HSoundId
rangayyan_maternal_ecg_filter <- MEcgFilt
rangayyan_motion_artifact <- MotionArt
rangayyan_pan_tompkins <- QrsDetect
rangayyan_pcg_segments <- PcgParts
rangayyan_powerline_removal <- PLineNotch
rangayyan_ppg_features <- PpgFeat
rangayyan_p_wave_detect <- PWaveDet
rangayyan_resp_signal <- EdrSignal
rangayyan_sleep_apnea <- ApneaEdr
rangayyan_spectral_power_ratio <- LfHfRatio
rangayyan_twave_alternans <- TwaSpectr
rangayyan_t_wave_detect <- TWaveDet
rangayyan_vf_detect <- VfDetect
rangayyan_ch4_qrs_first_derivative_balda <- QrsDeriv1
rangayyan_ch4_qrs_second_derivative_balda <- QrsDeriv2
rangayyan_ch4_qrs_combined_balda <- QrsDerivMx
rangayyan_ch4_filtered_derivative_murthy <- QrsWSqDrv
rangayyan_ch4_qrs_smoothing_ma_filter <- QrsDrvSmth
rangayyan_ch4_pan_tompkins_lowpass_transfer <- QrsLPassTf
rangayyan_ch4_pan_tompkins_lowpass_difference_eq <- QrsLPassDf
rangayyan_ch4_pan_tompkins_highpass_lp_component <- QrsHpLpTf
rangayyan_ch4_pan_tompkins_highpass_lp_difference_eq <- QrsHpLpDf
rangayyan_ch4_pan_tompkins_highpass_transfer <- QrsHPassTf
rangayyan_ch4_pan_tompkins_highpass_difference_eq <- QrsHPassDf
rangayyan_ch4_pan_tompkins_highpass_combined <- QrsHPassIo
rangayyan_ch4_pan_tompkins_derivative_operator <- QrsDerivOp
rangayyan_ch4_pan_tompkins_moving_window_integrator <- QrsMwInt
rangayyan_ch4_pan_tompkins_thresholds <- QrsThresh
rangayyan_ch4_pan_tompkins_searchback_update <- QrsSpkiUpd
rangayyan_ch4_heart_rate_from_count <- HrFromCnt
rangayyan_ch4_length_transformation <- LengthXfm
rangayyan_ch4_dicrotic_notch_smoothed_squared <- DNotchSmth

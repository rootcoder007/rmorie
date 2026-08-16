# morie -- aaa_rangayyan_phys.R
#
# R mirror of morie.fn.bsaphys: physiological models and clinical
# applications -- membrane and neuron models, PCG, EMG, EEG, VAG,
# respiratory and sleep signal analysis.  Biomedical Signal Analysis
# (Rangayyan, 2024).
#
# Internal: no roxygen, no exports.  Base R + stats only.
# The Python module is the source of truth; field names and units match it
# exactly (mV, ms, mS/cm^2, mM).

# -- shared helpers for the biophysical signal-generation blocks --------------

.BSA_R_GAS <- 8.314462618 # J/(mol K), CODATA 2018
.BSA_FARADAY <- 96485.33212 # C/mol,     CODATA 2018


#' In-place iterative radix-2 Cooley-Tukey FFT; len must be a power of 2
#'
#' Part of the rangayyan_phys implementation; see the file header for
#' the source it follows.
#'
#' @param re See Usage.
#' @param im See Usage.
#' @return A list with \code{re}, \code{im}.
#' @export
.bsafft <- function(re, im) {
  # In-place iterative radix-2 Cooley-Tukey FFT; len must be a power of 2.
  n <- length(re)
  j <- 0L
  for (i in seq_len(n - 1L)) {
    bit <- bitwShiftR(n, 1L)
    while (bitwAnd(j, bit) != 0L) {
      j <- bitwXor(j, bit)
      bit <- bitwShiftR(bit, 1L)
    }
    j <- bitwOr(j, bit)
    if (i < j) {
      tt <- re[i + 1L]
      re[i + 1L] <- re[j + 1L]
      re[j + 1L] <- tt
      tt <- im[i + 1L]
      im[i + 1L] <- im[j + 1L]
      im[j + 1L] <- tt
    }
  }
  len <- 2L
  while (len <= n) {
    ang <- -2 * pi / len
    wr <- cos(ang)
    wi <- sin(ang)
    half <- bitwShiftR(len, 1L)
    i <- 0L
    while (i < n) {
      cr <- 1
      ci <- 0
      for (k in i:(i + half - 1L)) {
        ur <- re[k + 1L]
        ui <- im[k + 1L]
        br <- re[k + half + 1L]
        bi <- im[k + half + 1L]
        vr <- br * cr - bi * ci
        vi <- br * ci + bi * cr
        re[k + 1L] <- ur + vr
        im[k + 1L] <- ui + vi
        re[k + half + 1L] <- ur - vr
        im[k + half + 1L] <- ui - vi
        ncr <- cr * wr - ci * wi
        ci <- cr * wi + ci * wr
        cr <- ncr
      }
      i <- i + len
    }
    len <- bitwShiftL(len, 1L)
  }
  list(re = re, im = im)
}


#' Periodogram of x sampled at fs Hz with a Hann window, 0 .. fs/2
#'
#' Part of the rangayyan_phys implementation; see the file header for
#' the source it follows.
#'
#' @param x See Usage.
#' @param fs See Usage.
#' @param detrend Defaults to \code{TRUE}.
#' @return A list with \code{freqs}, \code{power}.
#' @export
.bsapsd <- function(x, fs, detrend = TRUE) {
  # Periodogram of x sampled at fs Hz with a Hann window, 0 .. fs/2.
  xs <- as.numeric(x)
  n <- length(xs)
  if (n < 4L) stop("need at least 4 samples for a spectrum")
  if (!(fs > 0)) stop("fs must be positive (Hz)")
  if (detrend) xs <- xs - .morie_fsum(xs) / n
  w <- 0.5 - 0.5 * cos(2 * pi * (seq_len(n) - 1L) / (n - 1L))
  wsum2 <- .morie_fsum(w * w)
  nfft <- 1L
  while (nfft < n) nfft <- bitwShiftL(nfft, 1L)
  ft <- .bsafft(c(xs * w, rep(0, nfft - n)), rep(0, nfft))
  m <- nfft %/% 2L + 1L
  idx <- seq_len(m)
  list(
    freqs = (idx - 1L) * fs / nfft,
    power = (ft$re[idx] * ft$re[idx] + ft$im[idx] * ft$im[idx]) / wsum2
  )
}


#' Total power in the half-open band [lo, hi) Hz
#'
#' Part of the rangayyan_phys implementation; see the file header for
#' the source it follows.
#'
#' @param freqs See Usage.
#' @param power See Usage.
#' @param lo See Usage.
#' @param hi See Usage.
#' @return The value of \code{.morie_fsum}.
#' @export
.bsabandpow <- function(freqs, power, lo, hi) {
  # Total power in the half-open band [lo, hi) Hz.
  .morie_fsum(power[freqs >= lo & freqs < hi])
}


#' Local maxima of power, strongest first, at least minsep Hz apart
#'
#' Part of the rangayyan_phys implementation; see the file header for
#' the source it follows.
#'
#' @param freqs See Usage.
#' @param power See Usage.
#' @param count Defaults to \code{3L}.
#' @param minsep Defaults to \code{0}.
#' @return A list with \code{freqs}, \code{powers}.
#' @export
.bsapeaks <- function(freqs, power, count = 3L, minsep = 0) {
  # Local maxima of power, strongest first, at least minsep Hz apart.
  np <- length(power)
  of <- numeric(0)
  op <- numeric(0)
  if (np >= 3L) {
    k <- 2:(np - 1L)
    sel <- k[power[k] > power[k - 1L] & power[k] >= power[k + 1L]]
    if (length(sel)) {
      o <- order(power[sel], freqs[sel], decreasing = TRUE)
      cp <- power[sel][o]
      cf <- freqs[sel][o]
      for (i in seq_along(cp)) {
        if (!length(of) || all(abs(cf[i] - of) >= minsep)) {
          of <- c(of, cf[i])
          op <- c(op, cp[i])
        }
        if (length(of) >= count) break
      }
    }
  }
  list(freqs = of, powers = op)
}


#' Biased autocorrelation of the mean-removed x, lags 0..maxlag
#'
#' Part of the rangayyan_phys implementation; see the file header for
#' the source it follows.
#'
#' @param x See Usage.
#' @param maxlag See Usage.
#' @return A vector, from \code{vapply}.
#' @export
.bsaacf <- function(x, maxlag) {
  # Biased autocorrelation of the mean-removed x, lags 0..maxlag.
  xs <- as.numeric(x)
  n <- length(xs)
  if (n < 2L) stop("need at least 2 samples")
  xs <- xs - .morie_fsum(xs) / n
  maxlag <- min(maxlag, n - 1L)
  vapply(
    0:maxlag,
    function(k) .morie_fsum(xs[seq_len(n - k)] * xs[seq_len(n - k) + k]) / n,
    numeric(1)
  )
}


#' Levinson-Durbin linear prediction; returns list(a = a[1..p], e =
#' error)
#'
#' Part of the rangayyan_phys implementation; see the file header for
#' the source it follows.
#'
#' @param x See Usage.
#' @param order See Usage.
#' @return A list with \code{a}, \code{e}.
#' @export
.bsalpc <- function(x, order) {
  # Levinson-Durbin linear prediction; returns list(a = a[1..p], e = error).
  if (order < 1L) stop("order must be >= 1")
  r <- .bsaacf(x, order)
  if (r[1L] <= 0) stop("signal is constant; no linear-prediction model")
  a <- rep(0, order + 1L) # a[1] is Python a[0], unused
  e <- r[1L]
  for (i in seq_len(order)) {
    acc <- r[i + 1L]
    if (i > 1L) {
      acc <- acc + .morie_fsum(a[seq_len(i - 1L) + 1L] * r[i - seq_len(i - 1L) + 1L])
    }
    k <- -acc / e
    newa <- a
    newa[i + 1L] <- k
    if (i > 1L) {
      j <- seq_len(i - 1L)
      newa[j + 1L] <- a[j + 1L] + k * a[i - j + 1L]
    }
    a <- newa
    e <- e * (1 - k * k)
    if (e <= 0) {
      e <- 1e-30
      break
    }
  }
  list(a = a[-1L], e = e)
}


#' Magnitude-squared response of the all-pole LPC filter, 0..fs/2 Hz
#'
#' Part of the rangayyan_phys implementation; see the file header for
#' the source it follows.
#'
#' @param a See Usage.
#' @param fs See Usage.
#' @param npts Defaults to \code{1024L}.
#' @return A list with \code{freqs}, \code{power}.
#' @export
.bsalpcspec <- function(a, fs, npts = 1024L) {
  # Magnitude-squared response of the all-pole LPC filter, 0..fs/2 Hz.
  i <- seq_len(npts) - 1L
  f <- 0.5 * fs * i / (npts - 1L)
  w <- 2 * pi * f / fs
  dr <- rep(1, npts)
  di <- rep(0, npts)
  for (k in seq_along(a)) {
    dr <- dr + a[k] * cos(-w * k)
    di <- di + a[k] * sin(-w * k)
  }
  d2 <- dr * dr + di * di
  list(freqs = f, power = ifelse(d2 > 1e-30, 1 / d2, 1e30))
}


#' Moments of a PSD treated as a density, Rangayyan (2024) Section 6.4.1
#'
#' Part of the rangayyan_phys implementation; see the file header for
#' the source it follows.
#'
#' @param freqs See Usage.
#' @param power See Usage.
#' @return A list with \code{total_power}, \code{mean_freq_hz}, \code{median_freq_hz}, \code{fm2_hz2}, \code{spread_hz}, \code{spectral_skewness}, \code{spectral_kurtosis}.
#' @export
.bsapsdmom <- function(freqs, power) {
  # Moments of a PSD treated as a density, Rangayyan (2024) Section 6.4.1.
  Ep <- .morie_fsum(power)
  if (Ep <= 0) stop("PSD has zero total power; nothing to characterise")
  fmean <- .morie_fsum(freqs * power) / Ep
  run <- cumsum(power)
  hit <- which(run >= 0.5 * Ep)
  fmed <- if (length(hit)) freqs[hit[1L]] else freqs[length(freqs)]
  d <- freqs - fmean
  fm2 <- .morie_fsum(d * d * power) / Ep
  fm3 <- .morie_fsum(d * d * d * power) / Ep
  fm4 <- .morie_fsum(d * d * d * d * power) / Ep
  list(
    total_power = Ep, mean_freq_hz = fmean, median_freq_hz = fmed,
    fm2_hz2 = fm2, spread_hz = sqrt(fm2),
    spectral_skewness = if (fm2 > 0) fm3 / fm2^1.5 else 0,
    spectral_kurtosis = if (fm2 > 0) fm4 / (fm2 * fm2) else 0
  )
}


#' 3 dB bandwidth and Q = f_peak / bandwidth of the peak at fpeak
#'
#' Part of the rangayyan_phys implementation; see the file header for
#' the source it follows.
#'
#' @param freqs See Usage.
#' @param power See Usage.
#' @param fpeak See Usage.
#' @return A list with \code{bandwidth_hz}, \code{q}.
#' @export
.bsaqfactor <- function(freqs, power, fpeak) {
  # -3 dB bandwidth and Q = f_peak / bandwidth of the peak at fpeak.
  if (!length(freqs)) {
    return(list(bandwidth_hz = NULL, q = NULL))
  }
  i <- which.min(abs(freqs - fpeak))
  half <- power[i] / 2
  lo <- NULL
  hi <- NULL
  for (k in i:1L) {
    if (power[k] <= half) {
      lo <- freqs[k]
      break
    }
  }
  for (k in i:length(freqs)) {
    if (power[k] <= half) {
      hi <- freqs[k]
      break
    }
  }
  if (is.null(lo) || is.null(hi) || hi <= lo) {
    return(list(bandwidth_hz = NULL, q = NULL))
  }
  bw <- hi - lo
  list(bandwidth_hz = bw, q = if (bw > 0) freqs[i] / bw else NULL)
}


#' Hjorth activity, mobility, form factor; Rangayyan (2024) eqs.
#' (5.25-26)
#'
#' Part of the rangayyan_phys implementation; see the file header for
#' the source it follows.
#'
#' @param x See Usage.
#' @return A list with \code{activity}, \code{mobility}, \code{form_factor}.
#' @export
.bsahjorth <- function(x) {
  # Hjorth activity, mobility, form factor; Rangayyan (2024) eqs. (5.25-26).
  xs <- as.numeric(x)
  if (length(xs) < 4L) {
    stop("need at least 4 samples for Hjorth parameters")
  }
  vr <- function(v) {
    n <- length(v)
    mu <- .morie_fsum(v) / n
    .morie_fsum((v - mu) * (v - mu)) / n
  }
  d1 <- diff(xs)
  d2 <- diff(d1)
  a0 <- vr(xs)
  a1 <- vr(d1)
  a2 <- vr(d2)
  if (a0 <= 0 || a1 <= 0) {
    stop("signal is constant; Hjorth parameters undefined")
  }
  mob <- sqrt(a1 / a0)
  list(activity = a0, mobility = mob, form_factor = sqrt(a2 / a1) / mob)
}


#' .bsarms
#'
#' Part of the rangayyan_phys implementation; see the file header for
#' the source it follows.
#'
#' @param x See Usage.
#' @return A numeric value.
#' @export
.bsarms <- function(x) {
  xs <- as.numeric(x)
  if (!length(xs)) stop("empty signal")
  sqrt(.morie_fsum(xs * xs) / length(xs))
}


#' (mean, variance, skewness, kurtosis); kurtosis is the raw fourth
#'
#' standardised moment (3.0 for a Gaussian).
#'
#' @param x See Usage.
#' @return A vector, from \code{c}.
#' @export
.bsamoments <- function(x) {
  # (mean, variance, skewness, kurtosis); kurtosis is the raw fourth
  # standardised moment (3.0 for a Gaussian).
  xs <- as.numeric(x)
  n <- length(xs)
  if (n < 2L) stop("need at least 2 samples")
  mu <- .morie_fsum(xs) / n
  d <- xs - mu
  m2 <- .morie_fsum(d * d) / n
  if (m2 <= 0) {
    return(c(mu, 0, 0, 0))
  }
  s <- sqrt(m2)
  m3 <- .morie_fsum(d * d * d) / n
  m4 <- .morie_fsum(d * d * d * d) / n
  c(mu, m2, m3 / (s * s * s), m4 / (m2 * m2))
}


#' Short-time RMS envelope, non-overlapping windows of win_s seconds
#'
#' Part of the rangayyan_phys implementation; see the file header for
#' the source it follows.
#'
#' @param x See Usage.
#' @param fs See Usage.
#' @param win_s See Usage.
#' @return A list with \code{env}, \code{step_s}.
#' @export
.bsaenvelope <- function(x, fs, win_s) {
  # Short-time RMS envelope, non-overlapping windows of win_s seconds.
  xs <- as.numeric(x)
  w <- max(1L, as.integer(round(win_s * fs)))
  starts <- seq.int(0L, length(xs) - w, by = w)
  if (length(xs) < w) starts <- integer(0)
  list(
    env = vapply(
      starts, function(i) .bsarms(xs[(i + 1L):(i + w)]),
      numeric(1)
    ),
    step_s = w / fs
  )
}


#' Hodgkin-Huxley (1952) alpha/beta rate constants in 1/ms for membrane
#'
#' potential v in mV, modern sign convention, rest -65 mV.  Removable
#' singularities at v = -55 and v = -40 mV replaced by their limits.
#'
#' @param v See Usage.
#' @return A vector, from \code{c}.
#' @export
.bsahhrates <- function(v) {
  # Hodgkin-Huxley (1952) alpha/beta rate constants in 1/ms for membrane
  # potential v in mV, modern sign convention, rest -65 mV.  Removable
  # singularities at v = -55 and v = -40 mV replaced by their limits.
  d <- v + 55
  an <- if (abs(d) < 1e-6) 0.1 else 0.01 * d / (1 - exp(-d / 10))
  bn <- 0.125 * exp(-(v + 65) / 80)
  d <- v + 40
  am <- if (abs(d) < 1e-6) 1 else 0.1 * d / (1 - exp(-d / 10))
  bm <- 4 * exp(-(v + 65) / 18)
  ah <- 0.07 * exp(-(v + 65) / 20)
  bh <- 1 / (1 + exp(-(v + 35) / 10))
  c(am = am, bm = bm, ah = ah, bh = bh, an = an, bn = bn)
}


# -- ApWave: idealised action-potential waveform (ramp and decay).
#' ApWave: idealised action-potential waveform (ramp and decay)
#'
#' Part of the rangayyan_phys implementation; see the file header for
#' the source it follows.
#'
#' @param t See Usage.
#' @param v_rest Defaults to \code{-70}.
#' @param v_peak Defaults to \code{30}.
#' @param t_rise Defaults to \code{0.5}.
#' @param t_fall Defaults to \code{1}.
#' @param t_onset Defaults to \code{0}.
#' @param v_undershoot Defaults to \code{NULL}.
#' @param t_recover Defaults to \code{NULL}.
#' @return A list with \code{t_ms}, \code{V_mV}, \code{amplitude_mV}, \code{peak_time_ms}, \code{v_rest_mV}, \code{v_peak_mV}, \code{t_rise_ms}, \code{t_fall_ms}, \code{width_half_ms}, \code{units}, \code{method}.
#' @export
ApWave <- function(t, v_rest = -70, v_peak = 30, t_rise = 0.5, t_fall = 1,
                   t_onset = 0, v_undershoot = NULL, t_recover = NULL) {
  ts <- as.numeric(t)
  if (!length(ts)) stop("t must contain at least one time point (ms)")
  v_rest <- as.numeric(v_rest)
  v_peak <- as.numeric(v_peak)
  if (v_peak <= v_rest) stop("v_peak must exceed v_rest (mV)")
  t_rise <- as.numeric(t_rise)
  t_fall <- as.numeric(t_fall)
  if (t_rise <= 0) stop("t_rise must be positive (ms)")
  if (t_fall <= 0) stop("t_fall must be positive (ms)")
  t_onset <- as.numeric(t_onset)
  if (is.null(v_undershoot)) {
    v_us <- NULL
  } else {
    v_us <- as.numeric(v_undershoot)
    if (v_us >= v_rest) stop("v_undershoot must lie below v_rest (mV)")
    if (is.null(t_recover) || as.numeric(t_recover) <= 0) {
      stop("t_recover must be positive (ms) when v_undershoot is set")
    }
  }
  t_peak <- t_onset + t_rise
  v_floor <- if (is.null(v_us)) v_rest else v_us
  Vs <- vapply(ts, function(ti) {
    if (ti < t_onset) {
      v_rest
    } else if (ti < t_peak) {
      v_rest + (v_peak - v_rest) * (ti - t_onset) / t_rise
    } else {
      v <- v_floor + (v_peak - v_floor) * exp(-(ti - t_peak) / t_fall)
      if (!is.null(v_us)) {
        v <- v + (v_rest - v_us) *
          (1 - exp(-(ti - t_peak) / as.numeric(t_recover)))
      }
      v
    }
  }, numeric(1))
  amp <- v_peak - v_rest
  half <- v_rest + 0.5 * amp
  above <- ts[Vs >= half]
  width <- if (length(above) > 1L) above[length(above)] - above[1L] else NULL
  list(
    t_ms = ts, V_mV = Vs,
    amplitude_mV = amp, peak_time_ms = t_peak,
    v_rest_mV = v_rest, v_peak_mV = v_peak,
    t_rise_ms = t_rise, t_fall_ms = t_fall,
    width_half_ms = width,
    units = list(V = "mV", t = "ms"),
    method = paste(
      "Idealised ramp-and-decay action potential;",
      "Rangayyan (2024) Section 1.2.2 is descriptive and",
      "gives no waveform equation"
    )
  )
}


# -- Ghk: Goldman-Hodgkin-Katz voltage equation for the resting potential.
#' Ghk: Goldman-Hodgkin-Katz voltage equation for the resting potential
#'
#' Part of the rangayyan_phys implementation; see the file header for
#' the source it follows.
#'
#' @param ion_concs See Usage.
#' @param P_K Defaults to \code{1}.
#' @param P_Na Defaults to \code{0.04}.
#' @param P_Cl Defaults to \code{0.45}.
#' @param T Defaults to \code{310.15}.
#' @return A list with \code{potential_mV}, \code{potential_V}, \code{numerator_mM}, \code{denominator_mM}, \code{slope_mV}, \code{permeabilities}, \code{T_K}, \code{units}, \code{method}.
#' @export
Ghk <- function(ion_concs, P_K = 1, P_Na = 0.04, P_Cl = 0.45, T = 310.15) {
  if (!is.list(ion_concs)) {
    stop("ion_concs must be a dict of concentrations in mM")
  }
  need <- c("K_out", "K_in", "Na_out", "Na_in", "Cl_out", "Cl_in")
  missing <- need[!(need %in% names(ion_concs))]
  if (length(missing)) {
    stop(paste0("ion_concs is missing keys: ", paste(missing, collapse = ", ")))
  }
  cc <- numeric(0)
  for (k in need) {
    v <- as.numeric(ion_concs[[k]])
    if (v <= 0) stop(paste0("concentration ", k, " must be positive (mM)"))
    cc[k] <- v
  }
  T <- as.numeric(T)
  if (T <= 0) stop("T must be a positive absolute temperature in kelvin")
  P_K <- as.numeric(P_K)
  P_Na <- as.numeric(P_Na)
  P_Cl <- as.numeric(P_Cl)
  if (min(P_K, P_Na, P_Cl) < 0) stop("permeabilities must be non-negative")
  if (P_K + P_Na + P_Cl <= 0) {
    stop("at least one permeability must be positive")
  }
  num <- P_K * cc[["K_out"]] + P_Na * cc[["Na_out"]] + P_Cl * cc[["Cl_in"]]
  den <- P_K * cc[["K_in"]] + P_Na * cc[["Na_in"]] + P_Cl * cc[["Cl_out"]]
  if (num <= 0 || den <= 0) {
    stop("weighted concentration sums must be positive")
  }
  slope <- .BSA_R_GAS * T / .BSA_FARADAY
  volts <- slope * log(num / den)
  list(
    potential_mV = volts * 1000, potential_V = volts,
    numerator_mM = num, denominator_mM = den,
    slope_mV = slope * 1000,
    permeabilities = list(K = P_K, Na = P_Na, Cl = P_Cl),
    T_K = T,
    units = list(potential = "mV", concentration = "mM", T = "K"),
    method = paste(
      "Goldman (1943) / Hodgkin & Katz (1949) GHK voltage",
      "equation; not given in Rangayyan (2024)"
    )
  )
}


# -- Nernst: equilibrium (reverse) potential of a single ionic species.
#' Nernst: equilibrium (reverse) potential of a single ionic species
#'
#' Part of the rangayyan_phys implementation; see the file header for
#' the source it follows.
#'
#' @param T Defaults to \code{310.15}.
#' @param z Defaults to \code{1}.
#' @param conc_out Defaults to \code{5}.
#' @param conc_in Defaults to \code{140}.
#' @param ion Defaults to \code{"K+"}.
#' @return A list with \code{ion}, \code{potential_mV}, \code{potential_V}, \code{slope_mV}, \code{ratio}, \code{T_K}, \code{z}, \code{conc_out_mM}, \code{conc_in_mM}, \code{units}, \code{method}.
#' @export
Nernst <- function(T = 310.15, z = 1, conc_out = 5, conc_in = 140,
                   ion = "K+") {
  T <- as.numeric(T)
  z <- as.numeric(z)
  conc_out <- as.numeric(conc_out)
  conc_in <- as.numeric(conc_in)
  if (T <= 0) stop("T must be a positive absolute temperature in kelvin")
  if (z == 0) stop("z (ionic valence) must be non-zero")
  if (conc_out <= 0 || conc_in <= 0) {
    stop("ion concentrations must be positive (mM)")
  }
  slope <- .BSA_R_GAS * T / (z * .BSA_FARADAY)
  volts <- slope * log(conc_out / conc_in)
  list(
    ion = as.character(ion),
    potential_mV = volts * 1000, potential_V = volts,
    slope_mV = slope * 1000, ratio = conc_out / conc_in,
    T_K = T, z = z, conc_out_mM = conc_out, conc_in_mM = conc_in,
    units = list(potential = "mV", T = "K", concentration = "mM"),
    method = paste(
      "Nernst equilibrium potential, Rangayyan (2024)",
      "eq. (7.139), Section 7.8.1"
    )
  )
}


# -- HhGate: Hodgkin-Huxley gating variables m, h, n at a clamped potential.
#' HhGate: Hodgkin-Huxley gating variables m, h, n at a clamped
#' potential
#'
#' Part of the rangayyan_phys implementation; see the file header for
#' the source it follows.
#'
#' @param V See Usage.
#' @param dt Defaults to \code{0.01}.
#' @param m Defaults to \code{NULL}.
#' @param h Defaults to \code{NULL}.
#' @param n Defaults to \code{NULL}.
#' @param steps Defaults to \code{1L}.
#' @return A list with \code{V_mV}, \code{dt_ms}, \code{steps}, \code{m}, \code{h}, \code{n}, \code{m_inf}, \code{h_inf}, \code{n_inf}, \code{tau_m_ms}, \code{tau_h_ms}, \code{tau_n_ms}, \code{alpha_per_ms}, \code{beta_per_ms}, \code{units}, \code{method}.
#' @export
HhGate <- function(V, dt = 0.01, m = NULL, h = NULL, n = NULL, steps = 1L) {
  V <- as.numeric(V)
  dt <- as.numeric(dt)
  if (dt <= 0) stop("dt must be positive (ms)")
  steps <- as.integer(steps)
  if (steps < 1L) stop("steps must be at least 1")
  rt <- .bsahhrates(V)
  out <- list()
  spec <- list(
    list("m", rt[["am"]], rt[["bm"]], m),
    list("h", rt[["ah"]], rt[["bh"]], h),
    list("n", rt[["an"]], rt[["bn"]], n)
  )
  for (s in spec) {
    nm <- s[[1L]]
    a <- s[[2L]]
    b <- s[[3L]]
    x0 <- s[[4L]]
    tot <- a + b
    if (tot <= 0) stop(paste0("degenerate rate constants for gate ", nm))
    xinf <- a / tot
    tau <- 1 / tot
    if (is.null(x0)) {
      x <- xinf
    } else {
      x <- as.numeric(x0)
      if (!(x >= 0 && x <= 1)) stop(paste0("gate ", nm, " must start in [0, 1]"))
      x <- xinf + (x - xinf) * exp(-steps * dt / tau)
    }
    out[[nm]] <- x
    out[[paste0(nm, "_inf")]] <- xinf
    out[[paste0("tau_", nm, "_ms")]] <- tau
  }
  list(
    V_mV = V, dt_ms = dt, steps = steps,
    m = out$m, h = out$h, n = out$n,
    m_inf = out$m_inf, h_inf = out$h_inf, n_inf = out$n_inf,
    tau_m_ms = out$tau_m_ms, tau_h_ms = out$tau_h_ms,
    tau_n_ms = out$tau_n_ms,
    alpha_per_ms = list(m = rt[["am"]], h = rt[["ah"]], n = rt[["an"]]),
    beta_per_ms = list(m = rt[["bm"]], h = rt[["bh"]], n = rt[["bn"]]),
    units = list(
      V = "mV", time = "ms", rates = "1/ms",
      gates = "dimensionless"
    ),
    method = paste(
      "Hodgkin & Huxley (1952) J Physiol 117(4):500-544",
      "gating kinetics; rates not printed in",
      "Rangayyan (2024)"
    )
  )
}


# -- HhModel: four-variable Hodgkin-Huxley membrane model, RK4.
#' HhModel: four-variable Hodgkin-Huxley membrane model, RK4
#'
#' Part of the rangayyan_phys implementation; see the file header for
#' the source it follows.
#'
#' @param duration Defaults to \code{30}.
#' @param dt Defaults to \code{0.01}.
#' @param I_ext Defaults to \code{10}.
#' @param stim_start Defaults to \code{5}.
#' @param stim_stop Defaults to \code{6}.
#' @param C_m Defaults to \code{1}.
#' @param g_Na Defaults to \code{120}.
#' @param g_K Defaults to \code{36}.
#' @param g_L Defaults to \code{0.3}.
#' @param E_Na Defaults to \code{50}.
#' @param E_K Defaults to \code{-77}.
#' @param E_L Defaults to \code{-54.387}.
#' @param V0 Defaults to \code{-65}.
#' @return A list with \code{t_ms}, \code{V_mV}, \code{m}, \code{h}, \code{n}, \code{I_Na_uA_cm2}, \code{I_K_uA_cm2}, \code{I_L_uA_cm2}, \code{peak_mV}, \code{peak_time_ms}, \code{rest_mV}, \code{min_mV}, \code{spiked}, \code{n_spikes}, \code{dt_ms}, \code{I_ext_uA_cm2}, \code{units}, \code{method}.
#' @export
HhModel <- function(duration = 30, dt = 0.01, I_ext = 10, stim_start = 5,
                    stim_stop = 6, C_m = 1, g_Na = 120, g_K = 36, g_L = 0.3,
                    E_Na = 50, E_K = -77, E_L = -54.387, V0 = -65) {
  duration <- as.numeric(duration)
  dt <- as.numeric(dt)
  if (duration <= 0) stop("duration must be positive (ms)")
  if (!(dt > 0 && dt <= 1)) {
    stop("dt must be in (0, 1] ms for a stable RK4 integration")
  }
  if (stim_stop < stim_start) stop("stim_stop must not precede stim_start")
  if (C_m <= 0) stop("C_m must be positive (uF/cm^2)")
  if (min(as.numeric(g_Na), as.numeric(g_K), as.numeric(g_L)) < 0) {
    stop("conductances must be non-negative (mS/cm^2)")
  }
  C_m <- as.numeric(C_m)
  g_Na <- as.numeric(g_Na)
  g_K <- as.numeric(g_K)
  g_L <- as.numeric(g_L)
  E_Na <- as.numeric(E_Na)
  E_K <- as.numeric(E_K)
  E_L <- as.numeric(E_L)
  I_ext <- as.numeric(I_ext)
  stim_start <- as.numeric(stim_start)
  stim_stop <- as.numeric(stim_stop)
  stim <- function(tt) if (stim_start <= tt && tt < stim_stop) I_ext else 0
  deriv <- function(tt, V, m, h, n) {
    rt <- .bsahhrates(V)
    iNa <- g_Na * m^3 * h * (V - E_Na)
    iK <- g_K * n^4 * (V - E_K)
    iL <- g_L * (V - E_L)
    c(
      (stim(tt) - iNa - iK - iL) / C_m,
      rt[["am"]] * (1 - m) - rt[["bm"]] * m,
      rt[["ah"]] * (1 - h) - rt[["bh"]] * h,
      rt[["an"]] * (1 - n) - rt[["bn"]] * n
    )
  }
  V <- as.numeric(V0)
  rt <- .bsahhrates(V)
  m <- rt[["am"]] / (rt[["am"]] + rt[["bm"]])
  h <- rt[["ah"]] / (rt[["ah"]] + rt[["bh"]])
  n <- rt[["an"]] / (rt[["an"]] + rt[["bn"]])
  nsteps <- as.integer(round(duration / dt))
  ts <- numeric(nsteps + 1L)
  Vs <- ts
  ms <- ts
  hs <- ts
  ns <- ts
  ts[1L] <- 0
  Vs[1L] <- V
  ms[1L] <- m
  hs[1L] <- h
  ns[1L] <- n
  for (i in seq_len(nsteps)) {
    tt <- (i - 1L) * dt
    y <- c(V, m, h, n)
    k1 <- deriv(tt, y[1L], y[2L], y[3L], y[4L])
    z <- y + dt / 2 * k1
    k2 <- deriv(tt + dt / 2, z[1L], z[2L], z[3L], z[4L])
    z <- y + dt / 2 * k2
    k3 <- deriv(tt + dt / 2, z[1L], z[2L], z[3L], z[4L])
    z <- y + dt * k3
    k4 <- deriv(tt + dt, z[1L], z[2L], z[3L], z[4L])
    y <- y + dt / 6 * (k1 + 2 * k2 + 2 * k3 + k4)
    V <- y[1L]
    m <- min(1, max(0, y[2L]))
    h <- min(1, max(0, y[3L]))
    n <- min(1, max(0, y[4L]))
    ts[i + 1L] <- i * dt
    Vs[i + 1L] <- V
    ms[i + 1L] <- m
    hs[i + 1L] <- h
    ns[i + 1L] <- n
  }
  iNa <- g_Na * ms^3 * hs * (Vs - E_Na)
  iK <- g_K * ns^4 * (Vs - E_K)
  iL <- g_L * (Vs - E_L)
  peak <- max(Vs)
  pk <- which(Vs == peak)[1L]
  rest_idx <- max(0L, as.integer(stim_start / dt) - 1L)
  crossings <- sum(Vs[-length(Vs)] <= 0 & Vs[-1L] > 0)
  list(
    t_ms = ts, V_mV = Vs, m = ms, h = hs, n = ns,
    I_Na_uA_cm2 = iNa, I_K_uA_cm2 = iK, I_L_uA_cm2 = iL,
    peak_mV = peak, peak_time_ms = ts[pk],
    rest_mV = Vs[rest_idx + 1L], min_mV = min(Vs),
    spiked = peak > 0, n_spikes = as.integer(crossings),
    dt_ms = dt, I_ext_uA_cm2 = I_ext,
    units = list(
      V = "mV", t = "ms", I = "uA/cm^2",
      g = "mS/cm^2", C_m = "uF/cm^2"
    ),
    method = paste(
      "Hodgkin & Huxley (1952) J Physiol 117(4):500-544,",
      "four-variable model, RK4; Rangayyan (2024)",
      "eq. (7.138), Section 7.8.1"
    )
  )
}


# -- Fhn: FitzHugh-Nagumo two-variable excitable-medium neuron model.
#' Fhn: FitzHugh-Nagumo two-variable excitable-medium neuron model
#'
#' Part of the rangayyan_phys implementation; see the file header for
#' the source it follows.
#'
#' @param duration Defaults to \code{200}.
#' @param dt Defaults to \code{0.01}.
#' @param I_ext Defaults to \code{0.5}.
#' @param a Defaults to \code{0.7}.
#' @param b Defaults to \code{0.8}.
#' @param eps Defaults to \code{0.08}.
#' @param v0 Defaults to \code{-1.2}.
#' @param w0 Defaults to \code{-0.6}.
#' @param stim_start Defaults to \code{0}.
#' @param stim_stop Defaults to \code{NULL}.
#' @return A list with \code{t}, \code{v}, \code{w}, \code{peak}, \code{min}, \code{n_spikes}, \code{spike_times}, \code{period}, \code{a}, \code{b}, \code{eps}, \code{I_ext}, \code{units}, \code{method}.
#' @export
Fhn <- function(duration = 200, dt = 0.01, I_ext = 0.5, a = 0.7, b = 0.8,
                eps = 0.08, v0 = -1.2, w0 = -0.6, stim_start = 0,
                stim_stop = NULL) {
  duration <- as.numeric(duration)
  dt <- as.numeric(dt)
  if (duration <= 0) stop("duration must be positive")
  if (!(dt > 0 && dt <= 1)) {
    stop("dt must be in (0, 1] for a stable RK4 integration")
  }
  a <- as.numeric(a)
  b <- as.numeric(b)
  eps <- as.numeric(eps)
  if (eps <= 0) stop("eps must be positive (fast/slow time-scale ratio)")
  if (b <= 0) stop("b must be positive for a restoring recovery variable")
  I_ext <- as.numeric(I_ext)
  stim_start <- as.numeric(stim_start)
  stop_t <- if (is.null(stim_stop)) duration else as.numeric(stim_stop)
  if (stop_t < stim_start) stop("stim_stop must not precede stim_start")
  deriv <- function(tt, v, w) {
    cur <- if (stim_start <= tt && tt < stop_t) I_ext else 0
    c(v - v^3 / 3 - w + cur, eps * (v + a - b * w))
  }
  v <- as.numeric(v0)
  w <- as.numeric(w0)
  nsteps <- as.integer(round(duration / dt))
  ts <- numeric(nsteps + 1L)
  vs <- ts
  ws <- ts
  ts[1L] <- 0
  vs[1L] <- v
  ws[1L] <- w
  for (i in seq_len(nsteps)) {
    tt <- (i - 1L) * dt
    k1 <- deriv(tt, v, w)
    k2 <- deriv(tt + dt / 2, v + dt / 2 * k1[1L], w + dt / 2 * k1[2L])
    k3 <- deriv(tt + dt / 2, v + dt / 2 * k2[1L], w + dt / 2 * k2[2L])
    k4 <- deriv(tt + dt, v + dt * k3[1L], w + dt * k3[2L])
    v <- v + dt / 6 * (k1[1L] + 2 * k2[1L] + 2 * k3[1L] + k4[1L])
    w <- w + dt / 6 * (k1[2L] + 2 * k2[2L] + 2 * k3[2L] + k4[2L])
    ts[i + 1L] <- i * dt
    vs[i + 1L] <- v
    ws[i + 1L] <- w
  }
  spikes <- ts[-1L][vs[-length(vs)] <= 1 & vs[-1L] > 1]
  period <- if (length(spikes) > 1L) {
    (spikes[length(spikes)] - spikes[1L]) / (length(spikes) - 1L)
  } else {
    NULL
  }
  list(
    t = ts, v = vs, w = ws,
    peak = max(vs), min = min(vs),
    n_spikes = length(spikes), spike_times = spikes, period = period,
    a = a, b = b, eps = eps, I_ext = I_ext,
    units = list(
      v = "dimensionless", w = "dimensionless",
      t = "dimensionless model time units"
    ),
    method = paste(
      "FitzHugh (1961) Biophys J 1(6):445-466 / Nagumo et al.",
      "(1962) Proc IRE 50(10):2061-2070; equations not printed",
      "in Rangayyan (2024), named in Section 7.8.3"
    )
  )
}


# -- RcMemb: passive RC membrane potential dynamics.
#' RcMemb: passive RC membrane potential dynamics
#'
#' Part of the rangayyan_phys implementation; see the file header for
#' the source it follows.
#'
#' @param t See Usage.
#' @param I_inj Defaults to \code{0}.
#' @param C_m Defaults to \code{0.2}.
#' @param R_m Defaults to \code{100}.
#' @param V_rest Defaults to \code{-65}.
#' @return A list with \code{t_ms}, \code{V_mV}, \code{tau_ms}, \code{V_steady_mV}, \code{input_resistance_MOhm}, \code{peak_mV}, \code{final_mV}, \code{V_rest_mV}, \code{units}, \code{method}.
#' @export
RcMemb <- function(t, I_inj = 0, C_m = 0.2, R_m = 100, V_rest = -65) {
  ts <- as.numeric(t)
  if (length(ts) < 1L) {
    stop("t must contain at least one time point (ms)")
  }
  if (any(diff(ts) < 0)) stop("t must be non-decreasing (ms)")
  C_m <- as.numeric(C_m)
  R_m <- as.numeric(R_m)
  V_rest <- as.numeric(V_rest)
  if (C_m <= 0) stop("C_m must be positive (nF)")
  if (R_m <= 0) stop("R_m must be positive (MOhm)")
  cur <- as.numeric(I_inj)
  if (length(cur) == 1L) {
    cur <- rep(cur, length(ts))
  } else if (length(cur) != length(ts)) {
    stop("I_inj must be scalar or the same length as t")
  }
  tau <- R_m * C_m
  V <- V_rest
  Vs <- numeric(length(ts))
  for (i in seq_along(ts)) {
    if (i > 1L) {
      step <- ts[i] - ts[i - 1L]
      vinf <- V_rest + cur[i - 1L] * R_m
      V <- vinf + (V - vinf) * exp(-step / tau)
    }
    Vs[i] <- V
  }
  list(
    t_ms = ts, V_mV = Vs, tau_ms = tau,
    V_steady_mV = V_rest + cur[length(cur)] * R_m,
    input_resistance_MOhm = R_m,
    peak_mV = max(Vs), final_mV = Vs[length(Vs)], V_rest_mV = V_rest,
    units = list(
      V = "mV", t = "ms", I = "nA", R = "MOhm",
      C = "nF", tau = "ms"
    ),
    method = paste(
      "Passive RC membrane, leak-only reduction of",
      "Rangayyan (2024) eq. (7.138), Section 7.8.1"
    )
  )
}


# -- BiDomain: 1-D monodomain propagation with the bidomain extracellular
#    field; Rangayyan (2024) eqs. (7.143)-(7.149), Section 7.8.2.
#' BiDomain: 1-D monodomain propagation with the bidomain extracellular
#'
#' field; Rangayyan (2024) eqs. (7.143)-(7.149), Section 7.8.2.
#'
#' @param n_nodes Defaults to \code{100}.
#' @param dx_cm Defaults to \code{0.02}.
#' @param duration_ms Defaults to \code{60}.
#' @param dt_ms Defaults to \code{0.005}.
#' @param sigma_i Defaults to \code{1}.
#' @param sigma_e Defaults to \code{2}.
#' @param C_m Defaults to \code{1}.
#' @param Sv Defaults to \code{1000}.
#' @param I_ion Defaults to \code{NULL}.
#' @param stim_nodes Defaults to \code{5}.
#' @param I_stim Defaults to \code{50}.
#' @param stim_ms Defaults to \code{1}.
#' @param v_rest Defaults to \code{-85}.
#' @param v_peak Defaults to \code{20}.
#' @param I_ion_peak Defaults to \code{10}.
#' @param threshold_frac Defaults to \code{0.25}.
#' @return A list with \code{x_cm}, \code{Vm_mV}, \code{phi_e_mV}, \code{phi_i_mV}, \code{Im_uA_cm2}, \code{activation_ms}, \code{n_activated}, \code{cv_cm_per_ms}, \code{D_cm2_per_ms}, \code{sigma_bulk_mS_cm}, \code{dt_ms}, \code{dx_cm}, \code{stability_limit_ms}, \code{units}, \code{method}.
#' @export
BiDomain <- function(n_nodes = 100, dx_cm = 0.02, duration_ms = 60,
                     dt_ms = 0.005, sigma_i = 1, sigma_e = 2, C_m = 1,
                     Sv = 1000, I_ion = NULL, stim_nodes = 5, I_stim = 50,
                     stim_ms = 1, v_rest = -85, v_peak = 20,
                     I_ion_peak = 10, threshold_frac = 0.25) {
  n <- as.integer(n_nodes)
  if (n < 5L) stop("n_nodes must be at least 5")
  dx <- as.numeric(dx_cm)
  dt <- as.numeric(dt_ms)
  if (dx <= 0) stop("dx_cm must be positive (cm)")
  if (dt <= 0) stop("dt_ms must be positive (ms)")
  dur <- as.numeric(duration_ms)
  if (dur <= 0) stop("duration_ms must be positive (ms)")
  si <- as.numeric(sigma_i)
  se <- as.numeric(sigma_e)
  if (si <= 0 || se <= 0) stop("sigma_i and sigma_e must be positive (mS/cm)")
  C_m <- as.numeric(C_m)
  Sv <- as.numeric(Sv)
  if (C_m <= 0) stop("C_m must be positive (uF/cm^2)")
  if (Sv <= 0) stop("Sv must be positive (1/cm)")
  v_rest <- as.numeric(v_rest)
  v_peak <- as.numeric(v_peak)
  if (v_peak <= v_rest) stop("v_peak must exceed v_rest (mV)")
  ns <- as.integer(stim_nodes)
  if (!(ns >= 1L && ns <= n)) {
    stop("stim_nodes must be between 1 and n_nodes")
  }
  sigma_bulk <- si * se / (si + se)
  D <- sigma_bulk / (Sv * C_m) # eq. (7.144), cm^2/ms
  lim <- dx * dx / (2 * D)
  if (dt > lim) {
    stop(sprintf(paste(
      "dt_ms=%g exceeds the explicit stability limit %g ms",
      "for D=%g cm^2/ms and dx=%g cm"
    ), dt, lim, D, dx))
  }
  tf <- as.numeric(threshold_frac)
  if (!(tf > 0 && tf < 0.5)) {
    stop(paste(
      "threshold_frac must be strictly between 0 and 0.5;",
      "0.5 gives a stationary front that never propagates"
    ))
  }
  amp <- v_peak - v_rest
  thresh <- v_rest + tf * amp
  if (is.null(I_ion)) {
    ipk <- as.numeric(I_ion_peak)
    if (ipk <= 0) stop("I_ion_peak must be positive (uA/cm^2)")
    vk <- v_rest + amp * (0:500) / 500
    raw <- max(abs((vk - v_rest) * (vk - thresh) * (vk - v_peak)))
    gain <- ipk / raw
    I_ion <- function(v) gain * (v - v_rest) * (v - thresh) * (v - v_peak)
  } else if (!is.function(I_ion)) {
    stop("I_ion must be callable: I_ion(V_mV) -> uA/cm^2")
  }
  V <- rep(v_rest, n)
  act <- rep(NA_real_, n)
  nsteps <- as.integer(round(dur / dt))
  stim_steps <- as.integer(round(as.numeric(stim_ms) / dt))
  I_stim <- as.numeric(I_stim)
  iapp0 <- rep(0, n)
  iapp1 <- c(rep(-I_stim, ns), rep(0, n - ns))
  dVdt <- rep(0, n)
  for (step in seq_len(nsteps)) {
    lo <- c(V[2L], V[seq_len(n - 1L)]) # eq. (7.145), no flux
    hi <- c(V[-1L], V[n - 1L])
    lap <- (lo - 2 * V + hi) / (dx * dx)
    iapp <- if (step - 1L < stim_steps) iapp1 else iapp0
    dVdt <- D * lap - (I_ion(V) + iapp) / C_m
    V <- V + dt * dVdt
    tt <- step * dt
    fresh <- is.na(act) & V >= thresh
    if (any(fresh)) act[fresh] <- tt
  }
  lo <- c(V[2L], V[seq_len(n - 1L)])
  hi <- c(V[-1L], V[n - 1L])
  rhs <- -si * (lo - 2 * V + hi) / (dx * dx)
  # Thomas algorithm on (si+se)/dx^2 * tridiag(1, -2, 1), node 1 pinned.
  k <- (si + se) / (dx * dx)
  a <- rep(0, n)
  b <- rep(0, n)
  cc <- rep(0, n)
  d <- rhs
  b[1L] <- 1
  cc[1L] <- 0
  d[1L] <- 0 # gauge pin
  if (n > 2L) {
    mid <- 2:(n - 1L)
    a[mid] <- k
    b[mid] <- -2 * k
    cc[mid] <- k
  }
  a[n] <- k
  b[n] <- -k
  for (i in 2:n) {
    if (b[i - 1L] == 0) {
      stop("singular extracellular system; check conductivities")
    }
    mm <- a[i] / b[i - 1L]
    b[i] <- b[i] - mm * cc[i - 1L]
    d[i] <- d[i] - mm * d[i - 1L]
  }
  phie <- rep(0, n)
  if (b[n] == 0) {
    stop("singular extracellular system; check conductivities")
  }
  phie[n] <- d[n] / b[n]
  for (i in (n - 1L):1L) phie[i] <- (d[i] - cc[i] * phie[i + 1L]) / b[i]
  phie <- phie - .morie_fsum(phie) / n
  phii <- V + phie # eq. (7.146)
  Im <- C_m * dVdt + I_ion(V) # eq. (7.149)
  xs <- (seq_len(n) - 1L) * dx
  keep <- !is.na(act)
  cv <- NULL
  if (sum(keep) > 2L) {
    tt <- act[keep]
    xx <- xs[keep]
    mt <- .morie_fsum(tt) / length(tt)
    mx <- .morie_fsum(xx) / length(xx)
    den <- .morie_fsum((tt - mt) * (tt - mt))
    if (den > 0) cv <- .morie_fsum((tt - mt) * (xx - mx)) / den
  }
  list(
    x_cm = xs, Vm_mV = V, phi_e_mV = phie, phi_i_mV = phii,
    Im_uA_cm2 = Im, activation_ms = act, n_activated = sum(keep),
    cv_cm_per_ms = cv,
    D_cm2_per_ms = D, sigma_bulk_mS_cm = sigma_bulk,
    dt_ms = dt, dx_cm = dx, stability_limit_ms = lim,
    units = list(
      V = "mV", x = "cm", t = "ms", sigma = "mS/cm",
      C_m = "uF/cm^2", Sv = "1/cm", I = "uA/cm^2",
      D = "cm^2/ms", cv = "cm/ms"
    ),
    method = paste(
      "Rangayyan (2024) eqs. (7.143)-(7.149), Section 7.8.2,",
      "monodomain propagation with the bidomain",
      "extracellular field, 1-D"
    )
  )
}


# -- CadAcou: coronary artery disease detection from acoustic signals.
#' CadAcou: coronary artery disease detection from acoustic signals
#'
#' Part of the rangayyan_phys implementation; see the file header for
#' the source it follows.
#'
#' @param coronary_sound See Usage.
#' @param fs See Usage.
#' @param order Defaults to \code{8}.
#' @param hf_band Defaults to \code{c(300, 900)}.
#' @param ref_band Defaults to \code{c(50, 300)}.
#' @return A vector, from \code{c}.
#' @export
CadAcou <- function(coronary_sound, fs, order = 8, hf_band = c(300, 900),
                    ref_band = c(50, 300)) {
  xs <- as.numeric(coronary_sound)
  fs <- as.numeric(fs)
  if (fs <= 0) stop("fs must be positive (Hz)")
  order <- as.integer(order)
  if (order < 2L) stop("order must be at least 2")
  if (length(xs) < 4L * order) {
    stop("need at least 4*order samples to fit the AR model")
  }
  for (bd in list(hf_band, ref_band)) {
    if (as.numeric(bd[2L]) <= as.numeric(bd[1L])) {
      stop("each band must have hi > lo (Hz)")
    }
    if (as.numeric(bd[2L]) > fs / 2) {
      stop("band upper edge exceeds the Nyquist frequency")
    }
  }
  lp <- .bsalpc(xs, order)
  sp <- .bsalpcspec(lp$a, fs)
  freqs <- sp$freqs
  psd <- lp$e * sp$power
  hf <- .bsabandpow(freqs, psd, as.numeric(hf_band[1L]), as.numeric(hf_band[2L]))
  rf <- .bsabandpow(
    freqs, psd, as.numeric(ref_band[1L]),
    as.numeric(ref_band[2L])
  )
  if (rf <= 0) {
    stop("reference band carries no power; choose another band")
  }
  mom <- .bsapsdmom(freqs, psd)
  pk <- .bsapeaks(freqs, psd, count = 4L, minsep = fs / 200)
  c(mom, list(
    power_ratio = hf / rf,
    hf_fraction = hf / mom$total_power,
    hf_band_hz = c(as.numeric(hf_band[1L]), as.numeric(hf_band[2L])),
    ref_band_hz = c(as.numeric(ref_band[1L]), as.numeric(ref_band[2L])),
    ar_coeffs = lp$a, prediction_error = lp$e, order = order,
    ar_peaks_hz = Map(c, pk$freqs, pk$powers),
    freq_hz = freqs, ar_psd = psd, fs_hz = fs,
    units = list(frequency = "Hz", ratios = "dimensionless"),
    method = paste(
      "Rangayyan (2024) Section 7.10 with AR modelling of",
      "Section 7.5 and the spectral power ratio of eq. (6.44),",
      "Section 6.4.2"
    )
  ))
}


# -- CorSound: coronary artery sound generation model (turbulent flow);
#    Rangayyan (2024) eqs. (7.135)-(7.136), Section 7.7.2.
#' CorSound: coronary artery sound generation model (turbulent flow);
#'
#' Rangayyan (2024) eqs. (7.135)-(7.136), Section 7.7.2.
#'
#' @param diameter See Usage.
#' @param flow_velocity See Usage.
#' @param stenosis_pct Defaults to \code{0}.
#' @param p2max Defaults to \code{1}.
#' @param freqs Defaults to \code{NULL}.
#' @param nu Defaults to \code{3.5e-06}.
#' @return A list with \code{freq_hz}, \code{psd_Pa2_per_Hz}, \code{D_normal_m}, \code{d_stenotic_m}, \code{U_normal_m_s}, \code{u_stenotic_m_s}, \code{stenosis_pct}, \code{corner_freq_hz}, \code{reynolds_param_x}, \code{reynolds_number}, \code{total_power_Pa2}, \code{units}, \code{method}.
#' @export
CorSound <- function(diameter, flow_velocity, stenosis_pct = 0, p2max = 1,
                     freqs = NULL, nu = 3.5e-6) {
  D <- as.numeric(diameter)
  U <- as.numeric(flow_velocity)
  if (D <= 0) stop("diameter must be positive (m)")
  if (U <= 0) stop("flow_velocity must be positive (m/s)")
  s <- as.numeric(stenosis_pct)
  if (!(s >= 0 && s < 100)) stop("stenosis_pct must be in [0, 100) percent")
  p2max <- as.numeric(p2max)
  if (p2max <= 0) stop("p2max must be positive (Pa^2)")
  nu <- as.numeric(nu)
  if (nu <= 0) stop("nu must be positive (m^2/s)")
  open_frac <- 1 - s / 100
  d <- D * sqrt(open_frac)
  u <- U / open_frac
  if (is.null(freqs)) {
    fs_hz <- as.numeric(1:1000)
  } else {
    fs_hz <- as.numeric(freqs)
    if (any(fs_hz < 0)) stop("frequencies must be non-negative (Hz)")
  }
  tau <- d / U # seconds
  psd <- 0.7 * tau * p2max / (1 + 0.5 * fs_hz * tau)^(10 / 3)
  x <- 1e-3 * (u * d / nu) * (D / d)^0.75
  list(
    freq_hz = fs_hz, psd_Pa2_per_Hz = psd,
    D_normal_m = D, d_stenotic_m = d,
    U_normal_m_s = U, u_stenotic_m_s = u,
    stenosis_pct = s,
    corner_freq_hz = 2 / tau,
    reynolds_param_x = x,
    reynolds_number = u * d / nu,
    total_power_Pa2 = .morie_fsum(psd) *
      (if (length(fs_hz) > 1L) fs_hz[2L] - fs_hz[1L] else 1),
    units = list(
      freq = "Hz", psd = "Pa^2/Hz", diameter = "m",
      velocity = "m/s", nu = "m^2/s"
    ),
    method = paste(
      "Rangayyan (2024) eqs. (7.135) and (7.136),",
      "Section 7.7.2, after Wang et al. (1990) and Fredberg"
    )
  )
}


# -- InfantCry: cry F0 track, melody coding and formants.
#' InfantCry: cry F0 track, melody coding and formants
#'
#' Part of the rangayyan_phys implementation; see the file header for
#' the source it follows.
#'
#' @param cry See Usage.
#' @param fs See Usage.
#' @param window_ms Defaults to \code{40}.
#' @param f0_range Defaults to \code{c(200, 1000)}.
#' @param order Defaults to \code{NULL}.
#' @param flat_tolerance Defaults to \code{0.06}.
#' @return A list with \code{f0_track_hz}, \code{t_track_s}, \code{melody}, \code{melody_units}, \code{mean_f0_hz}, \code{sd_f0_hz}, \code{min_f0_hz}, \code{max_f0_hz}, \code{f0_range_semitones}, \code{voiced_fraction}, \code{in_common_band_fraction}, \code{high_pitched}, \code{formants_hz}, \code{window_ms}, \code{n_windows}, \code{fs_hz}, \code{flat_tolerance}, \code{units}, \code{method}.
#' @export
InfantCry <- function(cry, fs, window_ms = 40, f0_range = c(200, 1000),
                      order = NULL, flat_tolerance = 0.06) {
  xs <- as.numeric(cry)
  fs <- as.numeric(fs)
  if (fs <= 0) stop("fs must be positive (Hz)")
  flo <- as.numeric(f0_range[1L])
  fhi <- as.numeric(f0_range[2L])
  if (!(flo > 0 && flo < fhi)) stop("f0_range must satisfy 0 < lo < hi (Hz)")
  if (fhi * 2 > fs) {
    stop("fs must exceed twice the upper edge of f0_range")
  }
  window_ms <- as.numeric(window_ms)
  if (window_ms <= 0) stop("window_ms must be positive (ms)")
  w <- as.integer(round(window_ms * fs / 1000))
  if (w < as.integer(2 * fs / flo)) {
    stop(sprintf(
      "a %g ms window holds fewer than two periods at %g Hz",
      window_ms, flo
    ))
  }
  nwin <- length(xs) %/% w
  if (nwin < 2L) stop("need at least 2 whole analysis windows")
  tol <- as.numeric(flat_tolerance)
  if (!(tol > 0 && tol < 1)) {
    stop("flat_tolerance must be a relative change in (0, 1)")
  }
  lag_lo <- max(1L, as.integer(fs / fhi))
  lag_hi <- min(w - 1L, as.integer(fs / flo))
  if (lag_hi <= lag_lo) {
    stop("window is too short for the requested f0_range")
  }
  track <- vector("list", nwin)
  tt <- numeric(nwin)
  best_seg <- NULL
  best_rms <- -1
  for (i in seq_len(nwin)) {
    seg <- xs[((i - 1L) * w + 1L):(i * w)]
    tt[i] <- (i - 1L) * w / fs
    acf <- tryCatch(.bsaacf(seg, lag_hi), error = function(e) NULL)
    if (is.null(acf)) next # element stays NULL
    if (acf[1L] <= 0) next
    sub <- acf[(lag_lo + 1L):(lag_hi + 1L)]
    k <- lag_lo + which.max(sub) - 1L
    if (acf[k + 1L] / acf[1L] < 0.3) next
    track[[i]] <- fs / k
    r <- .bsarms(seg)
    if (r > best_rms) {
      best_rms <- r
      best_seg <- seg
    }
  }
  voiced <- unlist(track[!vapply(track, is.null, logical(1))])
  if (!length(voiced)) {
    stop("no voiced window found; check f0_range and the recording")
  }
  melody <- integer(0)
  prev <- NULL
  for (i in seq_len(nwin)) {
    v <- track[[i]]
    if (is.null(v)) next
    if (!is.null(prev)) {
      ratio <- v / prev
      melody <- c(melody, if (abs(ratio - 1) < tol) {
        0L
      } else if (ratio > 1) 1L else -1L)
    }
    prev <- v
  }
  n <- length(voiced)
  mf <- .morie_fsum(voiced) / n
  sf <- if (n > 1L) {
    sqrt(.morie_fsum((voiced - mf) * (voiced - mf)) / (n - 1L))
  } else {
    0
  }
  fmt <- numeric(0)
  if (!is.null(best_seg)) {
    p <- if (!is.null(order)) as.integer(order) else as.integer(2 + fs / 1000)
    if (length(best_seg) >= 4L * p && p >= 4L) {
      fmt <- tryCatch(
        {
          lp <- .bsalpc(best_seg, p)
          sp <- .bsalpcspec(lp$a, fs, npts = 1024L)
          sort(.bsapeaks(sp$freqs, sp$power, count = 4L, minsep = 150)$freqs)
        },
        error = function(e) numeric(0)
      )
    }
  }
  list(
    f0_track_hz = track, t_track_s = tt, melody = melody,
    melody_units = list(`-1` = "falling", `0` = "flat", `1` = "rising"),
    mean_f0_hz = mf, sd_f0_hz = sf,
    min_f0_hz = min(voiced), max_f0_hz = max(voiced),
    f0_range_semitones = 12 * log(max(voiced) / min(voiced), 2),
    voiced_fraction = n / nwin,
    in_common_band_fraction = sum(voiced >= 300 & voiced <= 600) / n,
    high_pitched = mf > 1000,
    formants_hz = fmt,
    window_ms = window_ms, n_windows = nwin, fs_hz = fs,
    flat_tolerance = tol,
    units = list(
      frequency = "Hz", time = "s",
      f0_range_semitones = "semitones"
    ),
    method = paste(
      "Rangayyan (2024) Section 8.13 after Varallyay: 40 ms",
      "windows, F0 track as the cry melody coded",
      "falling/flat/rising"
    )
  )
}


# -- EggFeat: electrogastrogram dominant frequency and band powers.
#' EggFeat: electrogastrogram dominant frequency and band powers
#'
#' Part of the rangayyan_phys implementation; see the file header for
#' the source it follows.
#'
#' @param egg See Usage.
#' @param fs See Usage.
#' @param normal_band Defaults to \code{c(0.0333, 0.0667)}.
#' @return A vector, from \code{c}.
#' @export
EggFeat <- function(egg, fs, normal_band = c(0.0333, 0.0667)) {
  fs <- as.numeric(fs)
  if (fs <= 0) stop("fs must be positive (Hz)")
  lo <- as.numeric(normal_band[1L])
  hi <- as.numeric(normal_band[2L])
  if (!(lo > 0 && lo < hi)) stop("normal_band must satisfy 0 < lo < hi (Hz)")
  if (hi > fs / 2) {
    stop("normal_band upper edge exceeds the Nyquist frequency")
  }
  xs <- as.numeric(egg)
  dur <- length(xs) / fs
  if (dur < 2 / lo) {
    stop(sprintf(paste(
      "recording of %.1f s is too short to resolve %g Hz;",
      "need at least %.0f s"
    ), dur, lo, 2 / lo))
  }
  sp <- .bsapsd(xs, fs)
  freqs <- sp$freqs
  psd <- sp$power
  mom <- .bsapsdmom(freqs, psd)
  tot <- mom$total_power
  fr_norm <- .bsabandpow(freqs, psd, lo, hi) / tot
  fr_brady <- .bsabandpow(freqs, psd, 0, lo) / tot
  fr_tachy <- .bsabandpow(freqs, psd, hi, fs / 2 + 1) / tot
  sel <- freqs > 0 & freqs <= min(0.5, fs / 2)
  if (!any(sel)) stop("no spectral bins in the gastric frequency range")
  bf <- freqs[sel]
  bp <- psd[sel]
  fdom <- bf[which.max(bp)]
  rhythm <- if (fdom >= lo && fdom < hi) {
    "normogastria"
  } else if (fdom < lo) "bradygastria" else "tachygastria"
  c(mom, list(
    dominant_freq_hz = fdom, dominant_freq_cpm = fdom * 60,
    normal_fraction = fr_norm, brady_fraction = fr_brady,
    tachy_fraction = fr_tachy, rhythm = rhythm,
    normal_band_hz = c(lo, hi), normal_band_cpm = c(lo * 60, hi * 60),
    duration_s = dur, fs_hz = fs,
    freq_hz = freqs, psd = psd,
    units = list(
      frequency = "Hz", dominant_freq_cpm = "cycles/minute",
      fractions = "dimensionless", duration = "s"
    ),
    method = paste(
      "Rangayyan (2024) Section 1.2.8 with the PSD measures of",
      "Section 6.4.1 and the band fraction of eq. (6.44)"
    )
  ))
}


# -- EngCap: ENG compound action potential from a fibre-velocity population.
#' EngCap: ENG compound action potential from a fibre-velocity
#' population
#'
#' Part of the rangayyan_phys implementation; see the file header for
#' the source it follows.
#'
#' @param t See Usage.
#' @param distance_m Defaults to \code{0.1}.
#' @param n_fibers Defaults to \code{40}.
#' @param cv_range Defaults to \code{c(45, 70)}.
#' @param amp_range Defaults to \code{c(0.5, 2)}.
#' @param width_ms Defaults to \code{0.3}.
#' @return A list with \code{t_ms}, \code{cap_uV}, \code{peak_uV}, \code{peak_latency_ms}, \code{onset_latency_ms}, \code{cv_from_peak_m_s}, \code{cv_from_onset_m_s}, \code{latencies_ms}, \code{velocities_m_s}, \code{distance_m}, \code{units}, \code{method}.
#' @export
EngCap <- function(t, distance_m = 0.1, n_fibers = 40, cv_range = c(45, 70),
                   amp_range = c(0.5, 2), width_ms = 0.3) {
  ts <- as.numeric(t)
  if (length(ts) < 3L) {
    stop("t must contain at least 3 time points (ms)")
  }
  distance_m <- as.numeric(distance_m)
  if (distance_m <= 0) stop("distance_m must be positive (m)")
  n_fibers <- as.integer(n_fibers)
  if (n_fibers < 1L) stop("n_fibers must be at least 1")
  lo <- as.numeric(cv_range[1L])
  hi <- as.numeric(cv_range[2L])
  if (lo <= 0 || hi <= 0 || lo > hi) {
    stop("cv_range must be positive with low <= high (m/s)")
  }
  alo <- as.numeric(amp_range[1L])
  ahi <- as.numeric(amp_range[2L])
  width_ms <- as.numeric(width_ms)
  if (width_ms <= 0) stop("width_ms must be positive (ms)")
  i0 <- seq_len(n_fibers) - 1L
  cvs <- if (n_fibers == 1L) lo else lo + (hi - lo) * i0 / (n_fibers - 1L)
  amps <- if (n_fibers == 1L) alo else alo + (ahi - alo) * i0 / (n_fibers - 1L)
  lats <- 1000 * distance_m / cvs # m / (m/s) = s; x1000 -> ms
  wave <- vapply(ts, function(ti) {
    u <- (ti - lats) / width_ms
    keep <- u >= 0 & u <= 30
    if (!any(keep)) {
      return(0)
    }
    .morie_fsum(amps[keep] * u[keep] * (2 - u[keep]) * exp(-u[keep]))
  }, numeric(1))
  pk <- max(abs(wave))
  if (pk <= 0) {
    stop(paste0(
      "CAP is identically zero; t must cover latencies (",
      min(lats), ", ", max(lats), ") ms"
    ))
  }
  ipk <- which.max(abs(wave))
  above <- which(abs(wave) >= 0.05 * pk)
  onset <- if (length(above)) ts[above[1L]] else NULL
  list(
    t_ms = ts, cap_uV = wave,
    peak_uV = wave[ipk], peak_latency_ms = ts[ipk],
    onset_latency_ms = onset,
    cv_from_peak_m_s = if (ts[ipk] > 0) {
      1000 * distance_m / ts[ipk]
    } else {
      NULL
    },
    cv_from_onset_m_s = if (!is.null(onset) && onset > 0) {
      1000 * distance_m / onset
    } else {
      NULL
    },
    latencies_ms = lats, velocities_m_s = cvs,
    distance_m = distance_m,
    units = list(t = "ms", cap = "uV", velocity = "m/s", distance = "m"),
    method = paste(
      "ENG compound action potential from a fibre-velocity",
      "population; ENG and two-point conduction-velocity",
      "measurement per Rangayyan (2024) Section 1.2.3",
      "(no CAP equation in the book)"
    )
  )
}


# -- SeizDet: epileptic seizure detection in EEG.
#' SeizDet: epileptic seizure detection in EEG
#'
#' Part of the rangayyan_phys implementation; see the file header for
#' the source it follows.
#'
#' @param eeg See Usage.
#' @param fs See Usage.
#' @param epoch_s Defaults to \code{1}.
#' @param ratio_threshold Defaults to \code{2}.
#' @param baseline_epochs Defaults to \code{NULL}.
#' @return A list with \code{epochs}, \code{seizure_detected}, \code{n_flagged}, \code{seizure_intervals_s}, \code{baseline_slow_fraction}, \code{threshold_slow_fraction}, \code{baseline_epochs}, \code{n_epochs}, \code{epoch_s}, \code{fs_hz}, \code{binnie_bands_hz}, \code{units}, \code{method}.
#' @export
SeizDet <- function(eeg, fs, epoch_s = 1, ratio_threshold = 2,
                    baseline_epochs = NULL) {
  xs <- as.numeric(eeg)
  fs <- as.numeric(fs)
  if (fs < 30) stop("fs must be at least 30 Hz to resolve the beta band")
  epoch_s <- as.numeric(epoch_s)
  if (epoch_s <= 0) stop("epoch_s must be positive (s)")
  ratio_threshold <- as.numeric(ratio_threshold)
  if (ratio_threshold <= 1) {
    stop("ratio_threshold must exceed 1 (it is a ratio to baseline)")
  }
  w <- as.integer(round(epoch_s * fs))
  if (w < 8L) {
    stop(sprintf(
      "epoch of %g s is only %d samples; use a longer epoch",
      epoch_s, w
    ))
  }
  n_ep <- length(xs) %/% w
  if (n_ep < 2L) stop("need at least 2 whole epochs")
  binnie <- list(
    c(1, 2), c(2, 4), c(4, 6), c(6, 8), c(8, 11), c(11, 14),
    c(14, fs / 2)
  )
  trad <- list(
    delta = c(0.5, 4), theta = c(4, 8), alpha = c(8, 13.0001),
    beta = c(13.0001, fs / 2), gamma = c(30, min(80, fs / 2))
  )
  rows <- vector("list", n_ep)
  for (e in seq_len(n_ep)) {
    seg <- xs[((e - 1L) * w + 1L):(e * w)]
    sp <- .bsapsd(seg, fs)
    tot <- .morie_fsum(sp$power)
    if (tot <= 0) {
      stop(sprintf("epoch %d is constant; no spectrum to analyse", e - 1L))
    }
    row <- list(t_start_s = (e - 1L) * epoch_s)
    for (bd in binnie) {
      row[[sprintf("b_%g_%g_hz", bd[1L], bd[2L])]] <-
        .bsabandpow(sp$freqs, sp$power, bd[1L], bd[2L]) / tot
    }
    for (nm in names(trad)) {
      bd <- trad[[nm]]
      if (bd[2L] > bd[1L]) {
        row[[paste0(nm, "_fraction")]] <-
          .bsabandpow(sp$freqs, sp$power, bd[1L], bd[2L]) / tot
      }
    }
    row$slow_fraction <- (if (is.null(row$delta_fraction)) {
      0
    } else {
      row$delta_fraction
    }) +
      (if (is.null(row$theta_fraction)) {
        0
      } else {
        row$theta_fraction
      })
    row$form_factor <- .bsahjorth(seg)$form_factor
    rows[[e]] <- row
  }
  nb <- if (!is.null(baseline_epochs)) {
    as.integer(baseline_epochs)
  } else {
    max(1L, n_ep %/% 4L)
  }
  if (!(nb >= 1L && nb < n_ep)) {
    stop("baseline_epochs must be between 1 and n_epochs-1")
  }
  base <- .morie_fsum(vapply(
    rows[seq_len(nb)],
    function(r) r$slow_fraction, numeric(1)
  )) / nb
  if (base <= 0) {
    stop("baseline has no slow-band power; cannot form a ratio")
  }
  thr <- ratio_threshold * base
  flg <- logical(n_ep)
  for (i in seq_len(n_ep)) {
    flg[i] <- rows[[i]]$slow_fraction > thr
    rows[[i]]$flagged <- flg[i]
  }
  runs <- list()
  start <- NULL
  for (i in seq_len(n_ep)) {
    if (flg[i] && is.null(start)) {
      start <- i - 1L
    } else if (!flg[i] && !is.null(start)) {
      runs[[length(runs) + 1L]] <- c(start * epoch_s, (i - 1L) * epoch_s)
      start <- NULL
    }
  }
  if (!is.null(start)) {
    runs[[length(runs) + 1L]] <- c(start * epoch_s, n_ep * epoch_s)
  }
  nflag <- sum(flg)
  list(
    epochs = rows,
    seizure_detected = nflag > 0L, n_flagged = as.integer(nflag),
    seizure_intervals_s = runs,
    baseline_slow_fraction = base, threshold_slow_fraction = thr,
    baseline_epochs = nb, n_epochs = n_ep,
    epoch_s = epoch_s, fs_hz = fs,
    binnie_bands_hz = binnie,
    units = list(eeg = "uV", time = "s", fractions = "dimensionless"),
    method = paste(
      "Rangayyan (2024) Section 8.17 with the spectral",
      "banding of Binnie et al., Section 6.4.2, the EEG bands",
      "of Section 1.2.6 and the form factor of eq. (5.26)"
    )
  )
}


# -- ErpFeat: event-related potential latency and amplitude features.
#' ErpFeat: event-related potential latency and amplitude features
#'
#' Part of the rangayyan_phys implementation; see the file header for
#' the source it follows.
#'
#' @param erp See Usage.
#' @param fs See Usage.
#' @param t0 Defaults to \code{0}.
#' @param components Defaults to \code{NULL}.
#' @param baseline_ms Defaults to \code{list(NULL, 0)}.
#' @return A list with \code{components}, \code{peak_to_peak_uV}, \code{baseline_uV}, \code{t_ms}, \code{erp_uV}, \code{fs_hz}, \code{t0_ms}, \code{units}, \code{method}.
#' @export
ErpFeat <- function(erp, fs, t0 = 0, components = NULL,
                    baseline_ms = list(NULL, 0)) {
  xs <- as.numeric(erp)
  fs <- as.numeric(fs)
  if (fs <= 0) stop("fs must be positive (Hz)")
  if (length(xs) < 4L) stop("need at least 4 ERP samples")
  t0 <- as.numeric(t0)
  ts <- 1000 * (seq_len(length(xs)) - 1L) / fs - t0
  bstart <- baseline_ms[[1L]]
  bend <- as.numeric(baseline_ms[[2L]])
  sel <- ts < bend & (if (is.null(bstart)) TRUE else ts >= as.numeric(bstart))
  base <- if (any(sel)) .morie_fsum(xs[sel]) / sum(sel) else 0
  ys <- xs - base
  if (is.null(components)) {
    components <- list(
      N100 = list(50, 150, -1L), P200 = list(150, 250, 1L),
      N200 = list(180, 300, -1L), P300 = list(250, 500, 1L)
    )
  }
  if (!is.list(components)) {
    stop("components must be a dict of name -> (t1, t2, polarity)")
  }
  rows <- list()
  for (nm in names(components)) {
    spec <- components[[nm]]
    t1 <- as.numeric(spec[[1L]])
    t2 <- as.numeric(spec[[2L]])
    pol <- as.integer(spec[[3L]])
    if (t2 <= t1) stop(sprintf("component %s has an empty window", nm))
    if (!(pol %in% c(1L, -1L))) {
      stop(sprintf("component %s polarity must be +1 or -1", nm))
    }
    win <- which(ts >= t1 & ts <= t2)
    if (!length(win)) {
      rows[[nm]] <- list(
        latency_ms = NULL, amplitude_uV = NULL,
        found = FALSE, window_ms = c(t1, t2)
      )
      next
    }
    i <- win[which.max(pol * ys[win])]
    rows[[nm]] <- list(
      latency_ms = ts[i], amplitude_uV = ys[i],
      found = TRUE, window_ms = c(t1, t2), polarity = pol
    )
  }
  list(
    components = rows,
    peak_to_peak_uV = max(ys) - min(ys),
    baseline_uV = base,
    t_ms = ts, erp_uV = ys, fs_hz = fs, t0_ms = t0,
    units = list(amplitude = "uV", latency = "ms"),
    method = paste(
      "Rangayyan (2024) Section 1.2.7 (latency, duration and",
      "amplitude of the response); averaging per",
      "Section 3.12"
    )
  )
}


# -- ErdErs: event-related desynchronisation / synchronisation for BCI.
#' ErdErs: event-related desynchronisation / synchronisation for BCI
#'
#' Part of the rangayyan_phys implementation; see the file header for
#' the source it follows.
#'
#' @param eeg See Usage.
#' @param fs See Usage.
#' @param ref_window See Usage.
#' @param active_window See Usage.
#' @param band Defaults to \code{c(8, 13)}.
#' @return A list with \code{erd_percent}, \code{ref_power}, \code{active_power}, \code{power_ratio}, \code{event}, \code{band_hz}, \code{ref_window_s}, \code{active_window_s}, \code{ref_samples}, \code{active_samples}, \code{fs_hz}, \code{sign_convention}, \code{units}, \code{method}.
#' @export
ErdErs <- function(eeg, fs, ref_window, active_window, band = c(8, 13)) {
  xs <- as.numeric(eeg)
  fs <- as.numeric(fs)
  if (fs <= 0) stop("fs must be positive (Hz)")
  lo <- as.numeric(band[1L])
  hi <- as.numeric(band[2L])
  if (!(lo >= 0 && lo < hi)) stop("band must satisfy 0 <= lo < hi (Hz)")
  if (hi > fs / 2) stop("band upper edge exceeds the Nyquist frequency")
  dur <- length(xs) / fs
  cut <- function(win, name) {
    a <- as.numeric(win[1L])
    b <- as.numeric(win[2L])
    if (b <= a) stop(sprintf("%s must have end > start (s)", name))
    if (a < 0 || b > dur) {
      stop(sprintf(
        "%s (%g, %g) s falls outside the %.3f s record",
        name, a, b, dur
      ))
    }
    seg <- xs[(as.integer(round(a * fs)) + 1L):as.integer(round(b * fs))]
    if (length(seg) < 4L) {
      stop(sprintf("%s is only %d samples; widen it", name, length(seg)))
    }
    seg
  }
  rseg <- cut(ref_window, "ref_window")
  aseg <- cut(active_window, "active_window")
  rsp <- .bsapsd(rseg, fs)
  asp <- .bsapsd(aseg, fs)
  # power per sample so that windows of different length compare
  R <- .bsabandpow(rsp$freqs, rsp$power, lo, hi) / length(rseg)
  A <- .bsabandpow(asp$freqs, asp$power, lo, hi) / length(aseg)
  if (R <= 0) {
    stop(sprintf("reference window has no power in %g-%g Hz", lo, hi))
  }
  pct <- 100 * (A - R) / R
  event <- if (abs(pct) < 1) "none" else if (pct > 0) "ERS" else "ERD"
  list(
    erd_percent = pct, ref_power = R, active_power = A,
    power_ratio = A / R, event = event,
    band_hz = c(lo, hi),
    ref_window_s = c(
      as.numeric(ref_window[1L]),
      as.numeric(ref_window[2L])
    ),
    active_window_s = c(
      as.numeric(active_window[1L]),
      as.numeric(active_window[2L])
    ),
    ref_samples = length(rseg), active_samples = length(aseg), fs_hz = fs,
    sign_convention =
      "negative erd_percent = desynchronisation (power drop)",
    units = list(
      power = "uV^2 per sample", erd_percent = "percent",
      frequency = "Hz"
    ),
    method = paste(
      "Pfurtscheller & Aranibar (1979) / Pfurtscheller &",
      "Lopes da Silva (1999) ERD-ERS; not defined in",
      "Rangayyan (2024), whose Section 9.12.2 covers NMF",
      "channel selection"
    )
  )
}


# -- CadSpec: frequency-domain feature extraction for CAD.
#' CadSpec: frequency-domain feature extraction for CAD
#'
#' Part of the rangayyan_phys implementation; see the file header for
#' the source it follows.
#'
#' @param x See Usage.
#' @param fs See Usage.
#' @param bands Defaults to \code{NULL}.
#' @return A vector, from \code{c}.
#' @export
CadSpec <- function(x, fs, bands = NULL) {
  fs <- as.numeric(fs)
  sp <- .bsapsd(x, fs)
  freqs <- sp$freqs
  power <- sp$power
  mom <- .bsapsdmom(freqs, power)
  if (is.null(bands)) {
    bands <- list(c(0, 100), c(100, 300), c(300, 600), c(600, fs / 2))
  }
  frac <- list()
  for (bd in bands) {
    lo <- as.numeric(bd[1L])
    hi <- as.numeric(bd[2L])
    if (hi <= lo) stop("each band must have hi > lo (Hz)")
    frac[[length(frac) + 1L]] <-
      c(lo, hi, .bsabandpow(freqs, power, lo, hi) / mom$total_power)
  }
  pk <- .bsapeaks(freqs, power, count = 1L)
  fdom <- if (length(pk$freqs)) pk$freqs[1L] else freqs[which.max(power)]
  qf <- .bsaqfactor(freqs, power, fdom)
  c(mom, list(
    band_power_fraction = frac,
    dominant_freq_hz = fdom,
    bandwidth_3db_hz = qf$bandwidth_hz, q_factor = qf$q,
    freq_hz = freqs, psd = power, fs_hz = fs,
    units = list(
      frequency = "Hz", power = "signal units^2",
      fractions = "dimensionless"
    ),
    method = paste(
      "Rangayyan (2024) eqs. (6.32), (6.34), (6.35), (6.37),",
      "(6.38), (6.41), (6.44); Sections 6.4.1, 6.4.2 and 7.10"
    )
  ))
}


# -- VagClean: muscle-contraction artifact removal from VAG signals (LMS).
#' VagClean: muscle-contraction artifact removal from VAG signals (LMS)
#'
#' Part of the rangayyan_phys implementation; see the file header for
#' the source it follows.
#'
#' @param vag See Usage.
#' @param emg_ref See Usage.
#' @param fs See Usage.
#' @param n_taps Defaults to \code{8}.
#' @param mu Defaults to \code{0.05}.
#' @param alpha Defaults to \code{0.02}.
#' @param adaptive_mu Defaults to \code{TRUE}.
#' @return A list with \code{cleaned}, \code{artifact_estimate}, \code{weights}, \code{rms_before}, \code{rms_after}, \code{artifact_reduction_db}, \code{mu_trace}, \code{n_taps}, \code{alpha}, \code{adaptive_mu}, \code{fs_hz}, \code{units}, \code{method}.
#' @export
VagClean <- function(vag, emg_ref, fs, n_taps = 8, mu = 0.05, alpha = 0.02,
                     adaptive_mu = TRUE) {
  xs <- as.numeric(vag)
  rs <- as.numeric(emg_ref)
  if (length(xs) != length(rs)) {
    stop("vag and emg_ref must have the same length")
  }
  if (length(xs) < 8L) stop("need at least 8 samples")
  fs <- as.numeric(fs)
  if (fs <= 0) stop("fs must be positive (Hz)")
  M1 <- as.integer(n_taps)
  if (M1 < 1L) stop("n_taps must be at least 1")
  mu <- as.numeric(mu)
  alpha <- as.numeric(alpha)
  if (!(alpha >= 0 && alpha < 1)) {
    stop("alpha must satisfy 0 <= alpha < 1 (forgetting factor)")
  }
  rpow <- .morie_fsum(rs * rs) / length(rs)
  if (rpow <= 0) {
    stop("emg_ref is identically zero; nothing to cancel with")
  }
  if (adaptive_mu) {
    if (!(mu > 0 && mu < 1)) {
      stop("with adaptive_mu, mu must lie in (0, 1) per eq. (3.205)")
    }
  } else {
    if (mu <= 0) stop("mu must be positive")
    lim <- 1 / (M1 * rpow)
    if (mu >= lim) {
      stop(sprintf(
        paste(
          "mu=%g exceeds the stability limit %g for a",
          "reference of power %g; the LMS filter would diverge"
        ),
        mu, lim, rpow
      ))
    }
  }
  N <- length(xs)
  w <- rep(0, M1)
  xbar2 <- rpow
  out <- numeric(N)
  art <- numeric(N)
  mus <- numeric(N)
  for (n in seq_len(N)) {
    ki <- n - (seq_len(M1) - 1L) # Python n - k, k = 0..M1-1
    r <- ifelse(ki >= 1L, rs[pmax(ki, 1L)], 0)
    y <- .morie_fsum(w * r)
    e <- xs[n] - y
    if (adaptive_mu) {
      xbar2 <- alpha * r[1L] * r[1L] + (1 - alpha) * xbar2
      step <- if (xbar2 > 0) mu / (M1 * xbar2) else 0
    } else {
      step <- mu
    }
    w <- w + 2 * step * e * r
    out[n] <- e
    art[n] <- y
    mus[n] <- step
  }
  rb <- .bsarms(xs)
  ra <- .bsarms(out)
  list(
    cleaned = out, artifact_estimate = art, weights = w,
    rms_before = rb, rms_after = ra,
    artifact_reduction_db = if (ra > 0 && rb > 0) {
      20 * log(rb / ra, 10)
    } else {
      NULL
    },
    mu_trace = mus, n_taps = M1, alpha = alpha,
    adaptive_mu = as.logical(adaptive_mu), fs_hz = fs,
    units = list(
      signals = "input amplitude units (mV)",
      reduction = "dB", mu = "dimensionless"
    ),
    method = paste(
      "Rangayyan (2024) eqs. (3.203), (3.204), (3.205),",
      "Sections 3.10.1, 3.10.2, 3.3.6 and 3.15,",
      "after Zhang et al."
    )
  )
}


# -- MuapModel: motor unit action potential as summed single-fibre potentials.
#' MuapModel: motor unit action potential as summed single-fibre
#' potentials
#'
#' Part of the rangayyan_phys implementation; see the file header for
#' the source it follows.
#'
#' @param t See Usage.
#' @param n_fibers Defaults to \code{25}.
#' @param conduction_vel Defaults to \code{4}.
#' @param spread_mm Defaults to \code{3}.
#' @param amp_uV Defaults to \code{8}.
#' @param width_ms Defaults to \code{1}.
#' @param phases Defaults to \code{3}.
#' @return A list with \code{t_ms}, \code{muap_uV}, \code{peak_to_peak_uV}, \code{peak_uV}, \code{duration_ms}, \code{n_phases_observed}, \code{delays_ms}, \code{n_fibers}, \code{conduction_vel_m_s}, \code{in_normal_duration_band}, \code{units}, \code{method}.
#' @export
MuapModel <- function(t, n_fibers = 25, conduction_vel = 4, spread_mm = 3,
                      amp_uV = 8, width_ms = 1, phases = 3) {
  ts <- as.numeric(t)
  if (length(ts) < 3L) {
    stop("t must contain at least 3 time points (ms)")
  }
  n_fibers <- as.integer(n_fibers)
  if (n_fibers < 1L) stop("n_fibers must be at least 1")
  conduction_vel <- as.numeric(conduction_vel)
  if (conduction_vel <= 0) stop("conduction_vel must be positive (m/s)")
  width_ms <- as.numeric(width_ms)
  if (width_ms <= 0) stop("width_ms must be positive (ms)")
  spread_mm <- as.numeric(spread_mm)
  if (spread_mm < 0) stop("spread_mm must be non-negative (mm)")
  phases <- as.integer(phases)
  if (!(phases %in% c(2L, 3L))) {
    stop("phases must be 2 (biphasic) or 3 (triphasic)")
  }
  amp_uV <- as.numeric(amp_uV)
  span_ms <- spread_mm / conduction_vel
  delays <- if (n_fibers == 1L) {
    0
  } else {
    -span_ms / 2 + span_ms * (seq_len(n_fibers) - 1L) / (n_fibers - 1L)
  }
  tmid <- 0.5 * (ts[1L] + ts[length(ts)])
  wave <- vapply(ts, function(ti) {
    u <- (ti - tmid - delays) / width_ms
    keep <- abs(u) <= 8
    if (!any(keep)) {
      return(0)
    }
    uu <- u[keep]
    ee <- exp(-0.5 * uu * uu)
    amp_uV * .morie_fsum(if (phases == 2L) -uu * ee else (uu * uu - 1) * ee)
  }, numeric(1))
  pk <- max(abs(wave))
  if (pk <= 0) {
    stop("MUAP is identically zero; widen t to cover the waveform")
  }
  thr <- 0.05 * pk
  on <- ts[abs(wave) >= thr]
  dur <- if (length(on) > 1L) on[length(on)] - on[1L] else 0
  cross <- sum((wave[-length(wave)] < 0) != (wave[-1L] < 0))
  list(
    t_ms = ts, muap_uV = wave,
    peak_to_peak_uV = max(wave) - min(wave),
    peak_uV = pk,
    duration_ms = dur,
    n_phases_observed = as.integer(cross) + 1L,
    delays_ms = delays,
    n_fibers = n_fibers,
    conduction_vel_m_s = conduction_vel,
    in_normal_duration_band = dur >= 3 && dur <= 15,
    units = list(t = "ms", muap = "uV", `conduction velocity` = "m/s"),
    method = paste(
      "MUAP as summed single-fibre potentials; morphology and",
      "normal ranges from Rangayyan (2024) Section 1.2.4",
      "(no waveform equation given in the book)"
    )
  )
}


# -- MurmSpec: heart murmur frequency analysis; Rangayyan (2024) eq. (6.45).
#' MurmSpec: heart murmur frequency analysis; Rangayyan (2024) eq.
#' (6.45)
#'
#' Part of the rangayyan_phys implementation; see the file header for
#' the source it follows.
#'
#' @param pcg See Usage.
#' @param fs See Usage.
#' @param f1 Defaults to \code{25}.
#' @param f2 Defaults to \code{75}.
#' @param f3 Defaults to \code{150}.
#' @return A vector, from \code{c}.
#' @export
MurmSpec <- function(pcg, fs, f1 = 25, f2 = 75, f3 = 150) {
  fs <- as.numeric(fs)
  f1 <- as.numeric(f1)
  f2 <- as.numeric(f2)
  f3 <- as.numeric(f3)
  if (!(f1 < f2 && f2 < f3)) {
    stop("band edges must satisfy f1 < f2 < f3 (Hz)")
  }
  if (f3 > fs / 2) stop("f3 exceeds the Nyquist frequency")
  sp <- .bsapsd(pcg, fs)
  freqs <- sp$freqs
  psd <- sp$power
  mag <- sqrt(psd)
  ca <- .morie_fsum(mag[freqs >= f1 & freqs < f2])
  pa <- .morie_fsum(mag[freqs >= f2 & freqs < f3])
  if (ca <= 0) {
    stop(sprintf("constant-area band %g-%g Hz carries no energy", f1, f2))
  }
  mom <- .bsapsdmom(freqs, psd)
  pk <- .bsapeaks(freqs, psd, count = 3L)
  c(mom, list(
    pa_over_ca = pa / ca,
    predictive_area = pa, constant_area = ca,
    bands_hz = list(CA = c(f1, f2), PA = c(f2, f3)),
    dominant_freq_hz = if (length(pk$freqs)) pk$freqs[1L] else NULL,
    peaks_hz = Map(c, pk$freqs, pk$powers),
    freq_hz = freqs, magnitude = mag, psd = psd, fs_hz = fs,
    units = list(frequency = "Hz", pa_over_ca = "dimensionless"),
    method = paste(
      "Rangayyan (2024) eq. (6.45), Sections 6.2.2 and 6.4.2,",
      "after Johnson et al."
    )
  ))
}


# -- OaeFeat: otoacoustic emission band analysis.
#' OaeFeat: otoacoustic emission band analysis
#'
#' Part of the rangayyan_phys implementation; see the file header for
#' the source it follows.
#'
#' @param oae See Usage.
#' @param fs See Usage.
#' @param noise_floor Defaults to \code{NULL}.
#' @param bands Defaults to \code{NULL}.
#' @return The value of \code{out}, as built in the body.
#' @export
OaeFeat <- function(oae, fs, noise_floor = NULL, bands = NULL) {
  fs <- as.numeric(fs)
  if (fs <= 0) stop("fs must be positive (Hz)")
  xs <- as.numeric(oae)
  sp <- .bsapsd(xs, fs)
  freqs <- sp$freqs
  psd <- sp$power
  mom <- .bsapsdmom(freqs, psd)
  npsd <- NULL
  if (!is.null(noise_floor)) {
    ns <- as.numeric(noise_floor)
    if (length(ns) != length(xs)) {
      stop("noise_floor must have the same length as oae")
    }
    npsd <- .bsapsd(ns, fs)$power
  }
  if (is.null(bands)) {
    centres <- c(1000, 1414, 2000, 2828, 4000)
    q4 <- 2^0.25
    centres <- centres[centres * q4 < fs / 2]
    bands <- lapply(centres, function(cc) c(cc / q4, cc * q4))
    if (!length(bands)) {
      stop(paste(
        "fs is too low for any default OAE band;",
        "supply bands explicitly"
      ))
    }
  }
  rows <- list()
  detected <- FALSE
  for (bd in bands) {
    lo <- as.numeric(bd[1L])
    hi <- as.numeric(bd[2L])
    if (hi <= lo) stop("each band must have hi > lo (Hz)")
    if (hi > fs / 2) stop("band upper edge exceeds the Nyquist frequency")
    p <- .bsabandpow(freqs, psd, lo, hi)
    row <- list(
      lo_hz = lo, hi_hz = hi, power = p,
      fraction = p / mom$total_power
    )
    if (!is.null(npsd)) {
      npow <- .bsabandpow(freqs, npsd, lo, hi)
      if (npow <= 0) {
        stop(sprintf("noise floor has no power in band %g-%g Hz", lo, hi))
      }
      row$snr_db <- 10 * log(p / npow, 10)
      detected <- detected || row$snr_db >= 6
    }
    rows[[length(rows) + 1L]] <- row
  }
  pk <- .bsapeaks(freqs, psd, count = 3L, minsep = fs / 200)
  out <- c(mom, list(
    band_analysis = rows,
    dominant_freq_hz = if (length(pk$freqs)) pk$freqs[1L] else NULL,
    peaks_hz = Map(c, pk$freqs, pk$powers), rms = .bsarms(xs),
    freq_hz = freqs, psd = psd, fs_hz = fs,
    units = list(frequency = "Hz", snr = "dB", fraction = "dimensionless"),
    method = paste(
      "Rangayyan (2024) Section 1.2.16 with the PSD measures of",
      "Section 6.4.1 and the band fraction of eq. (6.44)",
      "(no OAE equation given in the book)"
    )
  ))
  if (!is.null(npsd)) {
    out$emission_detected <- detected
    out$snr_criterion_db <- 6
  }
  out
}


# -- PdMonitor: Parkinson's disease monitoring via multimodal signals.
#' PdMonitor: Parkinson\'s disease monitoring via multimodal signals
#'
#' Part of the rangayyan_phys implementation; see the file header for
#' the source it follows.
#'
#' @param eeg See Usage.
#' @param emg See Usage.
#' @param gait See Usage.
#' @param fs See Usage.
#' @param tremor_band Defaults to \code{c(3, 7)}.
#' @return A list with \code{eeg_bands}, \code{eeg_beta_fraction}, \code{emg_tremor_fraction}, \code{emg_tremor_freq_hz}, \code{gait_tremor_fraction}, \code{gait_tremor_freq_hz}, \code{emg_form_factor}, \code{emg_turns_per_second}, \code{gait_rate_hz}, \code{gait_regularity}, \code{tremor_present}, \code{tremor_band_hz}, \code{fs_hz}, \code{units}, \code{method}.
#' @export
PdMonitor <- function(eeg, emg, gait, fs, tremor_band = c(3, 7)) {
  fs <- as.numeric(fs)
  if (fs < 60) stop("fs must be at least 60 Hz")
  tlo <- as.numeric(tremor_band[1L])
  thi <- as.numeric(tremor_band[2L])
  if (!(tlo > 0 && tlo < thi)) {
    stop("tremor_band must satisfy 0 < lo < hi (Hz)")
  }
  if (thi > fs / 2) {
    stop("tremor_band upper edge exceeds the Nyquist frequency")
  }
  tremor <- function(sig, name) {
    xs <- as.numeric(sig)
    if (length(xs) < 4L) stop(paste0(name, " needs at least 4 samples"))
    sp <- .bsapsd(xs, fs)
    tot <- .morie_fsum(sp$power)
    if (tot <= 0) {
      stop(paste0(name, " is constant; no spectrum to analyse"))
    }
    inb <- sp$freqs >= tlo & sp$freqs <= thi
    fpk <- if (any(inb)) sp$freqs[inb][which.max(sp$power[inb])] else NULL
    list(
      xs = xs, fr = sp$freqs, ps = sp$power, tot = tot,
      frac = .bsabandpow(sp$freqs, sp$power, tlo, thi) / tot, fpk = fpk
    )
  }
  et <- tremor(eeg, "eeg")
  bands <- list(
    delta = c(0.5, 4), theta = c(4, 8), alpha = c(8, 13.0001),
    beta = c(13.0001, min(30, fs / 2))
  )
  eeg_bands <- list()
  for (k in names(bands)) {
    bd <- bands[[k]]
    if (bd[2L] > bd[1L]) {
      eeg_bands[[k]] <- .bsabandpow(et$fr, et$ps, bd[1L], bd[2L]) / et$tot
    }
  }
  mt <- tremor(emg, "emg")
  gt <- tremor(gait, "gait")
  ma <- mt$xs
  ga <- gt$xs
  hj <- .bsahjorth(ma)
  nm <- length(ma)
  turns <- if (nm >= 3L) {
    sum((ma[2:(nm - 1L)] - ma[seq_len(nm - 2L)]) *
      (ma[3:nm] - ma[2:(nm - 1L)]) < 0)
  } else {
    0L
  }
  gpk <- .bsapeaks(gt$fr, gt$ps, count = 1L)
  grate <- if (length(gpk$freqs)) gpk$freqs[1L] else NULL
  greg <- NULL
  if (!is.null(grate) && grate > 0) {
    lag <- as.integer(round(fs / grate))
    if (lag >= 1L && lag < length(ga) - 1L) {
      acf <- .bsaacf(ga, lag)
      if (acf[1L] > 0) greg <- max(0, min(1, acf[lag + 1L] / acf[1L]))
    }
  }
  list(
    eeg_bands = eeg_bands,
    eeg_beta_fraction = eeg_bands$beta,
    emg_tremor_fraction = mt$frac, emg_tremor_freq_hz = mt$fpk,
    gait_tremor_fraction = gt$frac, gait_tremor_freq_hz = gt$fpk,
    emg_form_factor = hj$form_factor,
    emg_turns_per_second = turns * fs / nm,
    gait_rate_hz = grate, gait_regularity = greg,
    tremor_present = mt$frac > 0.2 && !is.null(mt$fpk),
    tremor_band_hz = c(tlo, thi), fs_hz = fs,
    units = list(
      eeg = "uV", emg = "uV", frequency = "Hz",
      fractions = "dimensionless", turns = "1/s"
    ),
    method = paste(
      "Rangayyan (2024) Section 10.14 with the EEG bands of",
      "Section 1.2.6, band fractions of eq. (6.44) and the",
      "form factor of eq. (5.26); the 3-7 Hz tremor band is",
      "the standard clinical range, not a value given in the",
      "book"
    )
  )
}


# -- PcgEeg: PCG-EEG magnitude coherence; Rangayyan (2024) eq. (4.32).
#' PcgEeg: PCG-EEG magnitude coherence; Rangayyan (2024) eq. (4.32)
#'
#' Part of the rangayyan_phys implementation; see the file header for
#' the source it follows.
#'
#' @param pcg See Usage.
#' @param eeg See Usage.
#' @param fs See Usage.
#' @param n_segments Defaults to \code{8}.
#' @param band Defaults to \code{c(1, 100)}.
#' @return A list with \code{freq_hz}, \code{coherence}, \code{coherence_sq}, \code{phase_rad}, \code{peak_coherence}, \code{peak_freq_hz}, \code{mean_coherence}, \code{delay_ms_at_peak}, \code{significance_level}, \code{n_segments}, \code{segment_samples}, \code{band_hz}, \code{fs_hz}, \code{units}, \code{method}.
#' @export
PcgEeg <- function(pcg, eeg, fs, n_segments = 8, band = c(1, 100)) {
  xs <- as.numeric(pcg)
  ys <- as.numeric(eeg)
  if (length(xs) != length(ys)) {
    stop("pcg and eeg must have the same length")
  }
  fs <- as.numeric(fs)
  if (fs <= 0) stop("fs must be positive (Hz)")
  L <- as.integer(n_segments)
  if (L < 2L) {
    stop(paste(
      "n_segments must be at least 2: with one segment the",
      "coherence is identically unity (Rangayyan eq. 4.32)"
    ))
  }
  if (length(xs) < 8L * L) stop("need at least 8*n_segments samples")
  lo <- as.numeric(band[1L])
  hi <- as.numeric(band[2L])
  if (!(lo >= 0 && lo < hi)) stop("band must satisfy 0 <= lo < hi (Hz)")
  w <- length(xs) %/% L
  nfft <- 1L
  while (nfft < w) nfft <- bitwShiftL(nfft, 1L)
  m <- nfft %/% 2L + 1L
  idx <- seq_len(m)
  Sxx <- rep(0, m)
  Syy <- rep(0, m)
  Sxyr <- rep(0, m)
  Sxyi <- rep(0, m)
  han <- 0.5 - 0.5 * cos(2 * pi * (seq_len(w) - 1L) / (w - 1L))
  pad <- rep(0, nfft - w)
  for (s in seq_len(L)) {
    sl <- ((s - 1L) * w + 1L):(s * w)
    a <- xs[sl]
    b <- ys[sl]
    ma <- .morie_fsum(a) / w
    mb <- .morie_fsum(b) / w
    fa <- .bsafft(c((a - ma) * han, pad), rep(0, nfft))
    fb <- .bsafft(c((b - mb) * han, pad), rep(0, nfft))
    ar <- fa$re[idx]
    ai <- fa$im[idx]
    br <- fb$re[idx]
    bi <- fb$im[idx]
    Sxx <- Sxx + ar * ar + ai * ai
    Syy <- Syy + br * br + bi * bi
    Sxyr <- Sxyr + ar * br + ai * bi # X conj(Y)
    Sxyi <- Sxyi + ai * br - ar * bi
  }
  freqs <- (idx - 1L) * fs / nfft
  den <- Sxx * Syy
  c2 <- ifelse(den > 0, (Sxyr * Sxyr + Sxyi * Sxyi) / den, 0)
  c2 <- pmin(1, pmax(0, c2))
  coh <- sqrt(c2)
  ph <- atan2(Sxyi, Sxyr)
  inb <- which(freqs >= lo & freqs <= hi)
  if (!length(inb)) stop(sprintf("no spectral bins in %g-%g Hz", lo, hi))
  kpk <- inb[which.max(coh[inb])]
  delay <- if (freqs[kpk] > 0) 1000 * ph[kpk] / (2 * pi * freqs[kpk]) else NULL
  list(
    freq_hz = freqs, coherence = coh, coherence_sq = c2, phase_rad = ph,
    peak_coherence = coh[kpk], peak_freq_hz = freqs[kpk],
    mean_coherence = .morie_fsum(coh[inb]) / length(inb),
    delay_ms_at_peak = delay,
    significance_level = 1 - 0.05^(1 / (L - 1)),
    n_segments = L, segment_samples = w, band_hz = c(lo, hi),
    fs_hz = fs,
    units = list(
      frequency = "Hz", coherence = "dimensionless [0,1]",
      phase = "radians", delay = "ms"
    ),
    method = paste(
      "Rangayyan (2024) eq. (4.32), Section 4.5, magnitude",
      "coherence with segment averaging as the book requires"
    )
  )
}


# -- MurmDet: murmur presence detection in PCG via spectral analysis.
#' MurmDet: murmur presence detection in PCG via spectral analysis
#'
#' Part of the rangayyan_phys implementation; see the file header for
#' the source it follows.
#'
#' @param pcg See Usage.
#' @param fs See Usage.
#' @param threshold Defaults to \code{0.15}.
#' @param hf_band Defaults to \code{c(150, 600)}.
#' @return A vector, from \code{c}.
#' @export
MurmDet <- function(pcg, fs, threshold = 0.15, hf_band = c(150, 600)) {
  fs <- as.numeric(fs)
  threshold <- as.numeric(threshold)
  if (!(threshold > 0 && threshold < 1)) {
    stop("threshold must be a power fraction in (0, 1)")
  }
  lo <- as.numeric(hf_band[1L])
  hi <- as.numeric(hf_band[2L])
  if (hi <= lo) stop("hf_band must have hi > lo (Hz)")
  if (hi > fs / 2) stop("hf_band upper edge exceeds the Nyquist frequency")
  sp <- .bsapsd(pcg, fs)
  freqs <- sp$freqs
  psd <- sp$power
  mom <- .bsapsdmom(freqs, psd)
  frac <- .bsabandpow(freqs, psd, lo, hi) / mom$total_power
  pk <- .bsapeaks(freqs, psd, count = 3L)
  c(mom, list(
    murmur_present = frac >= threshold,
    hf_power_fraction = frac,
    threshold = threshold, margin = frac - threshold,
    hf_band_hz = c(lo, hi),
    dominant_freq_hz = if (length(pk$freqs)) pk$freqs[1L] else NULL,
    freq_hz = freqs, psd = psd, fs_hz = fs,
    units = list(frequency = "Hz", fraction = "dimensionless"),
    method = paste(
      "Rangayyan (2024) Section 10.2.4 with the band power",
      "fraction of eq. (6.44), Section 6.4.2"
    )
  ))
}


# -- PsgStage: polysomnography signal fusion for sleep staging.
#' PsgStage: polysomnography signal fusion for sleep staging
#'
#' Part of the rangayyan_phys implementation; see the file header for
#' the source it follows.
#'
#' @param eeg See Usage.
#' @param eog See Usage.
#' @param emg See Usage.
#' @param fs See Usage.
#' @param epoch_len Defaults to \code{30}.
#' @return A list with \code{epochs}, \code{stage_sequence}, \code{stage_minutes}, \code{total_sleep_time_min}, \code{recording_time_min}, \code{sleep_efficiency}, \code{n_epochs}, \code{epoch_len_s}, \code{fs_hz}, \code{heuristic}, \code{units}, \code{method}.
#' @export
PsgStage <- function(eeg, eog, emg, fs, epoch_len = 30) {
  a <- as.numeric(eeg)
  b <- as.numeric(eog)
  cx <- as.numeric(emg)
  if (!(length(a) == length(b) && length(b) == length(cx))) {
    stop("eeg, eog and emg must have the same length")
  }
  fs <- as.numeric(fs)
  if (fs < 60) stop("fs must be at least 60 Hz for polysomnographic bands")
  epoch_len <- as.numeric(epoch_len)
  if (epoch_len <= 0) stop("epoch_len must be positive (s)")
  w <- as.integer(round(epoch_len * fs))
  n_ep <- length(a) %/% w
  if (n_ep < 1L) stop("recording is shorter than one epoch")
  rows <- vector("list", n_ep)
  for (e in seq_len(n_ep)) {
    sl <- ((e - 1L) * w + 1L):(e * w)
    sp <- .bsapsd(a[sl], fs)
    tot <- .morie_fsum(sp$power)
    if (tot <= 0) stop(sprintf("EEG epoch %d is constant", e - 1L))
    d <- .bsabandpow(sp$freqs, sp$power, 0.5, 4) / tot
    th <- .bsabandpow(sp$freqs, sp$power, 4, 8) / tot
    al <- .bsabandpow(sp$freqs, sp$power, 8, 13.0001) / tot
    be <- .bsabandpow(sp$freqs, sp$power, 13.0001, fs / 2) / tot
    spo <- .bsapsd(b[sl], fs)
    eog_act <- sqrt(.bsabandpow(spo$freqs, spo$power, 0.3, 8) / length(sl))
    spm <- .bsapsd(cx[sl], fs)
    emg_tone <- sqrt(.bsabandpow(spm$freqs, spm$power, 10, fs / 2) /
      length(sl))
    rows[[e]] <- list(
      t_start_s = (e - 1L) * epoch_len, delta_fraction = d,
      theta_fraction = th, alpha_fraction = al,
      beta_fraction = be, eeg_slow_fraction = d + th,
      eog_activity = eog_act, emg_tone = emg_tone
    )
  }
  # Self-calibrating heuristic: reference levels are this recording's medians.
  med <- function(key) {
    v <- sort(vapply(rows, function(r) r[[key]], numeric(1)))
    mm <- length(v)
    if (mm %% 2L) v[mm %/% 2L + 1L] else 0.5 * (v[mm %/% 2L] + v[mm %/% 2L + 1L])
  }
  emg_ref <- med("emg_tone")
  eog_ref <- med("eog_activity")
  for (e in seq_len(n_ep)) {
    r <- rows[[e]]
    rows[[e]]$stage <-
      if (r$delta_fraction > 0.5) {
        "N3"
      } else if (r$emg_tone < 0.5 * emg_ref && r$eog_activity > eog_ref &&
        r$beta_fraction + r$theta_fraction > 0.4) {
        "REM"
      } else if (r$alpha_fraction > 0.3 && r$emg_tone >= emg_ref) {
        "Wake"
      } else if (r$theta_fraction > r$alpha_fraction) {
        "N2"
      } else {
        "N1"
      }
  }
  seqv <- vapply(rows, function(r) r$stage, character(1))
  mins <- list()
  for (s in seqv) {
    mins[[s]] <- (if (is.null(mins[[s]])) 0 else mins[[s]]) + epoch_len / 60
  }
  tst <- sum(unlist(mins[names(mins) != "Wake"]))
  if (!length(tst)) tst <- 0
  total <- n_ep * epoch_len / 60
  list(
    epochs = rows, stage_sequence = seqv, stage_minutes = mins,
    total_sleep_time_min = tst, recording_time_min = total,
    sleep_efficiency = if (total > 0) tst / total else 0,
    n_epochs = n_ep, epoch_len_s = epoch_len, fs_hz = fs,
    heuristic = TRUE,
    units = list(
      signals = "uV", time = "s", stage_minutes = "minutes",
      fractions = "dimensionless"
    ),
    method = paste(
      "Rangayyan (2024) Section 2.4.1 (polysomnography) with",
      "the EEG bands of Section 1.2.6 and band fractions of",
      "eq. (6.44); staging rule is a self-calibrating",
      "heuristic, not a clinical scoring algorithm and not",
      "given in the book"
    )
  )
}


# -- IeiStats: point-process inter-event interval statistics.
#' IeiStats: point-process inter-event interval statistics
#'
#' Part of the rangayyan_phys implementation; see the file header for
#' the source it follows.
#'
#' @param event_times See Usage.
#' @param T Defaults to \code{NULL}.
#' @param n_bins Defaults to \code{20}.
#' @return A list with \code{mean_ipi_s}, \code{sd_ipi_s}, \code{cv_ipi}, \code{mean_rate_pps}, \code{sd_rate_pps}, \code{cv_rate}, \code{event_rate_pps}, \code{min_ipi_s}, \code{max_ipi_s}, \code{median_ipi_s}, \code{fano_factor}, \code{ipi_histogram}, \code{n_events}, \code{n_intervals}, \code{duration_s}, \code{regularity}, \code{units}, \code{method}.
#' @export
IeiStats <- function(event_times, T = NULL, n_bins = 20) {
  ts <- as.numeric(event_times)
  if (length(ts) < 3L) {
    stop("need at least 3 event times to form 2 intervals")
  }
  if (any(diff(ts) <= 0)) {
    stop("event_times must be strictly increasing (s)")
  }
  n_bins <- as.integer(n_bins)
  if (n_bins < 2L) stop("n_bins must be at least 2")
  ipi <- diff(ts)
  n <- length(ipi)
  mi <- .morie_fsum(ipi) / n
  si <- if (n > 1L) sqrt(.morie_fsum((ipi - mi) * (ipi - mi)) / (n - 1L)) else 0
  rates <- 1 / ipi
  mr <- .morie_fsum(rates) / n
  sr <- if (n > 1L) {
    sqrt(.morie_fsum((rates - mr) * (rates - mr)) / (n - 1L))
  } else {
    0
  }
  span <- if (is.null(T)) ts[length(ts)] - ts[1L] else as.numeric(T)
  if (span <= 0) stop("observation duration must be positive (s)")
  srt <- sort(ipi)
  med <- if (n %% 2L) {
    srt[n %/% 2L + 1L]
  } else {
    0.5 * (srt[n %/% 2L] + srt[n %/% 2L + 1L])
  }
  lo <- min(ipi)
  hi <- max(ipi)
  if (hi > lo) {
    width <- (hi - lo) / n_bins
    b <- pmin(as.integer((ipi - lo) / width), n_bins - 1L)
    counts <- tabulate(b + 1L, nbins = n_bins)
    hist <- Map(
      function(k, ct) c(lo + (k + 0.5) * width, ct),
      0:(n_bins - 1L), counts
    )
  } else {
    hist <- list(c(lo, n))
  }
  # Fano factor over windows of ten mean intervals
  win <- 10 * mi
  nw <- max(2L, as.integer(span / win))
  bb <- as.integer((ts - ts[1L]) / span * nw)
  bb <- pmin(pmax(bb, 0L), nw - 1L)
  counts <- tabulate(bb + 1L, nbins = nw)
  mc <- .morie_fsum(counts) / nw
  fano <- if (mc > 0) {
    .morie_fsum((counts - mc) * (counts - mc)) / nw / mc
  } else {
    NULL
  }
  cvr <- if (mr > 0) sr / mr else 0
  reg <- if (cvr < 0.1) {
    "near-periodic"
  } else if (cvr < 0.25) "regular" else "irregular"
  list(
    mean_ipi_s = mi, sd_ipi_s = si, cv_ipi = if (mi > 0) si / mi else 0,
    mean_rate_pps = mr, sd_rate_pps = sr, cv_rate = cvr,
    event_rate_pps = length(ts) / span,
    min_ipi_s = lo, max_ipi_s = hi, median_ipi_s = med,
    fano_factor = fano,
    ipi_histogram = hist, n_events = length(ts), n_intervals = n,
    duration_s = span, regularity = reg,
    units = list(
      interval = "s", rate = "pps (pulses per second)",
      cv = "dimensionless"
    ),
    method = paste(
      "Rangayyan (2024) Section 7.3, IPI statistics mu_r and",
      "CV_r = sigma_r / mu_r, after Zhang et al."
    )
  )
}


# -- ValvePcg: prosthetic heart valve evaluation via PCG AR spectrum.
#' ValvePcg: prosthetic heart valve evaluation via PCG AR spectrum
#'
#' Part of the rangayyan_phys implementation; see the file header for
#' the source it follows.
#'
#' @param pcg See Usage.
#' @param fs See Usage.
#' @param n_peaks Defaults to \code{3}.
#' @param order Defaults to \code{NULL}.
#' @return A vector, from \code{c}.
#' @export
ValvePcg <- function(pcg, fs, n_peaks = 3, order = NULL) {
  xs <- as.numeric(pcg)
  fs <- as.numeric(fs)
  if (fs <= 0) stop("fs must be positive (Hz)")
  if (length(xs) < 32L) stop("need at least 32 PCG samples")
  n_peaks <- as.integer(n_peaks)
  if (n_peaks < 1L) stop("n_peaks must be at least 1")
  p <- if (!is.null(order)) as.integer(order) else 2L * n_peaks + 4L
  if (p < 2L) stop("order must be at least 2")
  if (length(xs) < 4L * p) {
    stop("need at least 4*order samples to fit the AR model")
  }
  lp <- .bsalpc(xs, p)
  sp <- .bsalpcspec(lp$a, fs, npts = 2048L)
  freqs <- sp$freqs
  psd <- lp$e * sp$power
  found <- .bsapeaks(freqs, psd, count = n_peaks, minsep = fs / 200)
  peaks <- list()
  for (i in seq_along(found$freqs)) {
    qf <- .bsaqfactor(freqs, psd, found$freqs[i])
    peaks[[i]] <- list(
      freq_hz = found$freqs[i], power = found$powers[i],
      bandwidth_3db_hz = qf$bandwidth_hz, q_factor = qf$q
    )
  }
  mom <- .bsapsdmom(freqs, psd)
  c(mom, list(
    peaks = peaks,
    dominant_freq_hz = if (length(peaks)) peaks[[1L]]$freq_hz else NULL,
    dominant_q = if (length(peaks)) peaks[[1L]]$q_factor else NULL,
    order = p, prediction_error = lp$e, ar_coeffs = lp$a,
    freq_hz = freqs, ar_psd = psd, fs_hz = fs,
    units = list(
      frequency = "Hz", bandwidth = "Hz",
      q_factor = "dimensionless"
    ),
    method = paste(
      "Rangayyan (2024) Section 6.5 after Durand et al., with",
      "the -3 dB bandwidth and quality-factor measures of",
      "Section 6.4.2"
    )
  ))
}


# -- RespFeat: respiratory rate, depth and I:E ratio.
#' RespFeat: respiratory rate, depth and I:E ratio
#'
#' Part of the rangayyan_phys implementation; see the file header for
#' the source it follows.
#'
#' @param resp See Usage.
#' @param fs See Usage.
#' @param signal_type Defaults to \code{"flow"}.
#' @param min_breath_s Defaults to \code{1}.
#' @return A list with \code{rate_breaths_per_min}, \code{mean_period_s}, \code{sd_period_s}, \code{regularity_cv}, \code{depth}, \code{mean_ti_s}, \code{mean_te_s}, \code{ie_ratio}, \code{breaths}, \code{n_breaths}, \code{signal_type}, \code{fs_hz}, \code{units}, \code{method}.
#' @export
RespFeat <- function(resp, fs, signal_type = "flow", min_breath_s = 1) {
  xs <- as.numeric(resp)
  fs <- as.numeric(fs)
  if (fs <= 0) stop("fs must be positive (Hz)")
  if (length(xs) < 8L) stop("need at least 8 samples")
  signal_type <- tolower(as.character(signal_type))
  if (!(signal_type %in% c("flow", "volume"))) {
    stop("signal_type must be 'flow' or 'volume'")
  }
  min_breath_s <- as.numeric(min_breath_s)
  if (min_breath_s <= 0) stop("min_breath_s must be positive (s)")
  mu <- .morie_fsum(xs) / length(xs)
  ys <- xs - mu
  drive <- if (signal_type == "volume") c(diff(ys), 0) else ys
  nd <- length(drive)
  # inspiration onsets = upward zero crossings of the drive signal
  ons <- which(drive[seq_len(nd - 1L)] <= 0 & drive[-1L] > 0) + 1L
  kept <- integer(0)
  for (i in ons) {
    if (!length(kept) || (i - kept[length(kept)]) / fs >= min_breath_s) {
      kept <- c(kept, i)
    }
  }
  if (length(kept) < 2L) {
    stop(paste(
      "fewer than 2 breaths detected; check signal_type,",
      "fs and min_breath_s"
    ))
  }
  breaths <- vector("list", length(kept) - 1L)
  for (b in seq_len(length(kept) - 1L)) {
    i0 <- kept[b]
    i1 <- kept[b + 1L]
    # end of inspiration = downward zero crossing of the drive signal
    iend <- i1
    if (i1 > i0 + 1L) {
      cand <- (i0 + 1L):(i1 - 1L)
      dn <- cand[drive[cand - 1L] > 0 & drive[cand] <= 0]
      if (length(dn)) iend <- dn[1L]
    }
    ti <- (iend - i0) / fs
    te <- (i1 - iend) / fs
    if (signal_type == "flow") {
      depth <- .morie_fsum(ys[i0:(iend - 1L)]) / fs
    } else {
      seg <- ys[i0:(i1 - 1L)]
      depth <- if (length(seg)) max(seg) - min(seg) else 0
    }
    breaths[[b]] <- list(
      t_start_s = (i0 - 1L) / fs,
      period_s = (i1 - i0) / fs,
      ti_s = ti, te_s = te, depth = depth
    )
  }
  per <- vapply(breaths, function(b) b$period_s, numeric(1))
  n <- length(per)
  mp <- .morie_fsum(per) / n
  sp <- if (n > 1L) sqrt(.morie_fsum((per - mp) * (per - mp)) / (n - 1L)) else 0
  mti <- .morie_fsum(vapply(breaths, function(b) b$ti_s, numeric(1))) / n
  mte <- .morie_fsum(vapply(breaths, function(b) b$te_s, numeric(1))) / n
  list(
    rate_breaths_per_min = 60 / mp,
    mean_period_s = mp, sd_period_s = sp,
    regularity_cv = if (mp > 0) sp / mp else 0,
    depth = .morie_fsum(vapply(
      breaths, function(b) b$depth,
      numeric(1)
    )) / n,
    mean_ti_s = mti, mean_te_s = mte,
    ie_ratio = if (mte > 0) mti / mte else NULL,
    breaths = breaths, n_breaths = n,
    signal_type = signal_type, fs_hz = fs,
    units = list(
      rate = "breaths/min", time = "s",
      depth = "litres if flow is L/s or volume is L",
      ie_ratio = "dimensionless"
    ),
    method = paste(
      "Standard per-breath respiratory measures;",
      "Rangayyan (2024) Sections 2.4.1 and 5.10 give the",
      "context but no equations for rate, depth or I:E"
    )
  )
}


# -- RespSound: respiratory sound generation model (bronchial turbulence);
#    Rangayyan (2024) eqs. (7.122), (7.127), (7.128), (7.129), Section 7.7.1.
#' RespSound: respiratory sound generation model (bronchial turbulence);
#'
#' Rangayyan (2024) eqs. (7.122), (7.127), (7.128), (7.129), Section
#' 7.7.1.
#'
#' @param length_m Defaults to \code{0.1}.
#' @param radius_m Defaults to \code{0.009}.
#' @param freqs Defaults to \code{NULL}.
#' @param rho Defaults to \code{1.2}.
#' @param c Defaults to \code{343}.
#' @param mu Defaults to \code{1.8e-05}.
#' @param P0 Defaults to \code{101325}.
#' @param eta Defaults to \code{1.4}.
#' @param lam Defaults to \code{0.026}.
#' @param cp Defaults to \code{1005}.
#' @return A list with \code{freq_hz}, \code{transfer_mag}, \code{transfer_db}, \code{La_kg_per_m4}, \code{Ca_m3_per_Pa}, \code{Ra_Pa_s_per_m3}, \code{Ga_m3_per_Pa_s}, \code{resonance_hz}, \code{area_m2}, \code{circumference_m}, \code{volume_m3}, \code{units}, \code{method}.
#' @export
RespSound <- function(length_m = 0.1, radius_m = 0.009, freqs = NULL,
                      rho = 1.2, c = 343, mu = 1.8e-5, P0 = 101325,
                      eta = 1.4, lam = 0.026, cp = 1005) {
  l <- as.numeric(length_m)
  r <- as.numeric(radius_m)
  if (l <= 0) stop("length_m must be positive (m)")
  if (r <= 0) stop("radius_m must be positive (m)")
  rho <- as.numeric(rho)
  c <- as.numeric(c)
  mu <- as.numeric(mu)
  P0 <- as.numeric(P0)
  eta <- as.numeric(eta)
  lam <- as.numeric(lam)
  cp <- as.numeric(cp)
  if (min(rho, c, mu, P0, lam, cp) <= 0) {
    stop("rho, c, mu, P0, lam and cp must all be positive")
  }
  if (eta <= 1) stop("eta (adiabatic constant) must exceed 1")
  A <- pi * r * r
  S <- 2 * pi * r
  Va <- A * l
  La <- rho * l / A # eq. (7.122), kg/m^4
  Ca <- Va / (P0 * eta) # eq. (7.127), m^3/Pa
  if (is.null(freqs)) {
    fs_hz <- 10 * (1:200)
  } else {
    fs_hz <- as.numeric(freqs)
    if (any(fs_hz <= 0)) stop("frequencies must be positive (Hz)")
  }
  w <- 2 * pi * fs_hz
  Ra <- (l * S / (A * A)) * sqrt(w * rho * mu / 2) # eq. (7.128)
  Ga <- (S * l / (rho * c * c)) * (eta - 1) *
    sqrt(lam * w / (2 * cp * rho)) # eq. (7.129)
  # series impedance Ra + j w La, shunt admittance Ga + j w Ca; H = 1/(1 + ZY)
  zr <- Ra
  zi <- w * La
  yr <- Ga
  yi <- w * Ca
  dr <- 1 + (zr * yr - zi * yi)
  di <- zr * yi + zi * yr
  mag <- 1 / sqrt(dr * dr + di * di)
  f0 <- 1 / (2 * pi * sqrt(La * Ca))
  list(
    freq_hz = fs_hz, transfer_mag = mag,
    transfer_db = ifelse(mag > 0, 20 * log(mag, 10), -300),
    La_kg_per_m4 = La, Ca_m3_per_Pa = Ca,
    Ra_Pa_s_per_m3 = Ra, Ga_m3_per_Pa_s = Ga,
    resonance_hz = f0,
    area_m2 = A, circumference_m = S, volume_m3 = Va,
    units = list(
      freq = "Hz", La = "kg/m^4", Ca = "m^3/Pa",
      Ra = "Pa s/m^3", Ga = "m^3/(Pa s)", length = "m"
    ),
    method = paste(
      "Rangayyan (2024) eqs. (7.122), (7.127), (7.128),",
      "(7.129), Section 7.7.1, after Flanagan and Moussavi"
    )
  )
}


# -- ApneaDet: sleep apnea detection from ECG, SpO2 and snore.
#' ApneaDet: sleep apnea detection from ECG, SpO2 and snore
#'
#' Part of the rangayyan_phys implementation; see the file header for
#' the source it follows.
#'
#' @param ecg See Usage.
#' @param spo2 See Usage.
#' @param snore See Usage.
#' @param fs See Usage.
#' @param epoch_s Defaults to \code{60}.
#' @param desat_pct Defaults to \code{4}.
#' @return A list with \code{epochs}, \code{n_flagged}, \code{apnea_suspected}, \code{events_per_hour}, \code{severity}, \code{n_epochs}, \code{epoch_s}, \code{fs_hz}, \code{desat_criterion_pct}, \code{heuristic}, \code{units}, \code{method}.
#' @export
ApneaDet <- function(ecg, spo2, snore, fs, epoch_s = 60, desat_pct = 4) {
  e <- as.numeric(ecg)
  s <- as.numeric(spo2)
  q <- as.numeric(snore)
  if (!(length(e) == length(s) && length(s) == length(q))) {
    stop("ecg, spo2 and snore must have the same length")
  }
  fs <- as.numeric(fs)
  if (fs < 100) stop("fs must be at least 100 Hz for R-wave timing")
  if (any(s < 0 | s > 100)) stop("spo2 values must lie in 0-100 percent")
  epoch_s <- as.numeric(epoch_s)
  if (epoch_s <= 0) stop("epoch_s must be positive (s)")
  desat_pct <- as.numeric(desat_pct)
  if (desat_pct <= 0) {
    stop("desat_pct must be positive (percentage points)")
  }
  w <- as.integer(round(epoch_s * fs))
  n_ep <- length(e) %/% w
  if (n_ep < 1L) stop("recording is shorter than one epoch")
  # R peaks: squared first difference thresholded at 60 percent of the
  # epoch maximum, with a 200 ms refractory period
  d <- c(diff(e), 0)
  sq <- d * d
  refr <- as.integer(0.2 * fs)
  rows <- vector("list", n_ep)
  snores <- numeric(n_ep)
  for (k in seq_len(n_ep)) {
    sl <- ((k - 1L) * w + 1L):(k * w)
    seg <- sq[sl]
    thr <- if (max(seg) > 0) 0.6 * max(seg) else 1
    peaks <- integer(0)
    last <- -refr # 0-based, as in Python
    for (i in seq_along(seg)) {
      i0 <- i - 1L
      if (seg[i] >= thr && i0 - last >= refr) {
        peaks <- c(peaks, i0)
        last <- i0
      }
    }
    rr <- if (length(peaks) > 1L) diff(peaks) / fs else numeric(0)
    hr <- if (length(rr)) 60 / (.morie_fsum(rr) / length(rr)) else NULL
    lf <- NULL
    if (length(rr) >= 8L) {
      hrs <- 60 / rr
      frr <- length(rr) / epoch_s # mean sampling rate of the HR series
      sp <- .bsapsd(hrs, frr)
      tot <- .morie_fsum(sp$power)
      if (tot > 0) lf <- .bsabandpow(sp$freqs, sp$power, 0.01, 0.04) / tot
    }
    base <- max(s[sl])
    lo <- min(s[sl])
    srms <- .bsarms(q[sl])
    snores[k] <- srms
    rows[[k]] <- list(
      t_start_s = (k - 1L) * epoch_s, min_spo2 = lo,
      baseline_spo2 = base, desat_depth_pct = base - lo,
      snore_rms = srms, mean_hr_bpm = hr,
      hr_lf_fraction = lf
    )
  }
  smed <- sort(snores)[length(snores) %/% 2L + 1L]
  for (k in seq_len(n_ep)) {
    row <- rows[[k]]
    score <- 0L
    if (row$desat_depth_pct >= desat_pct) score <- score + 1L
    if (smed > 0 && row$snore_rms >= 1.5 * smed) score <- score + 1L
    if (!is.null(row$hr_lf_fraction) && row$hr_lf_fraction >= 0.3) {
      score <- score + 1L
    }
    rows[[k]]$score <- score
    rows[[k]]$epoch_flagged <- score >= 2L
  }
  nflag <- sum(vapply(rows, function(r) r$epoch_flagged, logical(1)))
  hours <- n_ep * epoch_s / 3600
  idx <- if (hours > 0) nflag / hours else 0
  sev <- if (idx < 5) {
    "none"
  } else if (idx < 15) {
    "mild"
  } else if (idx < 30) "moderate" else "severe"
  list(
    epochs = rows, n_flagged = as.integer(nflag),
    apnea_suspected = idx >= 5,
    events_per_hour = idx, severity = sev,
    n_epochs = n_ep, epoch_s = epoch_s, fs_hz = fs,
    desat_criterion_pct = desat_pct, heuristic = TRUE,
    units = list(
      spo2 = "percent", ecg = "mV", hr = "bpm",
      events_per_hour = "1/h", time = "s"
    ),
    method = paste(
      "Rangayyan (2024) Sections 10.2.5, 10.13 and 2.4",
      "(multimodal apnea detection); the combination rule is",
      "a documented heuristic, not a trained classifier, and",
      "is not given in the book"
    )
  )
}


# -- SpeechFeat: speech formants and autocorrelation pitch.
#' SpeechFeat: speech formants and autocorrelation pitch
#'
#' Part of the rangayyan_phys implementation; see the file header for
#' the source it follows.
#'
#' @param speech See Usage.
#' @param fs See Usage.
#' @param order Defaults to \code{NULL}.
#' @param n_formants Defaults to \code{4}.
#' @param f0_range Defaults to \code{c(60, 400)}.
#' @return A list with \code{formants_hz}, \code{formant_bandwidths_hz}, \code{formant_powers}, \code{f0_hz}, \code{pitch_period_ms}, \code{voiced}, \code{voicing_strength}, \code{voicing_threshold}, \code{zero_crossing_rate}, \code{order}, \code{ar_coeffs}, \code{prediction_error}, \code{freq_hz}, \code{lpc_psd}, \code{fs_hz}, \code{units}, \code{method}.
#' @export
SpeechFeat <- function(speech, fs, order = NULL, n_formants = 4,
                       f0_range = c(60, 400)) {
  xs <- as.numeric(speech)
  fs <- as.numeric(fs)
  if (fs <= 0) stop("fs must be positive (Hz)")
  p <- if (!is.null(order)) as.integer(order) else as.integer(2 + fs / 1000)
  if (p < 4L) stop("order must be at least 4 to resolve a formant")
  if (length(xs) < 4L * p) {
    stop("need at least 4*order samples for the all-pole fit")
  }
  n_formants <- as.integer(n_formants)
  if (n_formants < 1L) stop("n_formants must be at least 1")
  flo <- as.numeric(f0_range[1L])
  fhi <- as.numeric(f0_range[2L])
  if (!(flo > 0 && flo < fhi)) stop("f0_range must satisfy 0 < lo < hi (Hz)")
  if (fhi > fs / 2) {
    stop("f0_range upper edge exceeds the Nyquist frequency")
  }
  lp <- .bsalpc(xs, p)
  sp <- .bsalpcspec(lp$a, fs, npts = 2048L)
  freqs <- sp$freqs
  psd <- lp$e * sp$power
  # formants: the strongest peaks, then re-sorted by frequency
  pk <- .bsapeaks(freqs, psd, count = n_formants, minsep = 90)
  o <- order(pk$freqs, pk$powers)
  fmt <- pk$freqs[o]
  pws <- pk$powers[o]
  bws <- lapply(fmt, function(f) .bsaqfactor(freqs, psd, f)$bandwidth_hz)
  lag_lo <- max(1L, as.integer(fs / fhi))
  lag_hi <- min(length(xs) - 1L, as.integer(fs / flo))
  if (lag_hi <= lag_lo) {
    stop("segment is too short for the requested f0_range")
  }
  acf <- .bsaacf(xs, lag_hi)
  if (acf[1L] <= 0) stop("segment is constant; no pitch to estimate")
  best <- lag_lo + which.max(acf[(lag_lo + 1L):(lag_hi + 1L)]) - 1L
  strength <- max(0, acf[best + 1L] / acf[1L])
  voiced <- strength >= 0.3
  zc <- sum((xs[-length(xs)] < 0) != (xs[-1L] < 0))
  list(
    formants_hz = fmt, formant_bandwidths_hz = bws,
    formant_powers = pws,
    f0_hz = if (voiced) fs / best else NULL,
    pitch_period_ms = if (voiced) 1000 * best / fs else NULL,
    voiced = voiced, voicing_strength = strength,
    voicing_threshold = 0.3,
    zero_crossing_rate = zc * fs / length(xs),
    order = p, ar_coeffs = lp$a, prediction_error = lp$e,
    freq_hz = freqs, lpc_psd = psd, fs_hz = fs,
    units = list(
      frequency = "Hz", period = "ms",
      zero_crossing_rate = "1/s"
    ),
    method = paste(
      "Rangayyan (2024) Section 7.2.3 with all-pole",
      "vocal-tract modelling of Section 7.5; pitch from the",
      "autocorrelation peak (mean inter-pulse interval of the",
      "point-process excitation)"
    )
  )
}


# -- VagFeat: statistical vibroarthrogram characterisation.
#' VagFeat: statistical vibroarthrogram characterisation
#'
#' Part of the rangayyan_phys implementation; see the file header for
#' the source it follows.
#'
#' @param vag See Usage.
#' @param fs See Usage.
#' @param n_segments Defaults to \code{8}.
#' @return A list with \code{mean}, \code{variance}, \code{skewness}, \code{kurtosis}, \code{kurtosis_excess}, \code{form_factor}, \code{mobility}, \code{activity}, \code{turns_count}, \code{turns_per_second}, \code{var_of_segment_means}, \code{var_of_segment_ms}, \code{segment_means}, \code{segment_ms}, \code{entropy_bits}, \code{rms}, \code{duration_s}, \code{fs_hz}, \code{n_segments}, \code{units}, \code{method}.
#' @export
VagFeat <- function(vag, fs, n_segments = 8) {
  xs <- as.numeric(vag)
  fs <- as.numeric(fs)
  if (fs <= 0) stop("fs must be positive (Hz)")
  ns <- as.integer(n_segments)
  if (ns < 2L) stop("n_segments must be at least 2")
  if (length(xs) < 4L * ns) stop("need at least 4*n_segments samples")
  mm <- .bsamoments(xs)
  hj <- .bsahjorth(xs)
  seg <- length(xs) %/% ns
  means <- vapply(seq_len(ns), function(i) {
    .morie_fsum(xs[((i - 1L) * seg + 1L):(i * seg)]) / seg
  }, numeric(1))
  mss <- vapply(seq_len(ns), function(i) {
    v <- xs[((i - 1L) * seg + 1L):(i * seg)]
    .morie_fsum(v * v) / seg
  }, numeric(1))
  varof <- function(v) {
    m <- .morie_fsum(v) / length(v)
    .morie_fsum((v - m) * (v - m)) / length(v)
  }
  nx <- length(xs)
  turns <- if (nx >= 3L) {
    sum((xs[2:(nx - 1L)] - xs[seq_len(nx - 2L)]) *
      (xs[3:nx] - xs[2:(nx - 1L)]) < 0)
  } else {
    0L
  }
  # Shannon entropy of a 64-bin amplitude histogram, eq. (5.31) style
  lo <- min(xs)
  hi <- max(xs)
  if (hi > lo) {
    nb <- 64L
    b <- pmin(as.integer((xs - lo) / (hi - lo) * nb), nb - 1L)
    counts <- tabulate(b + 1L, nbins = nb)
    cnz <- counts[counts > 0]
    ent <- -.morie_fsum((cnz / nx) * log(cnz / nx, 2))
  } else {
    ent <- 0
  }
  list(
    mean = mm[1L], variance = mm[2L], skewness = mm[3L],
    kurtosis = mm[4L], kurtosis_excess = mm[4L] - 3,
    form_factor = hj$form_factor, mobility = hj$mobility,
    activity = hj$activity,
    turns_count = as.integer(turns),
    turns_per_second = turns * fs / nx,
    var_of_segment_means = varof(means),
    var_of_segment_ms = varof(mss),
    segment_means = means, segment_ms = mss,
    entropy_bits = ent, rms = .bsarms(xs),
    duration_s = nx / fs, fs_hz = fs, n_segments = ns,
    units = list(
      amplitude = "signal units (mV at the accelerometer)",
      entropy = "bits", turns_per_second = "1/s",
      form_factor = "dimensionless"
    ),
    method = paste(
      "Rangayyan (2024) Section 5.12.3 with eqs. (5.25),",
      "(5.26) and (5.31), Sections 5.6.4 and 3.2.1"
    )
  )
}


# -- VagKnee: linear-discriminant screening of a VAG signal for knee
#    cartilage pathology.
#' VagKnee: linear-discriminant screening of a VAG signal for knee
#'
#' cartilage pathology.
#'
#' @param vag See Usage.
#' @param fs See Usage.
#' @param weights Defaults to \code{NULL}.
#' @param bias Defaults to \code{NULL}.
#' @param n_segments Defaults to \code{8}.
#' @return The value of \code{base}, as built in the body.
#' @export
VagKnee <- function(vag, fs, weights = NULL, bias = NULL, n_segments = 8) {
  base <- VagFeat(vag, fs, n_segments = n_segments)
  varms <- base$var_of_segment_ms
  feats <- c(
    base$form_factor, base$kurtosis_excess,
    if (varms > 0) log(varms) else -30,
    base$turns_per_second, base$entropy_bits
  )
  if (is.null(weights)) {
    if (!is.null(bias)) {
      stop("bias may only be given together with weights")
    }
    # Heuristic scoring against nominal normal-VAG levels; the signs follow
    # the direction the book reports for pathology.  NOT a trained model.
    w <- c(1, 0.5, 0.2, 0.01, 0.5)
    b <- -(1 * 1.2 + 0.5 * 0 + 0.2 * log(1e-4) + 0.01 * 200 + 0.5 * 5)
    trained <- FALSE
  } else {
    w <- as.numeric(weights)
    if (length(w) != 5L) stop("weights must have 5 coefficients")
    if (is.null(bias)) stop("bias is required when weights are given")
    b <- as.numeric(bias)
    trained <- TRUE
  }
  d <- b + .morie_fsum(w * feats)
  base$pathology_suspected <- d > 0
  base$discriminant <- d
  base$features <- list(
    form_factor = feats[1L],
    kurtosis_excess = feats[2L],
    log_var_segment_ms = feats[3L],
    turns_per_second = feats[4L],
    entropy_bits = feats[5L]
  )
  base$weights_used <- w
  base$bias_used <- b
  base$trained <- trained
  base$method <- paste(
    "Rangayyan (2024) Section 10.12 with the VAG feature",
    "set of Section 5.12.3 and the linear discriminant of",
    "Section 10.4.1; coefficients are the caller's unless",
    "the documented untrained heuristic is used"
  )
  base
}


#' .bsaunwrap
#'
#' Part of the rangayyan_phys implementation; see the file header for
#' the source it follows.
#'
#' @param ph See Usage.
#' @return The value of \code{out}, as built in the body.
#' @export
.bsaunwrap <- function(ph) {
  out <- numeric(length(ph))
  out[1L] <- ph[1L]
  for (i in seq_along(ph)[-1L]) {
    d <- ph[i] - out[i - 1L]
    while (d > pi) d <- d - 2 * pi
    while (d < -pi) d <- d + 2 * pi
    out[i] <- out[i - 1L] + d
  }
  out
}


# -- CLogProd: complex log turning Y = X H into log X + log H;
#    Rangayyan (2024) eq. (4.63), Section 4.8.
#' CLogProd: complex log turning Y = X H into log X + log H;
#'
#' Rangayyan (2024) eq. (4.63), Section 4.8.
#'
#' @param X See Usage.
#' @param H See Usage.
#' @param omega Defaults to \code{NULL}.
#' @return A list with \code{omega}, \code{Y_real}, \code{Y_imag}, \code{log_Y_real}, \code{log_Y_imag}, \code{log_X_real}, \code{log_X_imag}, \code{log_H_real}, \code{log_H_imag}, \code{max_abs_error}, \code{units}, \code{method}.
#' @export
CLogProd <- function(X, H, omega = NULL) {
  xs <- as.complex(X)
  hs <- as.complex(H)
  if (length(xs) != length(hs)) {
    stop("X and H must have the same length")
  }
  if (!length(xs)) stop("X and H must be non-empty")
  if (!is.null(omega)) {
    om <- as.numeric(omega)
    if (length(om) != length(xs)) {
      stop("omega must have the same length as X and H")
    }
  } else {
    om <- seq_len(length(xs)) - 1L
  }
  if (any(xs == 0)) {
    stop("X(omega) must be non-zero for all omega (eq. 4.63)")
  }
  if (any(hs == 0)) {
    stop("H(omega) must be non-zero for all omega (eq. 4.63)")
  }
  ys <- xs * hs
  lxr <- log(Mod(xs))
  lhr <- log(Mod(hs))
  lyr <- log(Mod(ys))
  lxi <- .bsaunwrap(atan2(Im(xs), Re(xs)))
  lhi <- .bsaunwrap(atan2(Im(hs), Re(hs)))
  lyi <- .bsaunwrap(atan2(Im(ys), Re(ys)))
  er <- abs(lyr - (lxr + lhr))
  # the unwrapped product phase may differ from the sum of the unwrapped
  # factor phases by a constant multiple of 2 pi
  ei <- (lyi - (lxi + lhi)) / (2 * pi)
  ei <- abs(ei - round(ei)) * 2 * pi
  err <- max(0, er, ei)
  list(
    omega = om,
    Y_real = Re(ys), Y_imag = Im(ys),
    log_Y_real = lyr, log_Y_imag = lyi,
    log_X_real = lxr, log_X_imag = lxi,
    log_H_real = lhr, log_H_imag = lhi,
    max_abs_error = err,
    units = list(
      `log magnitude` = "nepers",
      phase = "radians (unwrapped)"
    ),
    method = paste(
      "Rangayyan (2024) eq. (4.63), complex log of a product,",
      "Section 4.8 homomorphic filtering"
    )
  )
}


# -- CLogPz: complex log of a rational X(z) over its poles and zeros;
#    Rangayyan (2024) eq. (4.68) with eq. (4.67).
#' CLogPz: complex log of a rational X(z) over its poles and zeros;
#'
#' Rangayyan (2024) eq. (4.68) with eq. (4.67).
#'
#' @param z See Usage.
#' @param A Defaults to \code{1}.
#' @param r Defaults to \code{0}.
#' @param a_k Defaults to \code{complex(0)}.
#' @param b_k Defaults to \code{complex(0)}.
#' @param c_k Defaults to \code{complex(0)}.
#' @param d_k Defaults to \code{complex(0)}.
#' @param M_I Defaults to \code{NULL}.
#' @param M_O Defaults to \code{NULL}.
#' @param N_I Defaults to \code{NULL}.
#' @param N_O Defaults to \code{NULL}.
#' @return A list with \code{z_real}, \code{z_imag}, \code{xhat_real}, \code{xhat_imag}, \code{X_real}, \code{X_imag}, \code{max_abs_error}, \code{terms}, \code{counts}, \code{units}, \code{method}.
#' @export
CLogPz <- function(z, A = 1, r = 0, a_k = complex(0), b_k = complex(0),
                   c_k = complex(0), d_k = complex(0),
                   M_I = NULL, M_O = NULL, N_I = NULL, N_O = NULL) {
  zs <- as.complex(z)
  if (!length(zs)) stop("z must contain at least one point")
  A <- as.complex(A)
  if (A == 0) stop("A must be non-zero (eq. 4.67 gain)")
  r <- as.integer(r)
  sets <- list(
    a_k = as.complex(a_k), b_k = as.complex(b_k),
    c_k = as.complex(c_k), d_k = as.complex(d_k)
  )
  for (nm in names(sets)) {
    if (any(Mod(sets[[nm]]) >= 1)) {
      stop(paste0(nm, " entries must have modulus < 1 (eq. 4.67)"))
    }
  }
  decl <- list(a_k = M_I, b_k = M_O, c_k = N_I, d_k = N_O)
  for (nm in names(decl)) {
    cnt <- decl[[nm]]
    if (!is.null(cnt) && as.integer(cnt) != length(sets[[nm]])) {
      stop(paste0("declared count does not match the length of ", nm))
    }
  }
  ak <- sets$a_k
  bk <- sets$b_k
  ck <- sets$c_k
  dk <- sets$d_k
  if (r != 0L || length(ak) || length(ck)) {
    if (any(zs == 0)) {
      stop("z = 0 is a singularity of this expansion")
    }
  }
  clog <- function(w) {
    if (any(w == 0)) stop("log of zero: a factor vanishes at this z")
    complex(real = log(Mod(w)), imaginary = atan2(Im(w), Re(w)))
  }
  csum <- function(v) {
    if (!length(v)) {
      complex(real = 0, imaginary = 0)
    } else {
      sum(clog(v))
    }
  }
  n <- length(zs)
  xhat <- complex(n)
  xval <- complex(n)
  first <- NULL
  for (i in seq_len(n)) {
    zi <- zs[i]
    gain <- clog(A) + (if (r) {
      r * clog(zi)
    } else {
      complex(real = 0, imaginary = 0)
    })
    s_ai <- csum(1 - ak / zi)
    s_bo <- csum(1 - bk * zi)
    s_ci <- csum(1 - ck / zi)
    s_do <- csum(1 - dk * zi)
    tot <- gain + s_ai + s_bo - s_ci - s_do
    xhat[i] <- tot
    num <- A * (if (r) zi^r else complex(real = 1, imaginary = 0))
    for (v in ak) num <- num * (1 - v / zi)
    for (v in bk) num <- num * (1 - v * zi)
    den <- complex(real = 1, imaginary = 0)
    for (v in ck) den <- den * (1 - v / zi)
    for (v in dk) den <- den * (1 - v * zi)
    if (den == 0) {
      stop("X(z) has a pole at one of the evaluation points")
    }
    xval[i] <- num / den
    if (is.null(first)) {
      first <- list(
        gain_and_delay = c(Re(gain), Im(gain)),
        zeros_inside = c(Re(s_ai), Im(s_ai)),
        zeros_outside = c(Re(s_bo), Im(s_bo)),
        poles_inside = c(Re(s_ci), Im(s_ci)),
        poles_outside = c(Re(s_do), Im(s_do))
      )
    }
  }
  recon <- complex(
    real = exp(Re(xhat)) * cos(Im(xhat)),
    imaginary = exp(Re(xhat)) * sin(Im(xhat))
  )
  err <- max(0, Mod(recon - xval))
  list(
    z_real = Re(zs), z_imag = Im(zs),
    xhat_real = Re(xhat), xhat_imag = Im(xhat),
    X_real = Re(xval), X_imag = Im(xval),
    max_abs_error = err,
    terms = first,
    counts = list(
      M_I = length(ak), M_O = length(bk),
      N_I = length(ck), N_O = length(dk)
    ),
    units = list(
      xhat_real = "nepers (log magnitude)",
      xhat_imag = "radians (phase)", z = "dimensionless"
    ),
    method = paste(
      "Rangayyan (2024) eq. (4.68) with eq. (4.67), complex",
      "log of a rational X(z) over poles and zeros"
    )
  )
}


# pre-policy spellings
rangayyan_action_potential <- ApWave
rangayyan_cardiac_elecphys <- BiDomain
rangayyan_coronary_ad <- CadAcou
rangayyan_coronary_sound <- CorSound
rangayyan_infant_cry <- InfantCry
rangayyan_egg <- EggFeat
rangayyan_eng <- EngCap
rangayyan_epilepsy_detect <- SeizDet
rangayyan_erp_features <- ErpFeat
rangayyan_feature_extract_bci <- ErdErs
rangayyan_freq_domain_feat <- CadSpec
rangayyan_goldman_eqn <- Ghk
rangayyan_hh_gating <- HhGate
rangayyan_hodgkin_huxley <- HhModel
rangayyan_fitzhugh_nagumo <- Fhn
rangayyan_membrane_potential <- RcMemb
rangayyan_muscle_artifact <- VagClean
rangayyan_muap <- MuapModel
rangayyan_murmur_analysis <- MurmSpec
rangayyan_nernst_potential <- Nernst
rangayyan_oae <- OaeFeat
rangayyan_parkinson_multimodal <- PdMonitor
rangayyan_pcg_eeg_coupling <- PcgEeg
rangayyan_pcg_murmur_detect <- MurmDet
rangayyan_polysomnography <- PsgStage
rangayyan_point_process <- IeiStats
rangayyan_prosthetic_valve <- ValvePcg
rangayyan_respiration_features <- RespFeat
rangayyan_respiratory_sound <- RespSound
rangayyan_sleep_apnea_detect <- ApneaDet
rangayyan_speech_features <- SpeechFeat
rangayyan_vag_analysis <- VagFeat
rangayyan_vag_knee_cartilage <- VagKnee
rangayyan_ch4_complex_log_of_product <- CLogProd
rangayyan_ch4_complex_log_x_z <- CLogPz

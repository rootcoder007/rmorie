# Rangayyan filter design, part 2: the 8-point moving average, the
# integrator, the difference operators, the baseline-wander filter, the
# Butterworth family, the bilinear transformation, the DFT-indexed
# responses, the notch and comb filters, the windowed sinc and the window
# functions.  Mirror of the Python bsafilt chunks A2, B and C.
#
# Equations read from the PDF: 3.109-3.149.  Where the placeholder
# docstrings disagreed with the book, the book won -- see eq (3.111),
# whose bracket sits INSIDE the exponential product.

.morie_rg_polyroots <- function(roots) {
  # expand prod (z - r_k) into ascending-power coefficients
  coefs <- complex(real = 1, imaginary = 0)
  for (r in roots) {
    nxt <- complex(length.out = length(coefs) + 1L)
    for (i in seq_along(coefs)) {
      nxt[i] <- nxt[i] - as.complex(r) * coefs[i]
      nxt[i + 1L] <- nxt[i + 1L] + coefs[i]
    }
    coefs <- nxt
  }
  coefs
}

# ----------------------------------------------------- the 8-point average

Ma8Imp <- function(n = NULL) {
  # eq (3.109): eight equal taps.  Equal weighting is why the stopband
  # attenuation is poor -- the book notes no more than about -20 dB at
  # most frequencies, so a filtered noisy ECG is still visibly noisy.
  taps <- rep(0.125, 8)
  val <- NULL
  if (!is.null(n)) {
    i <- as.integer(n)
    val <- if (i >= 0L && i < 8L) taps[i + 1L] else 0
  }
  list(
    h = taps, value = val, index = n, n_taps = 8L, sum = 1,
    finite = TRUE, equal_weights = TRUE, attenuation_is_poor = TRUE,
    method = "Rangayyan (2024) eq. (3.109)"
  )
}

Ma8Tf <- function(z) {
  # eq (3.110): seven zeros spaced evenly round the unit circle, at every
  # multiple of fs/8 except DC.  For fs = 1000 Hz the book puts them at
  # 125, 250, 375 and 500 Hz.
  H <- .morie_rg_polyz(rep(0.125, 8), z)
  list(
    H = if (length(H) == 1L) H[[1]] else H, z = z, n_taps = 8L,
    n_zeros = 7L, zeros_at_multiples_of_fs_over_8 = TRUE,
    dc_gain = 1, always_stable = TRUE,
    method = "Rangayyan (2024) eq. (3.110)"
  )
}

Ma8Fr <- function(omega) {
  # eq (3.111).  The book's factored form is EXACT: the bracket is the sum
  # over lags -3..3, and exp(-j4w) shifts that to lags 1..7, which with
  # the leading 1 is the whole sum.  Rendering it as a product of two
  # brackets -- as the placeholder docstring did -- is a different and
  # wrong function.  Both forms are computed and compared.
  w <- as.numeric(omega)
  direct <- vapply(w, function(v) {
    0.125 * sum(complex(
      real = cos(-v * (0:7)),
      imaginary = sin(-v * (0:7))
    ))
  }, complex(1))
  brack <- 1 + 2 * cos(w) + 2 * cos(2 * w) + 2 * cos(3 * w)
  factored <- 0.125 * (1 + complex(
    real = cos(-4 * w),
    imaginary = sin(-4 * w)
  ) * brack)
  gap <- max(Mod(direct - factored))
  scalar <- length(direct) == 1L
  list(
    H = if (scalar) direct[[1]] else direct,
    factored = if (scalar) factored[[1]] else factored,
    omega = omega,
    magnitude = if (scalar) Mod(direct[[1]]) else Mod(direct),
    max_difference = gap, factored_form_agrees = gap <= 1e-12,
    bracket_is_inside_the_product = TRUE,
    method = "Rangayyan (2024) eq. (3.111)"
  )
}

Ma8Rec <- function(x, n = NULL) {
  # eq (3.120): y(n) = y(n-1) + (1/8)x(n) - (1/8)x(n-8).  Two additions a
  # sample instead of eight, and it "clearly depicts the integration
  # aspect".  The cost is that error accumulates -- the recursion never
  # forgets, where the direct form flushes a perturbation after 8 samples.
  xs <- as.numeric(x)
  if (!length(xs)) stop("need at least one sample")
  out <- numeric(length(xs))
  acc <- 0
  for (i in seq_along(xs)) {
    acc <- acc + 0.125 * xs[i]
    if (i > 8L) acc <- acc - 0.125 * xs[i - 8L]
    out[i] <- acc
  }
  direct <- vapply(seq_along(xs), function(i) {
    k <- which(i - (0:7) >= 1L)
    .morie_fsum(xs[i - (k - 1L)]) / 8
  }, numeric(1))
  gap <- max(abs(out - direct))
  val <- NULL
  if (!is.null(n)) {
    i <- as.integer(n)
    if (i < 0L || i >= length(out)) stop("n is outside the record")
    val <- out[i + 1L]
  }
  list(
    y = out, value = val, index = n, direct_form = direct,
    max_difference = gap, agrees_with_direct_form = gap <= 1e-9,
    additions_per_sample = 2L, direct_form_additions = 8L,
    error_accumulates = TRUE,
    method = "Rangayyan (2024) eq. (3.120)"
  )
}

Ma8RecTf <- function(z) {
  # eq (3.121): a pole at z = 1 cancelled by one of the numerator's zeros,
  # so the filter is still FIR despite the recursive implementation.  At
  # z = 1 the ratio is 0/0 and the limit is the DC gain, 1.
  zc <- as.complex(z)
  if (any(zc == 0)) {
    stop("z = 0 is a pole of a causal transfer function")
  }
  H <- vapply(zc, function(v) {
    den <- 1 - v^-1
    if (Mod(den) <= 1e-12) {
      complex(real = 1, imaginary = 0)
    } else {
      0.125 * (1 - v^-8) / den
    }
  }, complex(1))
  list(
    H = if (length(H) == 1L) H[[1]] else H, z = z,
    pole_at_dc_cancelled_by_a_zero = TRUE, still_fir = TRUE,
    dc_gain = 1, removable_singularity_at_z_equals_one = TRUE,
    method = "Rangayyan (2024) eq. (3.121)"
  )
}

Ma8Sinc <- function(omega) {
  # eq (3.122): the Dirichlet kernel, a real sinc-like envelope times a
  # pure delay of 7/2 samples.  The book states it "is equivalent to that
  # in Equation 3.111", and both are recomputed and compared.  The delay
  # is not an integer -- the price of an even-length filter.
  w <- as.numeric(omega)
  closed <- vapply(w, function(v) {
    s2 <- sin(v / 2)
    if (abs(s2) <= 1e-12) {
      complex(real = 1, imaginary = 0)
    } else {
      0.125 * complex(
        real = cos(-3.5 * v),
        imaginary = sin(-3.5 * v)
      ) * sin(4 * v) / s2
    }
  }, complex(1))
  direct <- vapply(w, function(v) {
    0.125 * sum(complex(
      real = cos(-v * (0:7)),
      imaginary = sin(-v * (0:7))
    ))
  }, complex(1))
  gap <- max(Mod(closed - direct))
  scalar <- length(closed) == 1L
  list(
    H = if (scalar) closed[[1]] else closed, omega = omega,
    direct_sum = if (scalar) direct[[1]] else direct,
    max_difference = gap, agrees_with_eq_3_111 = gap <= 1e-9,
    group_delay = 3.5, delay_is_not_an_integer = TRUE,
    method = "Rangayyan (2024) eq. (3.122)"
  )
}

# --------------------------------------------------------- the integrator

RunInt <- function(x, t, tau) {
  # eq (3.112): the continuous counterpart of the moving-average sum.  The
  # window is clipped at the start of the record, and how many windows
  # that affects is reported rather than left to be discovered.
  xs <- as.numeric(x)
  ts <- as.numeric(t)
  if (length(xs) != length(ts)) {
    stop("x and t must have the same length")
  }
  if (length(xs) < 2L) stop("need at least two samples to integrate")
  if (any(diff(ts) <= 0)) stop("t must be strictly increasing")
  tv <- as.numeric(tau)
  if (tv <= 0) stop("tau must be positive")
  clipped <- 0L
  out <- numeric(length(ts))
  for (i in seq_along(ts)) {
    lo <- ts[i] - tv
    if (lo < ts[1]) {
      lo <- ts[1]
      clipped <- clipped + 1L
    }
    acc <- 0
    if (i > 1L) {
      for (j in seq_len(i - 1L)) {
        a <- ts[j]
        b <- ts[j + 1L]
        if (b <= lo) next
        fa <- xs[j]
        fb <- xs[j + 1L]
        if (a < lo) {
          fa <- fa + (fb - fa) * (lo - a) / (b - a)
          a <- lo
        }
        acc <- acc + 0.5 * (fa + fb) * (b - a)
      }
    }
    out[i] <- acc
  }
  list(
    y = out, n = length(out), tau = tv, clipped_windows = clipped,
    trapezoidal = TRUE,
    continuous_counterpart_of_the_ma_filter = TRUE,
    method = "Rangayyan (2024) eq. (3.112)"
  )
}

RunIntAll <- function(x, t) {
  # eq (3.113).  Over a finite record the lower limit is the first sample,
  # so any mass before it is unobserved and the constant of integration is
  # arbitrary.  The discrete counterpart has a pole ON the unit circle at
  # DC, which is why the book says it is seldom used for filtering.
  xs <- as.numeric(x)
  ts <- as.numeric(t)
  if (length(xs) != length(ts)) {
    stop("x and t must have the same length")
  }
  if (length(xs) < 2L) stop("need at least two samples to integrate")
  if (any(diff(ts) <= 0)) stop("t must be strictly increasing")
  out <- c(0, cumsum(0.5 * (xs[-length(xs)] + xs[-1]) * diff(ts)))
  list(
    y = out, n = length(out), total = out[length(out)],
    lower_limit = ts[1],
    constant_of_integration_is_arbitrary = TRUE,
    discrete_pole_on_the_unit_circle = TRUE,
    seldom_used_for_filtering = TRUE,
    method = "Rangayyan (2024) eq. (3.113)"
  )
}

IntFt <- function(X, omega, X0 = NULL) {
  # eq (3.115): Y(w) = X(w)/(jw) + pi X(0) delta(w).  The delta carries
  # the DC content, which 1/(jw) cannot represent because it blows up
  # there.  It is returned as its WEIGHT, pi X(0); a delta has no value
  # at a point.
  w <- as.numeric(omega)
  Xs <- as.complex(X)
  if (length(Xs) == 1L) Xs <- rep(Xs, length(w))
  if (length(Xs) != length(w)) {
    stop("X and omega must have the same length")
  }
  at_dc <- abs(w) <= 1e-300
  Y <- vector("list", length(w))
  for (i in seq_along(w)) {
    # Y[[i]] <- NULL would DELETE the element; Y[i] <- list(NULL) sets it
    if (at_dc[i]) {
      Y[i] <- list(NULL)
    } else {
      Y[[i]] <- Xs[i] / complex(real = 0, imaginary = w[i])
    }
  }
  scalar <- length(w) == 1L
  list(
    Y = if (scalar) Y[[1]] else Y, omega = omega,
    delta_weight = if (is.null(X0)) NULL else pi * as.complex(X0),
    at_dc = if (scalar) at_dc[[1]] else at_dc,
    dc_term_carried_by_the_delta = TRUE,
    undefined_at_zero_without_the_delta = TRUE,
    method = "Rangayyan (2024) eq. (3.115)"
  )
}

IntFr <- function(omega) {
  # eq (3.116): H(w) = 1/(jw), the DC term of eq (3.115) set aside as the
  # book does.  The gain falls as frequency rises, so it is a lowpass, and
  # it is unbounded at w = 0 -- refused rather than returned as Inf.
  w <- as.numeric(omega)
  if (any(abs(w) <= 1e-300)) {
    stop(
      "H(w) = 1/(jw) is unbounded at w = 0; the DC content sits in ",
      "the delta term of eq. (3.115)"
    )
  }
  H <- 1 / complex(real = 0, imaginary = w)
  list(
    H = if (length(H) == 1L) H[[1]] else H, omega = omega,
    lowpass = TRUE, dc_term_set_aside = TRUE,
    gain_falls_nonlinearly_with_frequency = TRUE,
    method = "Rangayyan (2024) eq. (3.116)"
  )
}

IntMag <- function(omega) {
  # eq (3.117).  The book prints 1/w, which is right for w > 0 and is how
  # the response is plotted; a magnitude cannot be negative, so the
  # absolute value is taken here.
  w <- as.numeric(omega)
  if (any(abs(w) <= 1e-300)) {
    stop("the magnitude is unbounded at w = 0")
  }
  mag <- 1 / abs(w)
  list(
    magnitude = if (length(mag) == 1L) mag[[1]] else mag,
    omega = omega, book_prints_one_over_omega = TRUE,
    absolute_value_needed_for_negative_omega = TRUE,
    method = "Rangayyan (2024) eq. (3.117)"
  )
}

IntPh <- function(omega) {
  # eq (3.118): a constant -pi/2, because 1/(jw) is a fixed quarter turn.
  # A constant phase is NOT a constant delay: the group delay is the
  # derivative, which is zero, so the integrator delays nothing.
  w <- as.numeric(omega)
  if (any(abs(w) <= 1e-300)) stop("the phase is undefined at w = 0")
  ph <- ifelse(w > 0, -pi / 2, pi / 2)
  list(
    phase = if (length(ph) == 1L) ph[[1]] else ph, omega = omega,
    constant = TRUE, group_delay = 0,
    constant_phase_is_not_constant_delay = TRUE,
    sign_flips_for_negative_omega = TRUE,
    method = "Rangayyan (2024) eq. (3.118)"
  )
}

# ----------------------------------------------- the difference operators

FDiff <- function(x, T = 1, n = NULL) {
  # eq (3.123).  The 1/T is not cosmetic: the book is explicit that it "is
  # required in order to obtain the rate of change of the signal with
  # respect to the true time".  Highpass, and it amplifies noise --
  # eq (3.128) is the book's remedy.
  xs <- as.numeric(x)
  if (!length(xs)) stop("need at least one sample")
  Tv <- as.numeric(T)
  if (Tv <= 0) stop("the sampling interval T must be positive")
  out <- (xs - c(0, xs[-length(xs)])) / Tv
  val <- NULL
  if (!is.null(n)) {
    i <- as.integer(n)
    if (i < 0L || i >= length(out)) stop("n is outside the record")
    val <- out[i + 1L]
  }
  list(
    y = out, value = val, index = n, T = Tv,
    scale_factor_gives_true_time_rate = TRUE, highpass = TRUE,
    amplifies_noise = TRUE, removes_dc = TRUE,
    method = "Rangayyan (2024) eq. (3.123)"
  )
}

FDiffTf <- function(z, T = 1) {
  # eq (3.124): one zero, at z = 1, the DC point -- that single zero is
  # the whole of the operator's highpass character.
  Tv <- as.numeric(T)
  if (Tv <= 0) stop("the sampling interval T must be positive")
  H <- .morie_rg_polyz(c(1 / Tv, -1 / Tv), z)
  list(
    H = if (length(H) == 1L) H[[1]] else H, z = z, T = Tv,
    zeros = 1, zero_at_dc = TRUE, dc_gain = 0,
    method = "Rangayyan (2024) eq. (3.124)"
  )
}

FDiffFr <- function(omega, T = 1) {
  # eq (3.125).  The second form separates a half-sample delay from a real
  # gain; the factor of j is what puts the phase a quarter turn ahead, the
  # +pi/2 of eq (3.127).
  Tv <- as.numeric(T)
  if (Tv <= 0) stop("the sampling interval T must be positive")
  w <- as.numeric(omega)
  raw <- (1 - complex(real = cos(-w), imaginary = sin(-w))) / Tv
  split <- complex(real = cos(-w / 2), imaginary = sin(-w / 2)) *
    complex(real = 0, imaginary = 2 * sin(w / 2)) / Tv
  gap <- max(Mod(raw - split))
  scalar <- length(raw) == 1L
  list(
    H = if (scalar) raw[[1]] else raw, omega = omega, T = Tv,
    split_form = if (scalar) split[[1]] else split,
    max_difference = gap, forms_agree = gap <= 1e-12,
    half_sample_delay = 0.5,
    method = "Rangayyan (2024) eq. (3.125)"
  )
}

FDiffMag <- function(omega, T = 1) {
  # eq (3.126).  The book prints (2/T) sin(w/2) without bars, right on
  # 0 <= w <= pi, the range plotted; the absolute value is needed outside
  # it.  Largest at Nyquist, which is why the operator amplifies
  # high-frequency noise.
  Tv <- as.numeric(T)
  if (Tv <= 0) stop("the sampling interval T must be positive")
  w <- as.numeric(omega)
  mag <- 2 * abs(sin(w / 2)) / Tv
  list(
    magnitude = if (length(mag) == 1L) mag[[1]] else mag,
    omega = omega, T = Tv, dc_gain = 0, nyquist_gain = 2 / Tv,
    roughly_proportional_to_frequency = TRUE,
    book_omits_the_absolute_value = TRUE,
    method = "Rangayyan (2024) eq. (3.126)"
  )
}

FDiffPh <- function(omega) {
  # eq (3.127): slope -1/2, so half a sample of group delay plus the
  # quarter turn from the j of eq (3.125).  A half-sample delay cannot be
  # undone by shifting samples.
  w <- as.numeric(omega)
  ph <- pi / 2 - w / 2
  list(
    phase = if (length(ph) == 1L) ph[[1]] else ph, omega = omega,
    group_delay = 0.5, slope = -0.5, quarter_turn_offset = pi / 2,
    linear_phase = TRUE,
    method = "Rangayyan (2024) eq. (3.127)"
  )
}

CDiff3 <- function(x, T = 1, n = NULL) {
  # eq (3.128): the mean of two successive first differences, which
  # controls the noise amplification.  The book warns the price is
  # accuracy -- the approximation to d/dt "is poor after about fs/10".
  xs <- as.numeric(x)
  if (!length(xs)) stop("need at least one sample")
  Tv <- as.numeric(T)
  if (Tv <= 0) stop("the sampling interval T must be positive")
  lag2 <- c(0, 0, xs[seq_len(max(0L, length(xs) - 2L))])[seq_along(xs)]
  out <- (xs - lag2) / (2 * Tv)
  d1 <- FDiff(xs, T = Tv)$y
  avg <- 0.5 * (d1 + c(0, d1[-length(d1)]))
  gap <- max(abs(out - avg))
  val <- NULL
  if (!is.null(n)) {
    i <- as.integer(n)
    if (i < 0L || i >= length(out)) stop("n is outside the record")
    val <- out[i + 1L]
  }
  list(
    y = out, value = val, index = n, T = Tv,
    as_averaged_first_differences = avg, max_difference = gap,
    derivation_agrees = gap <= 1e-9,
    controls_noise_amplification = TRUE,
    poor_above_fs_over_10 = TRUE,
    method = "Rangayyan (2024) eq. (3.128)"
  )
}

CDiff3Tf <- function(z, T = 1) {
  # eq (3.129).  The factored form is the point: the operator IS a
  # first-order difference in series with a two-point moving average, so
  # it may be built as that cascade.  Zeros at z = 1 and z = -1 make it a
  # bandpass, and the one at Nyquist is what kills the noise boost.
  Tv <- as.numeric(T)
  if (Tv <= 0) stop("the sampling interval T must be positive")
  zc <- as.complex(z)
  if (any(zc == 0)) {
    stop("z = 0 is a pole of a causal transfer function")
  }
  direct <- (1 - zc^-2) / (2 * Tv)
  cascade <- ((1 - zc^-1) / Tv) * (0.5 * (1 + zc^-1))
  gap <- max(Mod(direct - cascade))
  scalar <- length(direct) == 1L
  list(
    H = if (scalar) direct[[1]] else direct, z = z, T = Tv,
    cascade = if (scalar) cascade[[1]] else cascade,
    max_difference = gap, cascade_agrees = gap <= 1e-12,
    zeros = c(1, -1), bandpass = TRUE,
    is_first_difference_times_two_point_ma = TRUE,
    method = "Rangayyan (2024) eq. (3.129)"
  )
}

CDiff3Mag <- function(omega, T = 1) {
  # eq (3.130): (1/T)|sin w|.  Nought at BOTH ends -- at DC from the
  # highpass factor, at Nyquist from the moving-average factor -- peaking
  # at w = pi/2.
  Tv <- as.numeric(T)
  if (Tv <= 0) stop("the sampling interval T must be positive")
  w <- as.numeric(omega)
  mag <- abs(sin(w)) / Tv
  list(
    magnitude = if (length(mag) == 1L) mag[[1]] else mag,
    omega = omega, T = Tv, dc_gain = 0, nyquist_gain = 0,
    peak_at = pi / 2, bandpass = TRUE,
    method = "Rangayyan (2024) eq. (3.130)"
  )
}

CDiff3Ph <- function(omega) {
  # eq (3.131): slope -1, so a WHOLE sample of group delay against the
  # half sample of the plain difference.  An integer delay can be undone
  # by shifting the output back.
  w <- as.numeric(omega)
  ph <- pi / 2 - w
  list(
    phase = if (length(ph) == 1L) ph[[1]] else ph, omega = omega,
    group_delay = 1, slope = -1, quarter_turn_offset = pi / 2,
    integer_delay_can_be_undone_by_shifting = TRUE,
    method = "Rangayyan (2024) eq. (3.131)"
  )
}

Diff1 <- function(x, T = 1) {
  # eq (3.123) run over a record, with the coefficients reported so the
  # highpass character is visible.
  r <- FDiff(x, T = T)
  r$b <- c(1 / as.numeric(T), -1 / as.numeric(T))
  r$a <- 1
  r$zeros <- 1
  r$highpass <- TRUE
  r$use_bwander_to_avoid_the_noise_boost <- TRUE
  r$method <- "Rangayyan (2024) eq. (3.123) applied to a record"
  r
}

Diff2 <- function(x, T = 1, n = NULL) {
  # The second derivative has response (jw)(jw) = -w^2, a QUADRATIC rise
  # with frequency, and the book notes it "may be realized as a cascade of
  # two" first differences.  Both are computed and compared.
  xs <- as.numeric(x)
  if (!length(xs)) stop("need at least one sample")
  Tv <- as.numeric(T)
  if (Tv <= 0) stop("the sampling interval T must be positive")
  l1 <- c(0, xs[-length(xs)])
  l2 <- c(0, 0, xs[seq_len(max(0L, length(xs) - 2L))])[seq_along(xs)]
  out <- (xs - 2 * l1 + l2) / (Tv * Tv)
  cascade <- FDiff(FDiff(xs, T = Tv)$y, T = Tv)$y
  gap <- max(abs(out - cascade))
  val <- NULL
  if (!is.null(n)) {
    i <- as.integer(n)
    if (i < 0L || i >= length(out)) stop("n is outside the record")
    val <- out[i + 1L]
  }
  list(
    y = out, value = val, index = n, T = Tv,
    as_cascaded_first_differences = cascade, max_difference = gap,
    cascade_agrees = gap <= 1e-9,
    b = c(1, -2, 1) / (Tv * Tv), a = 1, zeros = c(1, 1),
    double_zero_at_dc = TRUE, gain_rises_quadratically = TRUE,
    method = "Rangayyan (2024) Section 3.3.3 (second derivative)"
  )
}

# ---------------------------------------------- the baseline-wander filter

BWander <- function(z, T = 1, pole = 0.995) {
  # eq (3.132): the first difference with a pole just inside the unit
  # circle at DC.  The pole nearly cancels the zero away from DC, so the
  # baseline goes without the wholesale high-frequency boost of
  # eq (3.123).  A pole AT 1 would cancel the zero exactly.
  Tv <- as.numeric(T)
  if (Tv <= 0) stop("the sampling interval T must be positive")
  p <- as.numeric(pole)
  if (!(p >= 0 && p < 1)) {
    stop(
      "the pole must lie inside the unit circle, 0 <= pole < 1; at 1 ",
      "it cancels the zero exactly"
    )
  }
  zc <- as.complex(z)
  if (any(zc == 0)) {
    stop("z = 0 is a pole of a causal transfer function")
  }
  den <- 1 - p * zc^-1
  if (any(Mod(den) <= 1e-300)) stop("z is the pole of H(z)")
  H <- (1 - zc^-1) / (Tv * den)
  list(
    H = if (length(H) == 1L) H[[1]] else H, z = z, T = Tv, pole = p,
    zeros = 1, poles = p, dc_gain = 0,
    pole_nearly_cancels_the_zero_away_from_dc = TRUE,
    no_longer_fir = TRUE,
    method = "Rangayyan (2024) eq. (3.132)"
  )
}

BWanderZ <- function(z, T = 1, pole = 0.995) {
  # eq (3.133): the same filter in positive powers of z.  The book keeps
  # this form because the graphical method reads it directly -- numerator
  # is the distance to the zero at 1, denominator the distance to the pole.
  Tv <- as.numeric(T)
  if (Tv <= 0) stop("the sampling interval T must be positive")
  p <- as.numeric(pole)
  if (!(p >= 0 && p < 1)) {
    stop("the pole must lie inside the unit circle")
  }
  zc <- as.complex(z)
  if (any(Mod(zc - p) <= 1e-300)) stop("z is the pole of H(z)")
  H <- (zc - 1) / (Tv * (zc - p))
  other <- BWander(z, T = Tv, pole = p)$H
  gap <- max(Mod(H - other))
  list(
    H = if (length(H) == 1L) H[[1]] else H, z = z, T = Tv, pole = p,
    max_difference_from_eq_3_132 = gap, forms_agree = gap <= 1e-9,
    numerator_is_the_distance_to_the_zero = TRUE,
    denominator_is_the_distance_to_the_pole = TRUE,
    method = "Rangayyan (2024) eq. (3.133)"
  )
}

BWanderEq <- function(x, T = 1, pole = 0.995, n = NULL) {
  # eq (3.134).  Note the PLUS on the feedback: the pole's coefficient is
  # already on the right-hand side, so this does NOT carry the minus of
  # the general form in eq (3.68).  Taking the sign from eq (3.68) puts
  # the pole at -0.995 -- a highpass at Nyquist, not at DC.
  xs <- as.numeric(x)
  if (!length(xs)) stop("need at least one sample")
  Tv <- as.numeric(T)
  if (Tv <= 0) stop("the sampling interval T must be positive")
  p <- as.numeric(pole)
  if (!(p >= 0 && p < 1)) {
    stop("the pole must lie inside the unit circle")
  }
  out <- numeric(length(xs))
  for (i in seq_along(xs)) {
    px <- if (i >= 2L) xs[i - 1L] else 0
    py <- if (i >= 2L) out[i - 1L] else 0
    out[i] <- (xs[i] - px) / Tv + p * py
  }
  val <- NULL
  if (!is.null(n)) {
    i <- as.integer(n)
    if (i < 0L || i >= length(out)) stop("n is outside the record")
    val <- out[i + 1L]
  }
  list(
    y = out, value = val, index = n, T = Tv, pole = p,
    feedback_sign = "+", iir = TRUE,
    sign_already_moved_to_the_right_hand_side = TRUE,
    method = "Rangayyan (2024) eq. (3.134)"
  )
}

# ---------------------------------------------------- the Butterworth family

BwSqMag <- function(Omega, Omega_c, N) {
  # eq (3.135): monotonic in both bands, no ripple anywhere -- the
  # defining Butterworth property.  At the cutoff the squared magnitude is
  # exactly 1/2 FOR EVERY ORDER, so raising N steepens the transition
  # without moving the half-power point.
  Wc <- as.numeric(Omega_c)
  if (Wc <= 0) stop("the cutoff Omega_c must be positive")
  n <- as.integer(N)
  if (n < 1L) stop("the order N must be at least 1")
  w <- as.numeric(Omega)
  sq <- 1 / (1 + (abs(w) / Wc)^(2 * n))
  scalar <- length(sq) == 1L
  list(
    squared_magnitude = if (scalar) sq[[1]] else sq,
    magnitude = if (scalar) sqrt(sq[[1]]) else sqrt(sq),
    Omega = Omega, Omega_c = Wc, N = n, half_power_at_cutoff = 0.5,
    monotonic = TRUE, no_ripple = TRUE,
    cutoff_is_half_power_for_every_order = TRUE,
    method = "Rangayyan (2024) eq. (3.135)"
  )
}

BwSqLap <- function(s, Omega_c, N) {
  # eq (3.136): 2N poles, half in the right half-plane, so this is NOT a
  # filter until the N left-half-plane ones are selected by eq (3.138).
  Wc <- as.numeric(Omega_c)
  if (Wc <= 0) stop("the cutoff Omega_c must be positive")
  n <- as.integer(N)
  if (n < 1L) stop("the order N must be at least 1")
  sc <- as.complex(s)
  den <- 1 + (sc / complex(real = 0, imaginary = Wc))^(2 * n)
  if (any(Mod(den) <= 1e-300)) {
    stop("s is a pole of H_a(s) H_a(-s)")
  }
  H <- 1 / den
  list(
    H = if (length(H) == 1L) H[[1]] else H, s = s, Omega_c = Wc, N = n,
    n_poles = 2L * n, half_are_right_half_plane = TRUE,
    not_a_filter_until_the_poles_are_selected = TRUE,
    method = "Rangayyan (2024) eq. (3.136)"
  )
}

BwPoles <- function(Omega_c, N, k = NULL) {
  # eq (3.137): all 2N poles on a circle of radius Omega_c, spaced pi/N
  # apart, symmetric about the imaginary axis and never on it.  For odd N
  # a pole falls on the real axis.  Complex poles come in conjugate pairs,
  # which keeps the filter coefficients real.
  Wc <- as.numeric(Omega_c)
  if (Wc <= 0) stop("the cutoff Omega_c must be positive")
  n <- as.integer(N)
  if (n < 1L) stop("the order N must be at least 1")
  ang <- pi * (0.5 + (2 * seq_len(2L * n) - 1) / (2 * n))
  allp <- Wc * complex(real = cos(ang), imaginary = sin(ang))
  lhp <- allp[Re(allp) < 0]
  val <- NULL
  if (!is.null(k)) {
    kk <- as.integer(k)
    if (kk < 1L || kk > 2L * n) stop("k must lie in 1..2N")
    val <- allp[kk]
  }
  list(
    poles = allp, left_half_plane = lhp, value = val, k = k,
    Omega_c = Wc, N = n, radius = Wc, angular_spacing = pi / n,
    n_left_half_plane = length(lhp),
    none_on_the_imaginary_axis = all(abs(Re(allp)) > 1e-12),
    real_pole_for_odd_order = n %% 2L == 1L,
    method = "Rangayyan (2024) eq. (3.137)"
  )
}

BwAnalog <- function(Omega_c, N, G = NULL, s = NULL) {
  # eq (3.138): built from the N LEFT-half-plane poles only.  With no gain
  # given, G normalizes the DC gain to unity, which makes it Omega_c^N.
  # The denominator coefficients are real to rounding because the poles
  # arrive in conjugate pairs; the residue is returned as a check.
  Wc <- as.numeric(Omega_c)
  if (Wc <= 0) stop("the cutoff Omega_c must be positive")
  n <- as.integer(N)
  if (n < 1L) stop("the order N must be at least 1")
  poles <- BwPoles(Wc, n)$left_half_plane
  if (length(poles) != n) {
    stop("expected ", n, " left-half-plane poles, found ", length(poles))
  }
  coefs <- .morie_rg_polyroots(poles)
  resid <- max(abs(Im(coefs)))
  den <- Re(coefs)
  gain <- if (is.null(G)) den[1] else as.numeric(G)
  Hs <- NULL
  if (!is.null(s)) {
    sc <- as.complex(s)
    d <- vapply(sc, function(v) {
      sum(den * v^(seq_along(den) - 1L))
    }, complex(1))
    if (any(Mod(d) <= 1e-300)) stop("s is a pole of H_a(s)")
    Hs <- gain / d
    if (length(Hs) == 1L) Hs <- Hs[[1]]
  }
  list(
    poles = poles, denominator = den, gain = gain, H = Hs,
    Omega_c = Wc, N = n, max_imaginary_residue = resid,
    coefficients_are_real = resid <= 1e-9 * max(1, max(abs(den))),
    gain_normalizes_dc_to_unity = is.null(G),
    left_half_plane_only = TRUE,
    method = "Rangayyan (2024) eq. (3.138)"
  )
}

# ------------------------------------------------ the bilinear transformation

Bilinear <- function(z, T = 1) {
  # eq (3.139): maps the whole left half-plane into the unit disc, so a
  # stable analog filter always yields a stable digital one -- unlike
  # impulse invariance, which aliases.  The price is the frequency warping
  # of eqs (3.141)-(3.142).  z = -1 maps to infinity.
  Tv <- as.numeric(T)
  if (Tv <= 0) stop("the sampling interval T must be positive")
  zc <- as.complex(z)
  if (any(zc == 0)) {
    stop("z = 0 is not in the domain of the bilinear transformation")
  }
  den <- 1 + zc^-1
  if (any(Mod(den) <= 1e-300)) stop("z = -1 maps to s = infinity")
  s <- (2 / Tv) * (1 - zc^-1) / den
  list(
    s = if (length(s) == 1L) s[[1]] else s, z = z, T = Tv,
    maps_lhp_into_the_unit_disc = TRUE,
    stability_is_preserved = TRUE, no_aliasing = TRUE,
    warps_the_frequency_axis = TRUE,
    method = "Rangayyan (2024) eq. (3.139)"
  )
}

BilinUnit <- function(omega, T = 1) {
  # eq (3.140): on the unit circle sigma vanishes exactly, so the
  # imaginary axis maps onto the unit circle and nowhere else.  The
  # residual real part is returned as a check.
  Tv <- as.numeric(T)
  if (Tv <= 0) stop("the sampling interval T must be positive")
  w <- as.numeric(omega)
  zc <- complex(real = cos(w), imaginary = sin(w))
  den <- 1 + zc^-1
  if (any(Mod(den) <= 1e-300)) stop("w = pi maps to s = infinity")
  direct <- (2 / Tv) * (1 - zc^-1) / den
  closed <- complex(real = 0, imaginary = 2 * tan(w / 2) / Tv)
  gap <- max(Mod(direct - closed))
  sigma <- max(abs(Re(direct)))
  scalar <- length(direct) == 1L
  list(
    s = if (scalar) direct[[1]] else direct, omega = omega, T = Tv,
    closed_form = if (scalar) closed[[1]] else closed,
    max_difference = gap, forms_agree = gap <= 1e-9,
    max_real_part = sigma, sigma_vanishes = sigma <= 1e-9,
    method = "Rangayyan (2024) eq. (3.140)"
  )
}

BilinWarp <- function(omega, T = 1) {
  # eq (3.141): the prewarping step.  Nonlinear, so a digital cutoff
  # cannot be handed to an analog design unchanged; skipping it puts the
  # realized cutoff below the one asked for, increasingly so near Nyquist.
  Tv <- as.numeric(T)
  if (Tv <= 0) stop("the sampling interval T must be positive")
  w <- as.numeric(omega)
  if (any(abs(w) >= pi)) {
    stop(
      "eq. (3.141) needs |w| < pi; w = pi maps to an infinite analog ",
      "frequency"
    )
  }
  W <- (2 / Tv) * tan(w / 2)
  list(
    Omega = if (length(W) == 1L) W[[1]] else W, omega = omega, T = Tv,
    nonlinear = TRUE, prewarping_is_required = TRUE,
    compression_is_severe_near_nyquist = TRUE,
    method = "Rangayyan (2024) eq. (3.141)"
  )
}

BilinUnwarp <- function(Omega, T = 1) {
  # eq (3.142): the inverse of eq (3.141); the two compose to the
  # identity, which is checked.  Every finite analog frequency lands
  # strictly inside (-pi, pi).
  Tv <- as.numeric(T)
  if (Tv <= 0) stop("the sampling interval T must be positive")
  W <- as.numeric(Omega)
  w <- 2 * atan(W * Tv / 2)
  back <- (2 / Tv) * tan(w / 2)
  gap <- if (length(W)) max(abs(W - back)) else 0
  list(
    omega = if (length(w) == 1L) w[[1]] else w, Omega = Omega, T = Tv,
    round_trip_error = gap, inverts_eq_3_141 = gap <= 1e-9,
    always_inside_the_open_interval = all(abs(w) < pi),
    method = "Rangayyan (2024) eq. (3.142)"
  )
}

BwDigital <- function(Omega_c = NULL, N = NULL, T = 1, fc = NULL,
                      fs = NULL, z = NULL) {
  # eq (3.143).  The N zeros at z = -1 are not a design choice: the
  # bilinear transform puts them there, since s = infinity maps to z = -1
  # and the analog prototype has all its zeros at infinity.  Prewarping is
  # done here rather than left to the caller to forget.
  Tv <- as.numeric(T)
  if (Tv <= 0) stop("the sampling interval T must be positive")
  if (is.null(N)) stop("the order N is required")
  n <- as.integer(N)
  if (n < 1L) stop("the order N must be at least 1")
  if (is.null(Omega_c) == is.null(fc)) {
    stop(
      "give either the prewarped Omega_c or a digital cutoff fc with ",
      "fs, not both and not neither"
    )
  }
  if (!is.null(fc)) {
    if (is.null(fs)) stop("fc needs the sampling rate fs")
    fsv <- as.numeric(fs)
    fcv <- as.numeric(fc)
    if (!(fcv > 0 && fcv < fsv / 2)) {
      stop(
        "the cutoff must lie strictly between 0 and the Nyquist ",
        "frequency"
      )
    }
    Tv <- 1 / fsv
    Wc <- (2 / Tv) * tan(pi * fcv / fsv)
    prewarped <- TRUE
  } else {
    Wc <- as.numeric(Omega_c)
    if (Wc <= 0) stop("the cutoff Omega_c must be positive")
    prewarped <- FALSE
  }
  ps <- BwPoles(Wc, n)$left_half_plane
  pz <- (2 / Tv + ps) / (2 / Tv - ps)
  den <- Re(.morie_rg_polyroots(pz))
  den <- den / den[length(den)]
  a <- rev(den)
  b <- rev(Re(.morie_rg_polyroots(rep(-1, n))))
  dcn <- .morie_fsum(b)
  dcd <- .morie_fsum(a)
  if (abs(dcn) <= 1e-300) stop("the numerator vanishes at DC")
  Gp <- dcd / dcn
  b <- Gp * b
  Hz <- NULL
  if (!is.null(z)) {
    dd <- .morie_rg_polyz(a, z)
    if (any(Mod(dd) <= 1e-300)) stop("z is a pole of H(z)")
    Hz <- .morie_rg_polyz(b, z) / dd
    if (length(Hz) == 1L) Hz <- Hz[[1]]
  }
  list(
    b = b, a = a, gain = Gp, poles_z = pz, H = Hz, N = n,
    Omega_c = Wc, T = Tv, prewarped_here = prewarped,
    zeros_at_minus_one = n,
    zeros_are_forced_by_the_bilinear_transform = TRUE,
    dc_gain = 1, leading_a_is_one = abs(a[1] - 1) < 1e-12,
    method = "Rangayyan (2024) eq. (3.143)"
  )
}

IirDiffGen <- function(x, b_k, a_k = NULL, n = NULL) {
  # eq (3.144): the time-domain form of eq (3.143), and how a designed
  # filter is actually run over data.  The feedback is SUBTRACTED, as in
  # eq (3.68), and a_k is a_1..a_N without the leading a_0 = 1.
  IirDiff(x, b_k, a_k = a_k, n = n)
}

BwDirect <- function(omega, omega_c, N) {
  # eq (3.145): specified on the discrete-frequency axis outright, so
  # there is no warping to prewarp for.  The filter so defined has zero
  # phase, which is only usable with the whole record in hand -- it is not
  # causal and cannot be run sample by sample.
  wc <- as.numeric(omega_c)
  if (wc <= 0) stop("the cutoff omega_c must be positive")
  n <- as.integer(N)
  if (n < 1L) stop("the order N must be at least 1")
  w <- as.numeric(omega)
  sq <- 1 / (1 + (abs(w) / wc)^(2 * n))
  scalar <- length(sq) == 1L
  list(
    squared_magnitude = if (scalar) sq[[1]] else sq,
    magnitude = if (scalar) sqrt(sq[[1]]) else sqrt(sq),
    omega = omega, omega_c = wc, N = n, half_power_at_cutoff = 0.5,
    no_warping = TRUE, zero_phase = TRUE, not_causal = TRUE,
    method = "Rangayyan (2024) eq. (3.145)"
  )
}

BwLpDft <- function(K, kc = NULL, N = 2, fc = NULL, fs = NULL) {
  # eq (3.146), valid for k = 0..K/2 with the upper half a reflection,
  # H(k) = H(K-k).  The book defines kc = ceil(K wc/ws) and that CEILING
  # matters: rounding down puts the realized cutoff below the request.
  Kv <- as.integer(K)
  if (Kv < 2L) stop("the DFT length K must be at least 2")
  n <- as.integer(N)
  if (n < 1L) stop("the order N must be at least 1")
  if (is.null(kc) == is.null(fc)) {
    stop(
      "give either the cutoff index kc or a cutoff fc with fs, not ",
      "both and not neither"
    )
  }
  if (!is.null(fc)) {
    if (is.null(fs)) stop("fc needs the sampling rate fs")
    fsv <- as.numeric(fs)
    fcv <- as.numeric(fc)
    if (!(fcv > 0 && fcv < fsv / 2)) {
      stop(
        "the cutoff must lie strictly between 0 and the Nyquist ",
        "frequency"
      )
    }
    kcv <- as.integer(ceiling(Kv * fcv / fsv))
  } else {
    kcv <- as.integer(kc)
  }
  if (kcv < 1L) stop("the cutoff index must be at least 1")
  half <- Kv %/% 2L
  sq <- 1 / (1 + ((0:half) / kcv)^(2 * n))
  full <- c(sq, sq[Kv - ((half + 1L):(Kv - 1L)) + 1L])
  list(
    squared_magnitude = full, magnitude = sqrt(full),
    half_spectrum = sq, K = Kv, kc = kcv, N = n, dc_gain = 1,
    reflected = TRUE, cutoff_index_uses_a_ceiling = TRUE,
    method = "Rangayyan (2024) eq. (3.146)"
  )
}

BwHpDft <- function(K, kc = NULL, N = 2, fc = NULL, fs = NULL) {
  # eq (3.149): the lowpass with the ratio inverted.  At k = 0 the ratio
  # is unbounded and the response is exactly nought -- the filter the book
  # uses to strip baseline drift, eighth order at 2 Hz.  It leaves
  # high-frequency noise untouched; a highpass is not a denoiser.
  Kv <- as.integer(K)
  if (Kv < 2L) stop("the DFT length K must be at least 2")
  n <- as.integer(N)
  if (n < 1L) stop("the order N must be at least 1")
  if (is.null(kc) == is.null(fc)) {
    stop(
      "give either the cutoff index kc or a cutoff fc with fs, not ",
      "both and not neither"
    )
  }
  if (!is.null(fc)) {
    if (is.null(fs)) stop("fc needs the sampling rate fs")
    fsv <- as.numeric(fs)
    fcv <- as.numeric(fc)
    if (!(fcv > 0 && fcv < fsv / 2)) {
      stop(
        "the cutoff must lie strictly between 0 and the Nyquist ",
        "frequency"
      )
    }
    kcv <- as.integer(ceiling(Kv * fcv / fsv))
  } else {
    kcv <- as.integer(kc)
  }
  if (kcv < 1L) stop("the cutoff index must be at least 1")
  half <- Kv %/% 2L
  sq <- c(0, 1 / (1 + (kcv / seq_len(half))^(2 * n)))
  full <- c(sq, sq[Kv - ((half + 1L):(Kv - 1L)) + 1L])
  list(
    squared_magnitude = full, magnitude = sqrt(full),
    half_spectrum = sq, K = Kv, kc = kcv, N = n, dc_gain = 0,
    reflected = TRUE, leaves_high_frequency_noise_untouched = TRUE,
    method = "Rangayyan (2024) eq. (3.149)"
  )
}

# ------------------------------------------- notch, comb, sinc and windows

Notch60 <- function(fs, f0 = 60, z = NULL) {
  # A conjugate pair of zeros AT the interference frequency, so the gain
  # there is exactly nought.  With zeros alone the notch is wide, which is
  # why the book goes on to add poles just inside them.
  fsv <- as.numeric(fs)
  if (fsv <= 0) stop("fs must be positive")
  f0v <- as.numeric(f0)
  if (!(f0v > 0 && f0v < fsv / 2)) {
    stop(
      "the notch frequency must lie strictly between 0 and the ",
      "Nyquist frequency"
    )
  }
  w0 <- 2 * pi * f0v / fsv
  b <- c(1, -2 * cos(w0), 1)
  dc <- .morie_fsum(b)
  if (abs(dc) <= 1e-300) {
    stop("the notch sits at DC; the gain cannot be normalized there")
  }
  G <- 1 / dc
  b <- G * b
  Hz <- NULL
  if (!is.null(z)) {
    Hz <- .morie_rg_polyz(b, z)
    if (length(Hz) == 1L) Hz <- Hz[[1]]
  }
  zc <- complex(real = cos(w0), imaginary = sin(w0))
  list(
    b = b, a = 1, gain = G, zeros = c(zc, Conj(zc)), H = Hz,
    f0 = f0v, fs = fsv, omega_0 = w0,
    gain_at_the_notch = Mod(.morie_rg_polyz(b, zc)),
    dc_gain = 1, fir = TRUE, linear_phase = TRUE,
    notch_is_wide_without_poles = TRUE,
    method = "Rangayyan (2024) Section 3.7 (notch filter with two zeros)"
  )
}

Notch <- function(notch_freq, bandwidth = NULL, fs = 1000, r = NULL,
                  z = NULL) {
  # Zeros ON the unit circle at the interference frequency and poles just
  # inside at the same angle.  The poles are what make the notch NARROW:
  # without them the zeros pull the response down over a wide band, taking
  # signal with them.  At r = 1 the poles cancel the zeros entirely.
  fsv <- as.numeric(fs)
  if (fsv <= 0) stop("fs must be positive")
  f0 <- as.numeric(notch_freq)
  if (!(f0 > 0 && f0 < fsv / 2)) {
    stop(
      "the notch frequency must lie strictly between 0 and the ",
      "Nyquist frequency"
    )
  }
  if (is.null(bandwidth) == is.null(r)) {
    stop(
      "give either the bandwidth or the pole radius r, not both and ",
      "not neither"
    )
  }
  if (!is.null(bandwidth)) {
    bw <- as.numeric(bandwidth)
    if (bw <= 0) stop("the bandwidth must be positive")
    rv <- 1 - pi * bw / fsv
    if (rv <= 0) {
      stop(
        "that bandwidth needs a pole radius <= 0; ask for a narrower ",
        "notch"
      )
    }
  } else {
    rv <- as.numeric(r)
    bw <- (1 - rv) * fsv / pi
  }
  if (!(rv > 0 && rv < 1)) {
    stop(
      "the pole radius must satisfy 0 < r < 1; at r = 1 the poles ",
      "cancel the zeros"
    )
  }
  w0 <- 2 * pi * f0 / fsv
  bb <- c(1, -2 * cos(w0), 1)
  aa <- c(1, -2 * rv * cos(w0), rv * rv)
  G <- .morie_fsum(aa) / .morie_fsum(bb)
  bb <- G * bb
  Hz <- NULL
  if (!is.null(z)) {
    dd <- .morie_rg_polyz(aa, z)
    if (any(Mod(dd) <= 1e-300)) stop("z is a pole of H(z)")
    Hz <- .morie_rg_polyz(bb, z) / dd
    if (length(Hz) == 1L) Hz <- Hz[[1]]
  }
  zc <- complex(real = cos(w0), imaginary = sin(w0))
  list(
    b = bb, a = aa, gain = G, H = Hz, f0 = f0, fs = fsv, r = rv,
    bandwidth_hz = bw, omega_0 = w0,
    zeros = c(zc, Conj(zc)), poles = c(rv * zc, rv * Conj(zc)),
    gain_at_the_notch = Mod(.morie_rg_polyz(bb, zc)) /
      Mod(.morie_rg_polyz(aa, zc)),
    dc_gain = 1, iir = TRUE, poles_narrow_the_notch = TRUE,
    method = "Rangayyan (2024) Section 3.7 (notch filter with poles)"
  )
}

Comb <- function(period_samples, fs = 1000, z = NULL) {
  # H(z) = (1/2)(1 - z^-N): N zeros spaced evenly round the unit circle,
  # so it notches DC and every harmonic of fs/N at once -- which is what
  # powerline interference is.  The zero at DC is not optional, so a comb
  # removes the mean along with the interference.
  N <- as.integer(period_samples)
  if (N < 1L) stop("the period must be at least one sample")
  fsv <- as.numeric(fs)
  if (fsv <= 0) stop("fs must be positive")
  b <- numeric(N + 1L)
  b[1] <- 0.5
  b[N + 1L] <- -0.5
  Hz <- NULL
  if (!is.null(z)) {
    Hz <- .morie_rg_polyz(b, z)
    if (length(Hz) == 1L) Hz <- Hz[[1]]
  }
  list(
    b = b, a = 1, H = Hz, period_samples = N, fs = fsv,
    notch_frequencies_hz = (0:(N %/% 2L)) * fsv / N, n_zeros = N,
    notch_spacing_hz = fsv / N, dc_gain = 0,
    removes_dc_as_well = TRUE, fir = TRUE, linear_phase = TRUE,
    method = "Rangayyan (2024) Section 3.7 (comb filter)"
  )
}

FreqResp <- function(b, a = NULL, fs = 1000, n_freqs = 512) {
  # Evaluated on a uniform grid from DC to NYQUIST inclusive -- the
  # one-sided response, since for real coefficients the other half is the
  # conjugate mirror.  `a` follows the eq (3.67) convention with a_0 = 1
  # included, the form BwLp and BwHp return.
  bs <- as.numeric(b)
  if (!length(bs)) stop("need at least one numerator coefficient")
  az <- if (is.null(a)) 1 else as.numeric(a)
  if (!length(az)) stop("the denominator needs at least one coefficient")
  if (abs(az[1]) <= 1e-300) stop("a_0 must not be zero")
  fsv <- as.numeric(fs)
  if (fsv <= 0) stop("fs must be positive")
  m <- as.integer(n_freqs)
  if (m < 2L) stop("need at least two frequency points")
  f <- 0.5 * fsv * (seq_len(m) - 1L) / (m - 1L)
  w <- 2 * pi * f / fsv
  num <- vapply(
    w, function(v) {
      sum(bs * complex(
        real = cos(-v * (seq_along(bs) - 1L)),
        imaginary = sin(-v * (seq_along(bs) - 1L))
      ))
    },
    complex(1)
  )
  den <- vapply(
    w, function(v) {
      sum(az * complex(
        real = cos(-v * (seq_along(az) - 1L)),
        imaginary = sin(-v * (seq_along(az) - 1L))
      ))
    },
    complex(1)
  )
  if (any(Mod(den) <= 1e-300)) {
    stop(
      "the denominator vanishes; the filter has a pole on the unit ",
      "circle"
    )
  }
  H <- num / den
  mag <- Mod(H)
  list(
    f = f, H = H, magnitude = mag,
    magnitude_db = ifelse(mag > 0, 20 * log10(mag), -Inf),
    phase = Arg(H), fs = fsv, n_freqs = m, one_sided = TRUE,
    includes_nyquist = TRUE,
    method = "Rangayyan (2024) Section 3.5 (frequency response)"
  )
}

PhaseResp <- function(b, a = NULL, fs = 1000, n_freqs = 512,
                      unwrap = TRUE) {
  # The principal value jumps by 2 pi at the branch cut, an artifact of
  # the arctangent and not of the filter, so it is unwrapped by default.
  # Where H VANISHES the phase is undefined -- atan2(0,0) returns 0, which
  # is not a phase -- and those points are marked and skipped, or the
  # bogus value is carried into every later sample.
  r <- FreqResp(b, a = a, fs = fs, n_freqs = n_freqs)
  wrapped <- r$phase
  mag <- r$magnitude
  scale <- max(mag)
  defined <- if (scale > 0) mag > 1e-9 * scale else rep(FALSE, length(mag))
  unw <- numeric(length(wrapped))
  last <- NA_integer_
  for (i in seq_along(wrapped)) {
    if (!defined[i]) {
      unw[i] <- if (i > 1L) unw[i - 1L] else wrapped[i]
      next
    }
    if (is.na(last)) {
      unw[i] <- wrapped[i]
    } else {
      d <- wrapped[i] - wrapped[last]
      while (d > pi) d <- d - 2 * pi
      while (d < -pi) d <- d + 2 * pi
      unw[i] <- unw[last] + d
    }
    last <- i
  }
  list(
    f = r$f, phase = if (unwrap) unw else wrapped, wrapped = wrapped,
    unwrapped = unw, unwrap = isTRUE(unwrap), fs = r$fs,
    defined = defined, n_undefined = sum(!defined),
    phase_undefined_where_the_response_vanishes = TRUE,
    wrapping_is_an_arctangent_artifact = TRUE,
    method = "Rangayyan (2024) Section 3.5 (phase response)"
  )
}

GrpDelay <- function(b, a = NULL, fs = 1000, n_freqs = 512) {
  # Computed from the COEFFICIENTS, not by differentiating a numerical
  # phase.  Differentiating is wrong at any zero on the unit circle, where
  # the phase steps by PI -- a real step, not a branch artifact, so
  # unwrapping (which removes multiples of 2 pi) leaves it and the
  # derivative spikes.  A three-point mean has such a zero at w = 2pi/3
  # and its phase-differentiated delay comes out near 0 instead of 1.
  bs <- as.numeric(b)
  if (!length(bs)) stop("need at least one numerator coefficient")
  az <- if (is.null(a)) 1 else as.numeric(a)
  if (!length(az)) stop("the denominator needs at least one coefficient")
  fsv <- as.numeric(fs)
  if (fsv <= 0) stop("fs must be positive")
  m <- as.integer(n_freqs)
  if (m < 2L) stop("need at least two frequency points")
  f <- 0.5 * fsv * (seq_len(m) - 1L) / (m - 1L)
  w <- 2 * pi * f / fsv
  ratio <- function(coefs, v) {
    k <- seq_along(coefs) - 1L
    e <- complex(real = cos(-v * k), imaginary = sin(-v * k))
    c(sum(k * coefs * e), sum(coefs * e))
  }
  tau <- numeric(m)
  defined <- logical(m)
  for (i in seq_len(m)) {
    bb <- ratio(bs, w[i])
    aa <- ratio(az, w[i])
    if (Mod(bb[2]) <= 1e-12 || Mod(aa[2]) <= 1e-12) {
      tau[i] <- NA_real_
      defined[i] <- FALSE
    } else {
      tau[i] <- Re(bb[1] / bb[2]) - Re(aa[1] / aa[2])
      defined[i] <- TRUE
    }
  }
  good <- tau[defined]
  if (!length(good)) {
    stop(
      "the response vanishes at every frequency evaluated; the group ",
      "delay is undefined"
    )
  }
  mu <- .morie_fsum(good) / length(good)
  spread <- max(abs(good - mu))
  list(
    f = f, group_delay = tau, fs = fsv, mean = mu,
    max_deviation = spread,
    approximately_constant = spread <= 1e-9 * max(1, abs(mu)),
    defined = defined, n_undefined = sum(!defined),
    from_the_coefficients = TRUE,
    phase_differentiation_breaks_at_unit_circle_zeros = TRUE,
    method = "Rangayyan (2024) Section 3.5 (group delay)"
  )
}

BwLp <- function(cutoff_hz, order = 4, fs = 1000, z = NULL) {
  # The book's route end to end: prewarp by eq (3.141), place poles by
  # eq (3.137), keep the left-half-plane ones by eq (3.138), apply the
  # bilinear transform of eq (3.139) to reach eq (3.143).  Prewarping is
  # done here; without it the realized cutoff sits below the request.
  fsv <- as.numeric(fs)
  fcv <- as.numeric(cutoff_hz)
  if (fsv <= 0) stop("fs must be positive")
  if (!(fcv > 0 && fcv < fsv / 2)) {
    stop(
      "the cutoff must lie strictly between 0 and the Nyquist ",
      "frequency ", fsv / 2, " Hz"
    )
  }
  r <- BwDigital(N = as.integer(order), fc = fcv, fs = fsv, z = z)
  r$cutoff_hz <- fcv
  r$fs <- fsv
  r$order <- as.integer(order)
  r$prewarped <- TRUE
  r$kind <- "lowpass"
  r$method <- paste(
    "Rangayyan (2024) eqs. (3.135)-(3.143); Butterworth",
    "lowpass via the bilinear transform"
  )
  r
}

BwHp <- function(cutoff_hz, order = 4, fs = 1000, z = NULL) {
  # The lowpass poles are reused -- a Butterworth highpass has the same
  # pole radius -- and the N zeros move from z = -1 to z = +1.  The gain
  # is renormalized at NYQUIST, since a highpass has no DC gain to
  # normalize against and dividing there would be division by zero.
  fsv <- as.numeric(fs)
  fcv <- as.numeric(cutoff_hz)
  if (fsv <= 0) stop("fs must be positive")
  if (!(fcv > 0 && fcv < fsv / 2)) {
    stop(
      "the cutoff must lie strictly between 0 and the Nyquist ",
      "frequency ", fsv / 2, " Hz"
    )
  }
  n <- as.integer(order)
  a <- BwDigital(N = n, fc = fcv, fs = fsv)$a
  b <- rev(Re(.morie_rg_polyroots(rep(1, n))))
  sgn <- (-1)^(seq_along(b) - 1L)
  nyq_num <- .morie_fsum(b * sgn)
  nyq_den <- .morie_fsum(a * (-1)^(seq_along(a) - 1L))
  if (abs(nyq_num) <= 1e-300) {
    stop("the numerator vanishes at Nyquist")
  }
  G <- nyq_den / nyq_num
  b <- G * b
  Hz <- NULL
  if (!is.null(z)) {
    dd <- .morie_rg_polyz(a, z)
    if (any(Mod(dd) <= 1e-300)) stop("z is a pole of H(z)")
    Hz <- .morie_rg_polyz(b, z) / dd
    if (length(Hz) == 1L) Hz <- Hz[[1]]
  }
  list(
    b = b, a = a, gain = G, H = Hz, N = n, cutoff_hz = fcv, fs = fsv,
    order = n, kind = "highpass", zeros_at_plus_one = n,
    dc_gain = 0, nyquist_gain = 1, prewarped = TRUE,
    normalized_at_nyquist = TRUE,
    method = "Rangayyan (2024) Section 3.7; Butterworth highpass"
  )
}

HammingW <- function(N) {
  # w(n) = 0.54 - 0.46 cos(2 pi n/(N-1)).  The 0.54/0.46 split cancels the
  # rectangle's largest sidelobe, about -43 dB, at the cost of a wider
  # main lobe than the Hann.  It does NOT reach zero at the ends --
  # w(0) = 0.08 -- which matters when windows are overlapped and added.
  n <- as.integer(N)
  if (n < 1L) stop("N must be at least 1")
  if (n == 1L) {
    return(list(
      w = 1, N = 1L, sum = 1, endpoints = c(1, 1),
      reaches_zero_at_the_ends = FALSE,
      method = "Rangayyan (2024) Section 3.4 (Hamming window)"
    ))
  }
  i <- seq_len(n) - 1L
  w <- 0.54 - 0.46 * cos(2 * pi * i / (n - 1L))
  list(
    w = w, N = n, sum = .morie_fsum(w), endpoints = c(w[1], w[n]),
    reaches_zero_at_the_ends = FALSE,
    coherent_gain = .morie_fsum(w) / n,
    symmetric = all(abs(w - rev(w)) < 1e-12),
    method = "Rangayyan (2024) Section 3.4 (Hamming window)"
  )
}

HannW <- function(N) {
  # w(n) = 0.5[1 - cos(2 pi n/(N-1))].  Reaches exactly zero at both ends,
  # so overlapped Hann windows add to a constant at 50 per cent overlap --
  # the property that makes it the default for overlap-add analysis.
  # Not to be confused with the Hann FILTER of eq (3.100), a three-tap
  # 1:2:1 smoother; this is a taper applied to a data segment.
  n <- as.integer(N)
  if (n < 1L) stop("N must be at least 1")
  if (n == 1L) {
    return(list(
      w = 1, N = 1L, sum = 1, endpoints = c(1, 1),
      reaches_zero_at_the_ends = FALSE,
      method = "Rangayyan (2024) Section 3.4 (Hann window)"
    ))
  }
  i <- seq_len(n) - 1L
  w <- 0.5 * (1 - cos(2 * pi * i / (n - 1L)))
  list(
    w = w, N = n, sum = .morie_fsum(w), endpoints = c(w[1], w[n]),
    reaches_zero_at_the_ends = TRUE,
    coherent_gain = .morie_fsum(w) / n,
    not_the_hann_filter_of_eq_3_100 = TRUE,
    symmetric = all(abs(w - rev(w)) < 1e-12),
    method = "Rangayyan (2024) Section 3.4 (Hann window)"
  )
}

BlackmanW <- function(N) {
  # w(n) = 0.42 - 0.5 cos(2 pi n/(N-1)) + 0.08 cos(4 pi n/(N-1)).  A third
  # cosine buys much deeper sidelobes than the Hamming, about -58 dB, at
  # the cost of a main lobe half again as wide.  Resolution against
  # leakage: no window escapes that trade.
  n <- as.integer(N)
  if (n < 1L) stop("N must be at least 1")
  if (n == 1L) {
    return(list(
      w = 1, N = 1L, sum = 1, endpoints = c(1, 1),
      method = "Rangayyan (2024) Section 3.4 (Blackman window)"
    ))
  }
  i <- seq_len(n) - 1L
  w <- 0.42 - 0.5 * cos(2 * pi * i / (n - 1L)) +
    0.08 * cos(4 * pi * i / (n - 1L))
  list(
    w = w, N = n, sum = .morie_fsum(w), endpoints = c(w[1], w[n]),
    coherent_gain = .morie_fsum(w) / n,
    widest_main_lobe_of_the_three = TRUE,
    resolution_traded_for_leakage = TRUE,
    symmetric = all(abs(w - rev(w)) < 1e-12),
    method = "Rangayyan (2024) Section 3.4 (Blackman window)"
  )
}

WindowFn <- function(N, window_type = "hamming") {
  # Section 3.4.  Truncating a record IS multiplying it by a rectangle,
  # whose transform has sidelobes that leak energy from strong components
  # into neighbouring bins; a tapered window trades a wider main lobe for
  # lower sidelobes.  The rectangular window is included because it is the
  # default -- doing nothing is choosing it -- and naming it makes that
  # choice explicit.
  n <- as.integer(N)
  if (n < 1L) stop("N must be at least 1")
  kinds <- c("rectangular", "hann", "hamming", "blackman")
  if (!window_type %in% kinds) {
    stop("window_type must be one of ", paste(kinds, collapse = ", "))
  }
  r <- switch(window_type,
    rectangular = list(
      w = rep(1, n), N = n, sum = as.numeric(n),
      endpoints = c(1, 1), coherent_gain = 1,
      symmetric = TRUE
    ),
    hann = HannW(n),
    hamming = HammingW(n),
    blackman = BlackmanW(n)
  )
  r$window_type <- window_type
  r$doing_nothing_is_the_rectangular_window <- TRUE
  r$method <- "Rangayyan (2024) Section 3.4 (window functions)"
  r
}

SincKern <- function(fc, fs = 1000, M = 64, window = NULL) {
  # The inverse transform of a rectangular passband, truncated to M+1 taps
  # and delayed by M/2 to make it causal.  Truncation is multiplying by a
  # rectangle, whose sidelobes decay slowly, so the realized stopband
  # ripples: Gibbs' phenomenon, and it does not improve with M -- only the
  # ripples narrow, they do not shrink.  A window fixes it.
  fsv <- as.numeric(fs)
  if (fsv <= 0) stop("fs must be positive")
  fcv <- as.numeric(fc)
  if (!(fcv > 0 && fcv < fsv / 2)) {
    stop(
      "the cutoff must lie strictly between 0 and the Nyquist ",
      "frequency"
    )
  }
  m <- as.integer(M)
  if (m < 1L) stop("M must be at least 1")
  ratio <- 2 * fcv / fsv
  t <- (0:m) - m / 2
  h <- ifelse(abs(t) <= 1e-12, ratio,
    ratio * sin(pi * ratio * t) / (pi * ratio * t)
  )
  win <- NULL
  if (!is.null(window)) {
    win <- WindowFn(m + 1L, window)$w
    h <- h * win
  }
  total <- .morie_fsum(h)
  if (abs(total) > 1e-300) h <- h / total
  list(
    h = h, n_taps = m + 1L, fc = fcv, fs = fsv, M = m,
    window = window, window_values = win, delay_samples = m / 2,
    dc_gain = 1, truncation_causes_gibbs_ripple = is.null(window),
    ripple_height_does_not_shrink_with_M = TRUE,
    method = "Rangayyan (2024) Section 3.4 (windowed sinc)"
  )
}

MfiltH <- function(g, normalize = FALSE) {
  # h(n) = g(N-1-n): the template reversed in time, which makes the
  # filter's output the cross-correlation with the template.  Reversal is
  # the whole content -- convolving with the unreversed template
  # correlates with a mirrored pattern and peaks in the wrong place.
  gs <- as.numeric(g)
  if (!length(gs)) stop("the template needs at least one sample")
  h <- rev(gs)
  energy <- .morie_fsum(gs * gs)
  if (normalize) {
    if (energy <= 0) {
      stop("a template with no energy cannot be normalized")
    }
    h <- h / sqrt(energy)
  }
  list(
    h = h, template = gs, n = length(h), energy = energy,
    normalized = isTRUE(normalize), peak_index = length(gs) - 1L,
    time_reversed = TRUE, output_is_the_cross_correlation = TRUE,
    method = "Rangayyan (2024) Ch. 4 (matched filter)"
  )
}

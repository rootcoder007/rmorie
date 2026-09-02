# Rangayyan homomorphic filtering and the cepstra, Chapter 4.7.  Mirror
# of the Python bsacep module.
#
# Three defects the Python arm's tests exposed are avoided here too:
# the DFT phase must be unwrapped over the HALF circle and mirrored (a
# straight-through unwrap forces a ramp that buries the cepstrum in a
# spurious 1/n tail); the reconstruction from split cepstra is CIRCULAR,
# not linear; and the low/high lifters must partition the quefrency axis
# (|q| <= k and |q| > k), or q = 0 and q = k are counted twice.

#' .morie_rg_dft
#'
#' A step of the rangayyan_cep implementation. Called by \code{CardioResp}, \code{CCepstrum}, \code{Cepstrum} and 11 others in the module.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param x A vector; its length is taken.
#' @return A list with \code{re}, \code{im}.
#' @export
.morie_rg_dft <- function(x) {
  n <- length(x)
  idx <- seq_len(n) - 1L
  re <- vapply(
    idx, function(k) .morie_fsum(x * cos(-2 * pi * idx * k / n)),
    numeric(1)
  )
  im <- vapply(
    idx, function(k) .morie_fsum(x * sin(-2 * pi * idx * k / n)),
    numeric(1)
  )
  list(re = re, im = im)
}

#' .morie_rg_idft_re
#'
#' A step of the rangayyan_cep implementation. Called by \code{CCepstrum}, \code{Cepstrum}, \code{HomDeconv} and 4 others in the module.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param re A vector; its length is taken.
#' @param im Numeric; combined arithmetically in the body.
#' @return A vector, from \code{vapply}.
#' @export
.morie_rg_idft_re <- function(re, im) {
  n <- length(re)
  idx <- seq_len(n) - 1L
  vapply(idx, function(i) {
    ang <- 2 * pi * i * idx / n
    .morie_fsum(re * cos(ang) - im * sin(ang)) / n
  }, numeric(1))
}

#' .morie_rg_unwrap
#'
#' A step of the rangayyan_cep implementation. Called by \code{CCepstrum}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param phase A vector; its length is taken and its elements indexed.
#' @return The value of \code{out}, as built in the body.
#' @export
.morie_rg_unwrap <- function(phase) {
  out <- numeric(length(phase))
  if (!length(phase)) {
    return(out)
  }
  out[1] <- phase[1]
  off <- 0
  for (i in seq_along(phase)[-1]) {
    d <- phase[i] - phase[i - 1L]
    while (d > pi) {
      off <- off - 2 * pi
      d <- d - 2 * pi
    }
    while (d < -pi) {
      off <- off + 2 * pi
      d <- d + 2 * pi
    }
    out[i] <- phase[i] + off
  }
  out
}

#' Real cepstrum: c(n) = IDFT(log|DFT(x)|).  Keeps only the magnitude,
#'
#' so it discards the phase and is NOT invertible -- the whole
#' difference from the complex cepstrum of eq (4.64).  It still shows
#' the echo impulses of eq (4.80), which is what it is used for.
#'
#' @param x Coerced to numeric by the body, with \code{as.numeric}.
#' @return A list with \code{cepstrum}, \code{log_magnitude}, \code{n}, \code{zero_bins}, \code{invertible}, \code{method}.
#' @export
Cepstrum <- function(x) {
  # real cepstrum: c(n) = IDFT(log|DFT(x)|).  Keeps only the magnitude,
  # so it discards the phase and is NOT invertible -- the whole
  # difference from the complex cepstrum of eq (4.64).  It still shows
  # the echo impulses of eq (4.80), which is what it is used for.
  xs <- as.numeric(x)
  n <- length(xs)
  if (n < 2L) stop("need at least two samples")
  f <- .morie_rg_dft(xs)
  mags <- sqrt(f$re^2 + f$im^2)
  floor_ <- 1e-300
  logmag <- log(pmax(mags, floor_))
  list(
    cepstrum = .morie_rg_idft_re(logmag, numeric(n)),
    log_magnitude = logmag, n = n, zero_bins = sum(mags <= floor_),
    invertible = FALSE,
    method = "real cepstrum; contrast Rangayyan (2024) eq. (4.64)"
  )
}

#' Eqs (4.63)-(4.64): the inverse transform of log|X| + j angle(X)
#'
#' The phase is unwrapped over k = 0..N/2 and mirrored by odd symmetry;
#' the linear term (the z^r delay of eq 4.68) is r =
#' round(phase(pi)/pi).
#'
#' @param x Coerced to numeric by the body, with \code{as.numeric}.
#' @return A list with \code{cepstrum}, \code{log_magnitude}, \code{phase}, \code{detrended_phase}, \code{linear_phase_removed}, \code{delay_removed}, \code{n}, \code{method}.
#' @export
CCepstrum <- function(x) {
  # eqs (4.63)-(4.64): the inverse transform of log|X| + j angle(X).
  # The phase is unwrapped over k = 0..N/2 and mirrored by odd symmetry;
  # the linear term (the z^r delay of eq 4.68) is r = round(phase(pi)/pi).
  xs <- as.numeric(x)
  n <- length(xs)
  if (n < 4L) stop("need at least four samples")
  f <- .morie_rg_dft(xs)
  mags <- sqrt(f$re^2 + f$im^2)
  floor_ <- 1e-300
  if (any(mags <= floor_)) {
    stop(sprintf(
      "the complex log needs a nonzero spectrum at every bin; %d bins vanish",
      sum(mags <= floor_)
    ))
  }
  half <- n %/% 2L
  raw <- atan2(f$im, f$re)
  up <- .morie_rg_unwrap(raw[seq_len(half + 1L)])
  r_int <- if (half > 0L) as.integer(round(up[half + 1L] / pi)) else 0L
  slope <- if (half > 0L) r_int * pi / half else 0
  up <- up - slope * (seq_len(half + 1L) - 1L)
  detr <- c(up, -up[(n - (half + 1L):(n - 1L)) + 1L])
  list(
    cepstrum = .morie_rg_idft_re(log(mags), detr),
    log_magnitude = log(mags), phase = detr, detrended_phase = detr,
    linear_phase_removed = slope, delay_removed = r_int, n = n,
    method = "Rangayyan (2024) eqs. (4.63)-(4.64)"
  )
}

#' Eqs (4.63)-(4.64) with the unwrapping diagnostics the book calls "an
#'
#' important consideration": a 2-pi jump at nearly every bin means the
#' spectrum is too coarsely sampled to track the phase.
#'
#' @param x Passed to \code{CCepstrum}.
#' @return The value of \code{r}, as built in the body.
#' @export
CCepX <- function(x) {
  # eqs (4.63)-(4.64) with the unwrapping diagnostics the book calls "an
  # important consideration": a 2-pi jump at nearly every bin means the
  # spectrum is too coarsely sampled to track the phase.
  r <- CCepstrum(x)
  wrapped <- atan2(sin(r$phase), cos(r$phase))
  jumps <- sum(abs(diff(wrapped)) > pi)
  r$wrapped_phase <- wrapped
  r$phase_jumps <- jumps
  r$well_conditioned <- jumps < length(wrapped) %/% 4L
  r$method <- paste(
    "Rangayyan (2024) eqs. (4.63)-(4.64), with the",
    "phase-unwrapping diagnostics"
  )
  r
}

#' Eq (4.58): y(t) = x(t) p(t), the model a multiplicative homomorphic
#'
#' system addresses.
#'
#' @param x Coerced to numeric by the body, with \code{as.numeric}.
#' @param p Coerced to numeric by the body, with \code{as.numeric}.
#' @return A list with \code{y}, \code{x}, \code{p}, \code{n}, \code{separable_by_log}, \code{method}.
#' @export
MultModel <- function(x, p) {
  # eq (4.58): y(t) = x(t) p(t), the model a multiplicative homomorphic
  # system addresses.
  xs <- as.numeric(x)
  ps <- as.numeric(p)
  if (length(xs) != length(ps)) stop("x and p must have the same length")
  if (!length(xs)) stop("need at least one sample")
  list(
    y = xs * ps, x = xs, p = ps, n = length(xs),
    separable_by_log = all(xs != 0) && all(ps != 0),
    method = "Rangayyan (2024) eq. (4.58)"
  )
}

#' Eq (4.59): log[y] = log[x] + log[p], for x != 0 and p != 0.  The
#'
#' book states that side condition, so a zero is rejected rather than
#' giving -Inf; a negative factor needs the complex-log route.
#'
#' @param x Coerced to numeric by the body, with \code{as.numeric}.
#' @param p Coerced to numeric by the body, with \code{as.numeric}.
#' @return A list with \code{log_y}, \code{log_x}, \code{log_p}, \code{sum}, \code{max_difference}, \code{additive}, \code{method}.
#' @export
LogSep <- function(x, p) {
  # eq (4.59): log[y] = log[x] + log[p], for x != 0 and p != 0.  The
  # book states that side condition, so a zero is rejected rather than
  # giving -Inf; a negative factor needs the complex-log route.
  xs <- as.numeric(x)
  ps <- as.numeric(p)
  if (length(xs) != length(ps)) stop("x and p must have the same length")
  if (!length(xs)) stop("need at least one sample")
  if (any(xs <= 0) || any(ps <= 0)) {
    stop(
      "eq. (4.59) needs x(t) != 0 and p(t) != 0; the real logarithm ",
      "also needs them positive"
    )
  }
  lhs <- log(xs * ps)
  rhs <- log(xs) + log(ps)
  gap <- max(abs(lhs - rhs))
  list(
    log_y = lhs, log_x = log(xs), log_p = log(ps), sum = rhs,
    max_difference = gap, additive = gap <= 1e-12 * (1 + max(abs(lhs))),
    method = "Rangayyan (2024) eq. (4.59)"
  )
}

#' Eq (4.61): y(t) = x(t) * h(t), the model homomorphic DEconvolution
#'
#' addresses.
#'
#' @param x Coerced to numeric by the body, with \code{as.numeric}.
#' @param h Coerced to numeric by the body, with \code{as.numeric}.
#' @return A list with \code{y}, \code{n}, \code{n_x}, \code{n_h}, \code{method}.
#' @export
ConvModel <- function(x, h) {
  # eq (4.61): y(t) = x(t) * h(t), the model homomorphic DEconvolution
  # addresses.
  xs <- as.numeric(x)
  hs <- as.numeric(h)
  if (!length(xs) || !length(hs)) {
    stop("both signals need at least one sample")
  }
  y <- .morie_rg_conv(xs, hs)
  list(
    y = y, n = length(y), n_x = length(xs), n_h = length(hs),
    method = "Rangayyan (2024) eq. (4.61)"
  )
}

#' Eq (4.66): y_hat = x_hat + h_hat.  The residual is not exactly zero
#'
#' because the cepstrum is of infinite duration (eq 4.73) and the DFT
#' truncates it; the size of the residual is the useful number.
#'
#' @param x Coerced to numeric by the body, with \code{as.numeric}.
#' @param h Coerced to numeric by the body, with \code{as.numeric}.
#' @return A list with \code{y}, \code{cepstrum_y}, \code{cepstrum_x}, \code{cepstrum_h}, \code{residual}, \code{max_residual}, \code{relative_residual}, \code{truncation_note}, \code{method}.
#' @export
CCepSum <- function(x, h) {
  # eq (4.66): y_hat = x_hat + h_hat.  The residual is not exactly zero
  # because the cepstrum is of infinite duration (eq 4.73) and the DFT
  # truncates it; the size of the residual is the useful number.
  xs <- as.numeric(x)
  hs <- as.numeric(h)
  if (!length(xs) || !length(hs)) {
    stop("both signals need at least one sample")
  }
  n <- length(xs) + length(hs) - 1L
  y <- .morie_rg_conv(xs, hs)
  cy <- CCepstrum(y)$cepstrum
  cx <- CCepstrum(c(xs, numeric(n - length(xs))))$cepstrum
  ch <- CCepstrum(c(hs, numeric(n - length(hs))))$cepstrum
  resid <- cy - cx - ch
  scale <- max(abs(cy))
  if (scale == 0) scale <- 1
  list(
    y = y, cepstrum_y = cy, cepstrum_x = cx, cepstrum_h = ch,
    residual = resid, max_residual = max(abs(resid)),
    relative_residual = max(abs(resid)) / scale,
    truncation_note = paste(
      "the complex cepstrum is of infinite",
      "duration (eq. 4.73), so a finite DFT",
      "leaves a residual"
    ),
    method = "Rangayyan (2024) eqs. (4.65)-(4.66)"
  )
}

#' RatZ
#'
#' A step of the rangayyan_cep implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param gain Coerced to complex by the body, with \code{as.complex}.
#' @param r Coerced to integer by the body, with \code{as.integer}.
#' @param zeros_in Coerced to complex by the body, with \code{as.complex}.
#' @param zeros_out Coerced to complex by the body, with \code{as.complex}.
#' @param poles_in Coerced to complex by the body, with \code{as.complex}.
#' @param poles_out Coerced to complex by the body, with \code{as.complex}.
#' @param z Optional; may be \code{NULL}. Coerced to complex by the body, with \code{as.complex}.
#' @return The value of \code{out}, as built in the body.
#' @export
RatZ <- function(gain, r, zeros_in, zeros_out, poles_in, poles_out,
                 z = NULL) {
  # The rational form whose complex log the book expands at eq (4.68).
  # The four-way split is not decoration: eq (4.72) gives a different
  # closed form for each group, and it decides whether the cepstrum is
  # causal, anticausal or two-sided.  Membership is checked, since an
  # "inside" root with modulus above 1 makes the series diverge.
  ai <- as.complex(zeros_in)
  bo <- as.complex(zeros_out)
  ci <- as.complex(poles_in)
  do_ <- as.complex(poles_out)
  for (nm in c("zeros_in", "poles_in")) {
    g <- if (nm == "zeros_in") ai else ci
    if (any(Mod(g) >= 1)) stop(nm, " must lie inside the unit circle")
  }
  for (nm in c("zeros_out", "poles_out")) {
    g <- if (nm == "zeros_out") bo else do_
    if (any(Mod(g) >= 1)) {
      stop(
        nm, " holds the RECIPROCAL of a root outside the unit circle, ",
        "so it must itself be inside"
      )
    }
  }
  out <- list(
    gain = as.complex(gain), r = as.integer(r), zeros_in = ai,
    zeros_out = bo, poles_in = ci, poles_out = do_,
    minimum_phase = !length(bo) && !length(do_),
    maximum_phase = !length(ai) && !length(ci),
    method = "Rangayyan (2024) the rational form expanded at eq. (4.68)"
  )
  if (is.null(z)) {
    out$X <- NULL
    return(out)
  }
  pts <- as.complex(z)
  if (any(pts == 0)) stop("z = 0 is a pole of the z^-1 factors")
  vals <- vapply(pts, function(zv) {
    num <- as.complex(gain) * zv^as.integer(r)
    for (ak in ai) num <- num * (1 - ak / zv)
    for (bk in bo) num <- num * (1 - bk * zv)
    den <- as.complex(1)
    for (ck in ci) den <- den * (1 - ck / zv)
    for (dk in do_) den <- den * (1 - dk * zv)
    if (den == 0) stop("z coincides with a pole of X")
    num / den
  }, complex(1))
  one <- length(pts) == 1L
  out$X <- if (one) vals[[1]] else vals
  out$z <- if (one) pts[[1]] else pts
  out
}

#' CCepClosed
#'
#' A step of the rangayyan_cep implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param gain Coerced to complex by the body, with \code{as.complex}.
#' @param zeros_in Coerced to complex by the body, with \code{as.complex}.
#' @param zeros_out Coerced to complex by the body, with \code{as.complex}.
#' @param poles_in Coerced to complex by the body, with \code{as.complex}.
#' @param poles_out Coerced to complex by the body, with \code{as.complex}.
#' @param nmax Coerced to integer by the body, with \code{as.integer}. Defaults to \code{32}.
#' @return A list with \code{cepstrum}, \code{quefrency}, \code{c0}, \code{positive}, \code{negative}, \code{causal}, \code{anticausal}, \code{infinite_duration}, \code{nmax}, \code{method}.
#' @export
CCepClosed <- function(gain, zeros_in, zeros_out, poles_in, poles_out,
                       nmax = 32) {
  # eq (4.72): x_hat(0) = log|A|; for n > 0, -sum a^n/n + sum c^n/n; for
  # n < 0, sum b^-n/n - sum d^-n/n.  The book's properties follow: a
  # minimum-phase signal has a CAUSAL cepstrum, a maximum-phase one an
  # anticausal cepstrum, and the cepstrum is of infinite duration even
  # for a finite signal -- so nmax is a truncation, not the whole thing.
  ai <- as.complex(zeros_in)
  bo <- as.complex(zeros_out)
  ci <- as.complex(poles_in)
  do_ <- as.complex(poles_out)
  k <- as.integer(nmax)
  if (k < 1L) stop("nmax must be positive")
  g <- Mod(as.complex(gain))
  if (g <= 0) stop("the gain must be nonzero")
  ns <- seq_len(k)
  pos <- vapply(ns, function(n) {
    sum(-(ai^n) / n) + sum((ci^n) / n)
  }, complex(1))
  neg <- vapply(ns, function(n) {
    sum((bo^n) / n) - sum((do_^n) / n)
  }, complex(1))
  list(
    cepstrum = c(rev(neg), as.complex(log(g)), pos),
    quefrency = (-k):k, c0 = log(g), positive = pos,
    negative = rev(neg),
    causal = !length(bo) && !length(do_),
    anticausal = !length(ai) && !length(ci),
    infinite_duration = TRUE, nmax = k,
    method = "Rangayyan (2024) eq. (4.72)"
  )
}

#' CCepDecay
#'
#' A step of the rangayyan_cep implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param zeros_in Coerced to complex by the body, with \code{as.complex}.
#' @param zeros_out Coerced to complex by the body, with \code{as.complex}.
#' @param poles_in Coerced to complex by the body, with \code{as.complex}.
#' @param poles_out Coerced to complex by the body, with \code{as.complex}.
#' @param nmax Coerced to integer by the body, with \code{as.integer}. Defaults to \code{32}.
#' @param constant Optional; may be \code{NULL}. Coerced to numeric by the body, with \code{as.numeric}.
#' @return A list with \code{alpha}, \code{K}, \code{bound}, \code{quefrency}, \code{decays_at_least_as_one_over_n}, \code{near_unit_circle}, \code{method}.
#' @export
CCepDecay <- function(zeros_in, zeros_out, poles_in, poles_out, nmax = 32,
                      constant = NULL) {
  # eq (4.73): |x_hat(n)| < K |alpha^n / n| with alpha the largest root
  # modulus.  The geometric factor is what makes liftering work; a root
  # close to the unit circle sends alpha towards 1, leaving only the 1/n
  # decay and a cepstrum a short lifter will truncate badly.
  roots <- c(
    as.complex(zeros_in), as.complex(zeros_out),
    as.complex(poles_in), as.complex(poles_out)
  )
  if (!length(roots)) stop("need at least one root")
  alpha <- max(Mod(roots))
  if (alpha <= 0) stop("all roots are at the origin; the bound is vacuous")
  k <- as.integer(nmax)
  if (k < 1L) stop("nmax must be positive")
  kk <- if (is.null(constant)) length(roots) else as.numeric(constant)
  ns <- seq_len(k)
  list(
    alpha = alpha, K = kk, bound = kk * alpha^ns / ns, quefrency = ns,
    decays_at_least_as_one_over_n = TRUE, near_unit_circle = alpha > 0.95,
    method = "Rangayyan (2024) eq. (4.73)"
  )
}

#' Eqs (4.79)-(4.80): the complex cepstrum of a wavelet plus one echo
#'
#' is the wavelet\'s cepstrum plus impulses at n0 and its multiples,
#' amplitudes (-1)^(k+1) a^k / k.  The expansion needs |a| < 1, which
#' the book states and which is enforced.
#'
#' @param a Coerced to numeric by the body, with \code{as.numeric}.
#' @param n0 Coerced to integer by the body, with \code{as.integer}.
#' @param terms Coerced to integer by the body, with \code{as.integer}. Defaults to \code{10}.
#' @param omega Optional; may be \code{NULL}. Coerced to numeric by the body, with \code{as.numeric}.
#' @return The value of \code{out}, as built in the body.
#' @export
EchoSeries <- function(a, n0, terms = 10, omega = NULL) {
  # eqs (4.79)-(4.80): the complex cepstrum of a wavelet plus one echo
  # is the wavelet's cepstrum plus impulses at n0 and its multiples,
  # amplitudes (-1)^(k+1) a^k / k.  The expansion needs |a| < 1, which
  # the book states and which is enforced.
  av <- as.numeric(a)
  if (!(abs(av) < 1)) stop(sprintf("eq. (4.79) needs |a| < 1; got %g", av))
  d <- as.integer(n0)
  if (d < 1L) stop("the echo delay must be at least one sample")
  k <- as.integer(terms)
  if (k < 1L) stop("terms must be positive")
  i <- seq_len(k)
  amps <- (-1)^(i + 1) * av^i / i
  lags <- i * d
  out <- list(
    amplitudes = amps, quefrencies = lags, a = av, n0 = d,
    terms = k, first_peak = d,
    method = "Rangayyan (2024) eqs. (4.79)-(4.80)"
  )
  if (!is.null(omega)) {
    ws <- as.numeric(omega)
    vals <- vapply(
      ws, function(w) {
        sum(amps * complex(real = cos(-w * lags), imaginary = sin(-w * lags)))
      },
      complex(1)
    )
    exact <- vapply(
      ws, function(w) {
        log(1 + av * complex(real = cos(-w * d), imaginary = sin(-w * d)))
      },
      complex(1)
    )
    one <- length(ws) == 1L
    out$series <- if (one) vals[[1]] else vals
    out$exact <- if (one) exact[[1]] else exact
    out$max_error <- max(Mod(vals - exact))
  }
  out
}

#' Eq (4.81).  The book notes the final squaring is omitted in some
#'
#' definitions, and that it matters: WITH the square, eq (4.82) holds
#' only when the cross-term is negligible; WITHOUT it, no cross-term
#' arises and eq (4.82) is exact.
#'
#' @param x Coerced to numeric by the body, with \code{as.numeric}.
#' @param square A flag; the body branches on it. Defaults to \code{TRUE}.
#' @return A list with \code{cepstrum}, \code{unsquared}, \code{log_power}, \code{n}, \code{squared}, \code{zero_bins}, \code{retains_phase}, \code{additivity_exact}, \code{method}.
#' @export
PCepstrum <- function(x, square = TRUE) {
  # eq (4.81).  The book notes the final squaring is omitted in some
  # definitions, and that it matters: WITH the square, eq (4.82) holds
  # only when the cross-term is negligible; WITHOUT it, no cross-term
  # arises and eq (4.82) is exact.
  xs <- as.numeric(x)
  n <- length(xs)
  if (n < 2L) stop("need at least two samples")
  f <- .morie_rg_dft(xs)
  p2 <- f$re^2 + f$im^2
  floor_ <- 1e-300
  logp <- log(pmax(p2, floor_))
  base <- .morie_rg_idft_re(logp, numeric(n))
  list(
    cepstrum = if (square) base^2 else base, unsquared = base,
    log_power = logp, n = n, squared = isTRUE(square),
    zero_bins = sum(p2 <= floor_), retains_phase = FALSE,
    additivity_exact = !isTRUE(square),
    method = "Rangayyan (2024) eq. (4.81)"
  )
}

#' Eq (4.82): the power cepstra of a convolution add, exactly when the
#'
#' squaring of eq (4.81) is omitted.  square defaults to FALSE here for
#' that reason; TRUE reproduces the book\'s definition and shows how
#' large the neglected cross-term is on the caller\'s own data.
#'
#' @param x Coerced to numeric by the body, with \code{as.numeric}.
#' @param h Coerced to numeric by the body, with \code{as.numeric}.
#' @param square A flag; the body branches on it. Defaults to \code{FALSE}.
#' @return A list with \code{y}, \code{cepstrum_y}, \code{cepstrum_x}, \code{cepstrum_h}, \code{residual}, \code{max_residual}, \code{relative_residual}, \code{squared}, \code{exact}, \code{method}.
#' @export
PCepSum <- function(x, h, square = FALSE) {
  # eq (4.82): the power cepstra of a convolution add, exactly when the
  # squaring of eq (4.81) is omitted.  square defaults to FALSE here for
  # that reason; TRUE reproduces the book's definition and shows how
  # large the neglected cross-term is on the caller's own data.
  xs <- as.numeric(x)
  hs <- as.numeric(h)
  if (!length(xs) || !length(hs)) {
    stop("both signals need at least one sample")
  }
  n <- length(xs) + length(hs) - 1L
  y <- .morie_rg_conv(xs, hs)
  cy <- PCepstrum(y, square = square)$cepstrum
  cx <- PCepstrum(c(xs, numeric(n - length(xs))), square = square)$cepstrum
  ch <- PCepstrum(c(hs, numeric(n - length(hs))), square = square)$cepstrum
  resid <- cy - cx - ch
  scale <- max(abs(cy))
  if (scale == 0) scale <- 1
  list(
    y = y, cepstrum_y = cy, cepstrum_x = cx, cepstrum_h = ch,
    residual = resid, max_residual = max(abs(resid)),
    relative_residual = max(abs(resid)) / scale,
    squared = isTRUE(square), exact = !isTRUE(square),
    method = "Rangayyan (2024) eq. (4.82)"
  )
}

#' Eq (4.83): y_hat_p(n) = \[y_hat(n) + y_hat(-n)\]^2 -- the squared even
#'
#' part of the complex cepstrum.  The odd part, where the phase lives,
#' is annihilated by the folding, which is exactly why the power
#' cepstrum loses the phase.
#'
#' @param x Coerced to numeric by the body, with \code{as.numeric}.
#' @return A list with \code{from_complex}, \code{direct}, \code{residual}, \code{max_residual}, \code{relative_residual}, \code{phase_lost}, \code{n}, \code{method}.
#' @export
PCepRel <- function(x) {
  # eq (4.83): y_hat_p(n) = [y_hat(n) + y_hat(-n)]^2 -- the squared even
  # part of the complex cepstrum.  The odd part, where the phase lives,
  # is annihilated by the folding, which is exactly why the power
  # cepstrum loses the phase.
  xs <- as.numeric(x)
  n <- length(xs)
  if (n < 4L) stop("need at least four samples")
  c0 <- CCepstrum(xs)$cepstrum
  idx <- seq_len(n) - 1L
  folded <- (c0 + c0[((-idx) %% n) + 1L])^2
  direct <- PCepstrum(xs, square = TRUE)$cepstrum
  resid <- folded - direct
  scale <- max(abs(direct))
  if (scale == 0) scale <- 1
  list(
    from_complex = folded, direct = direct, residual = resid,
    max_residual = max(abs(resid)),
    relative_residual = max(abs(resid)) / scale,
    phase_lost = TRUE, n = n,
    method = "Rangayyan (2024) eq. (4.83)"
  )
}

#' Section 4.7.3: the vocal tract lives at LOW quefrency, the glottal
#'
#' excitation at the pitch period and its multiples.  The window is
#' applied SYMMETRICALLY about zero quefrency, because the cepstrum of a
#' mixed-phase signal is two-sided (eq 4.72) and keeping only the causal
#' half would discard the maximum-phase component.
#'
#' @param cepstrum_values Coerced to numeric by the body, with \code{as.numeric}.
#' @param low Optional; may be \code{NULL}. Coerced to integer by the body, with \code{as.integer}.
#' @param high Optional; may be \code{NULL}. Coerced to integer by the body, with \code{as.integer}.
#' @param keep One of \code{"band"}, \code{"high"}, \code{"low"}. Defaults to \code{"low"}.
#' @return A list with \code{liftered}, \code{n}, \code{low}, \code{high}, \code{keep}, \code{symmetric}, \code{n_kept}, \code{energy_kept}, \code{method}.
#' @export
Lifter <- function(cepstrum_values, low = NULL, high = NULL, keep = "low") {
  # Section 4.7.3: the vocal tract lives at LOW quefrency, the glottal
  # excitation at the pitch period and its multiples.  The window is
  # applied SYMMETRICALLY about zero quefrency, because the cepstrum of
  # a mixed-phase signal is two-sided (eq 4.72) and keeping only the
  # causal half would discard the maximum-phase component.
  cc <- as.numeric(cepstrum_values)
  n <- length(cc)
  if (n < 2L) stop("need at least two cepstral samples")
  half <- n %/% 2L
  lo <- if (is.null(low)) 0L else as.integer(low)
  hi <- if (is.null(high)) half else as.integer(high)
  if (lo < 0L || hi < 0L) stop("quefrency limits must be nonnegative")
  if (hi < lo) stop("high must not be below low")
  if (!keep %in% c("low", "high", "band")) {
    stop("keep must be 'low', 'high' or 'band'")
  }
  idx <- seq_len(n) - 1L
  q <- abs(ifelse(idx <= half, idx, idx - n))
  take <- switch(keep,
    low = q <= hi,
    high = q >= lo,
    band = q >= lo & q <= hi
  )
  out <- ifelse(take, cc, 0)
  list(
    liftered = out, n = n, low = lo, high = hi, keep = keep,
    symmetric = TRUE, n_kept = sum(take),
    energy_kept = if (any(cc != 0)) sum(out^2) / sum(cc^2) else 0,
    method = "Rangayyan (2024) Section 4.7.3 (cepstral liftering)"
  )
}

#' Section 4.7.1, Figure 4.23: log -> linear filter -> exp.  The signal
#'
#' must be strictly positive (eq 4.59\'s side condition); a signal that
#' crosses zero needs the complex-log route of HomDeconv.  Rejected
#' rather than clipped, since clipping changes the factorization.
#'
#' @param y Coerced to numeric by the body, with \code{as.numeric}.
#' @param cutoff Coerced to integer by the body, with \code{as.integer}.
#' @param keep One of \code{"high"}, \code{"low"}. Defaults to \code{"low"}.
#' @return A list with \code{y}, \code{log_domain}, \code{log_input}, \code{cutoff}, \code{keep}, \code{n}, \code{stages}, \code{method}.
#' @export
HomoFilt <- function(y, cutoff, keep = "low") {
  # Section 4.7.1, Figure 4.23: log -> linear filter -> exp.  The signal
  # must be strictly positive (eq 4.59's side condition); a signal that
  # crosses zero needs the complex-log route of HomDeconv.  Rejected
  # rather than clipped, since clipping changes the factorization.
  ys <- as.numeric(y)
  n <- length(ys)
  if (n < 4L) stop("need at least four samples")
  if (any(ys <= 0)) {
    stop(
      "the multiplicative homomorphic filter needs a strictly positive ",
      "signal (eq. 4.59); use the complex-cepstrum route for signed data"
    )
  }
  if (!keep %in% c("low", "high")) stop("keep must be 'low' or 'high'")
  k <- as.integer(cutoff)
  if (k < 0L || k > n %/% 2L) stop("cutoff must lie in 0..N/2")
  ly <- log(ys)
  f <- .morie_rg_dft(ly)
  idx <- seq_len(n) - 1L
  band <- pmin(idx, n - idx)
  take <- if (keep == "low") band <= k else band > k
  filtered <- .morie_rg_idft_re(ifelse(take, f$re, 0), ifelse(take, f$im, 0))
  list(
    y = exp(filtered), log_domain = filtered, log_input = ly, cutoff = k,
    keep = keep, n = n, stages = c("log", "linear filter", "exp"),
    method = "Rangayyan (2024) Section 4.7.1, eqs. (4.58)-(4.60)"
  )
}

#' Section 4.7.2: DFT -> complex log -> IDFT -> lifter -> DFT -> exp ->
#'
#' IDFT.  Low quefrency estimates the slowly varying component (the
#' vocal tract, the basic wavelet); high quefrency the excitation.
#'
#' @param y Coerced to numeric by the body, with \code{as.numeric}.
#' @param cutoff Coerced to integer by the body, with \code{as.integer}.
#' @param keep Carried through into a list the body builds. Defaults to \code{"low"}.
#' @return A list with \code{y}, \code{cepstrum}, \code{liftered}, \code{cutoff}, \code{keep}, \code{n}, \code{linear_phase_removed}, \code{imaginary_energy}, \code{stages}, \code{method}.
#' @export
HomDeconv <- function(y, cutoff, keep = "low") {
  # Section 4.7.2: DFT -> complex log -> IDFT -> lifter -> DFT -> exp ->
  # IDFT.  Low quefrency estimates the slowly varying component (the
  # vocal tract, the basic wavelet); high quefrency the excitation.
  ys <- as.numeric(y)
  n <- length(ys)
  if (n < 8L) stop("need at least eight samples")
  cep <- CCepstrum(ys)
  lf <- Lifter(cep$cepstrum, high = cutoff, low = cutoff, keep = keep)$liftered
  f <- .morie_rg_dft(lf)
  m <- exp(f$re)
  out_re <- m * cos(f$im)
  out_im <- m * sin(f$im)
  list(
    y = .morie_rg_idft_re(out_re, out_im), cepstrum = cep$cepstrum,
    liftered = lf, cutoff = as.integer(cutoff), keep = keep, n = n,
    linear_phase_removed = cep$linear_phase_removed,
    imaginary_energy = sum(out_im^2),
    stages = c(
      "DFT", "complex log", "IDFT", "lifter", "DFT", "exp",
      "IDFT"
    ),
    method = "Rangayyan (2024) Section 4.7.2, eqs. (4.61)-(4.66)"
  )
}

#' Section 4.7.3.  The two lifters must PARTITION the quefrency axis
#'
#' (|q| <= k and |q| > k); sharing the cutoff keeps q = 0 and q = k in
#' both halves and the reconstruction fails.  And because the cepstra
#' add (eq 4.66), exponentiating the sum gives the CIRCULAR convolution
#' of the components -- reconstructing linearly leaves a wrap-around
#' error that looks like a failure of the separation.
#'
#' @param y Coerced to numeric by the body, with \code{as.numeric}.
#' @param cutoff Coerced to integer by the body, with \code{as.integer}.
#' @return A list with \code{low_time}, \code{high_time}, \code{cutoff}, \code{n}, \code{reconstruction}, \code{reconstruction_error}, \code{relative_error}, \code{separation_premise}, \code{method}.
#' @export
HomPred <- function(y, cutoff) {
  # Section 4.7.3.  The two lifters must PARTITION the quefrency axis
  # (|q| <= k and |q| > k); sharing the cutoff keeps q = 0 and q = k in
  # both halves and the reconstruction fails.  And because the cepstra
  # add (eq 4.66), exponentiating the sum gives the CIRCULAR convolution
  # of the components -- reconstructing linearly leaves a wrap-around
  # error that looks like a failure of the separation.
  ys <- as.numeric(y)
  n <- length(ys)
  if (n < 8L) stop("need at least eight samples")
  k <- as.integer(cutoff)
  if (k < 1L || k >= n %/% 2L) stop("cutoff must lie in 1..N/2-1")
  low <- HomDeconv(ys, k, keep = "low")$y
  high <- HomDeconv(ys, k + 1L, keep = "high")$y
  idx <- seq_len(n) - 1L
  conv <- vapply(idx, function(i) {
    .morie_fsum(low * high[((i - idx) %% n) + 1L])
  }, numeric(1))
  err <- max(abs(conv - ys))
  scale <- max(abs(ys))
  if (scale == 0) scale <- 1
  list(
    low_time = low, high_time = high, cutoff = k, n = n,
    reconstruction = conv, reconstruction_error = err,
    relative_error = err / scale,
    separation_premise = paste(
      "eq. (4.66) assumes the two components",
      "occupy non-overlapping quefrency ranges"
    ),
    method = "Rangayyan (2024) Section 4.7.3"
  )
}

#' VocalTract
#'
#' A step of the rangayyan_cep implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param y Coerced to numeric by the body, with \code{as.numeric}.
#' @param fs Coerced to numeric by the body, with \code{as.numeric}.
#' @param pitch_period Optional; may be \code{NULL}. Coerced to numeric by the body, with \code{as.numeric}.
#' @param cutoff Optional; may be \code{NULL}. Coerced to integer by the body, with \code{as.integer}.
#' @param pitch_range A vector; indexed elementwise. Defaults to \code{c(0.002, 0.02)}.
#' @return A list with \code{response}, \code{cepstrum}, \code{cutoff}, \code{pitch_period}, \code{pitch_hz}, \code{peak_quefrency}, \code{fs}, \code{n}, \code{method}.
#' @export
VocalTract <- function(y, fs, pitch_period = NULL, cutoff = NULL,
                       pitch_range = c(0.002, 0.020)) {
  # Section 4.7.3: the vocal tract contributes only below the pitch
  # period, where the excitation's impulses begin.  The pitch is the
  # largest cepstral peak INSIDE pitch_range -- eq (4.80) puts impulses
  # at the delay and its multiples, so searching the whole cepstrum
  # reports a rahmonic and a pitch two or three times too low.
  ys <- as.numeric(y)
  n <- length(ys)
  fsv <- as.numeric(fs)
  if (fsv <= 0) stop("fs must be positive")
  if (n < 16L) stop("need at least sixteen samples")
  cep <- CCepstrum(ys)$cepstrum
  half <- n %/% 2L
  lo_q <- max(1L, as.integer(pitch_range[1] * fsv))
  hi_q <- min(half, as.integer(pitch_range[2] * fsv) + 1L)
  if (hi_q <= lo_q) {
    stop(
      "the pitch range holds no quefrency bins at this sampling rate ",
      "and record length"
    )
  }
  if (is.null(pitch_period)) {
    rng <- lo_q:(hi_q - 1L)
    peak <- rng[which.max(abs(cep[rng + 1L]))]
    period <- peak / fsv
  } else {
    period <- as.numeric(pitch_period)
    peak <- as.integer(round(period * fsv))
  }
  k <- if (!is.null(cutoff)) as.integer(cutoff) else max(1L, as.integer(0.9 * peak))
  if (k >= half) stop("the lifter cutoff exceeds the usable quefrency range")
  est <- HomDeconv(ys, k, keep = "low")
  list(
    response = est$y, cepstrum = cep, cutoff = k, pitch_period = period,
    pitch_hz = if (period > 0) 1 / period else NULL,
    peak_quefrency = peak, fs = fsv, n = n,
    method = paste(
      "Rangayyan (2024) Section 4.7.3 (vocal-tract",
      "response by low-time liftering)"
    )
  )
}

#' Section 4.7.2, after eq (4.73): a minimum-phase signal has a CAUSAL
#'
#' complex cepstrum.  Folding the anticausal half onto the causal half
#' reflects every root outside the unit circle to its reciprocal inside,
#' leaving the magnitude spectrum untouched -- which is what
#' "correspondent" means, and is the check returned.
#'
#' @param x Coerced to numeric by the body, with \code{as.numeric}.
#' @return A list with \code{y}, \code{cepstrum}, \code{n}, \code{magnitude_error}, \code{magnitude_preserved}, \code{energy_front_loaded}, \code{method}.
#' @export
MinPhase <- function(x) {
  # Section 4.7.2, after eq (4.73): a minimum-phase signal has a CAUSAL
  # complex cepstrum.  Folding the anticausal half onto the causal half
  # reflects every root outside the unit circle to its reciprocal
  # inside, leaving the magnitude spectrum untouched -- which is what
  # "correspondent" means, and is the check returned.
  xs <- as.numeric(x)
  n <- length(xs)
  if (n < 8L) stop("need at least eight samples")
  cc <- CCepstrum(xs)$cepstrum
  half <- n %/% 2L
  folded <- numeric(n)
  folded[1] <- cc[1]
  if (half > 1L) {
    i <- 2:half
    folded[i] <- cc[i] + cc[n - i + 2L]
  }
  if (n %% 2L == 0L) folded[half + 1L] <- cc[half + 1L]
  f <- .morie_rg_dft(folded)
  m <- exp(f$re)
  y <- .morie_rg_idft_re(m * cos(f$im), m * sin(f$im))
  src <- sqrt(.morie_rg_dft(xs)$re^2 + .morie_rg_dft(xs)$im^2)
  d <- .morie_rg_dft(y)
  dst <- sqrt(d$re^2 + d$im^2)
  gap <- max(abs(src - dst))
  scale <- max(src)
  if (scale == 0) scale <- 1
  list(
    y = y, cepstrum = cc, n = n, magnitude_error = gap,
    magnitude_preserved = gap <= 1e-6 * scale,
    energy_front_loaded = sum(y[seq_len(half)]^2) >=
      sum(y[(half + 1L):n]^2),
    method = paste(
      "Rangayyan (2024) Section 4.7.2 (minimum-phase",
      "correspondent from the causal cepstrum)"
    )
  )
}

#' Mfcc
#'
#' A step of the rangayyan_cep implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param x Coerced to numeric by the body, with \code{as.numeric}.
#' @param fs Coerced to numeric by the body, with \code{as.numeric}.
#' @param n_filters Coerced to integer by the body, with \code{as.integer}. Defaults to \code{26}.
#' @param n_coeffs Coerced to integer by the body, with \code{as.integer}. Defaults to \code{13}.
#' @param fmin The body requires: need 0 <= fmin < fmax <= fs/2. Defaults to \code{0}.
#' @param fmax Optional; may be \code{NULL}. Coerced to numeric by the body, with \code{as.numeric}.
#' @return A list with \code{mfcc}, \code{filterbank_energies}, \code{log_energies}, \code{edges}, \code{n_filters}, \code{n_coeffs}, \code{fs}, \code{empty_filters}, \code{c0_is_energy}, \code{method}.
#' @export
Mfcc <- function(x, fs, n_filters = 26, n_coeffs = 13, fmin = 0,
                 fmax = NULL) {
  # Davis and Mermelstein (1980): power spectrum, triangular mel
  # filterbank, log band energies, DCT-II.  A cepstrum in Rangayyan's
  # sense, but with two departures the book's cepstrum does not make --
  # a mel-warped frequency axis and a DCT in place of the inverse DFT --
  # so it is cited to Davis and Mermelstein.  Coefficient 0 is the log
  # total energy and tracks recording gain, not spectral shape.
  xs <- as.numeric(x)
  n <- length(xs)
  fsv <- as.numeric(fs)
  if (fsv <= 0) stop("fs must be positive")
  if (n < 8L) stop("need at least eight samples")
  nf <- as.integer(n_filters)
  nc <- as.integer(n_coeffs)
  if (nf < 2L) stop("need at least two mel filters")
  if (nc < 1L || nc > nf) stop("n_coeffs must lie in 1..n_filters")
  top <- if (is.null(fmax)) fsv / 2 else as.numeric(fmax)
  if (!(fmin >= 0 && fmin < top && top <= fsv / 2)) {
    stop("need 0 <= fmin < fmax <= fs/2")
  }
  f <- .morie_rg_dft(xs)
  half <- n %/% 2L + 1L
  power <- (f$re[seq_len(half)]^2 + f$im[seq_len(half)]^2) / n
  freqs <- (seq_len(half) - 1L) * fsv / n
  to_mel <- function(v) 2595 * log10(1 + v / 700)
  from_mel <- function(v) 700 * (10^(v / 2595) - 1)
  m_lo <- to_mel(fmin)
  m_hi <- to_mel(top)
  edges <- from_mel(m_lo + (m_hi - m_lo) * (0:(nf + 1L)) / (nf + 1L))
  energies <- vapply(seq_len(nf), function(i) {
    lo <- edges[i]
    mid <- edges[i + 1L]
    hi <- edges[i + 2L]
    up <- freqs >= lo & freqs <= mid & mid > lo
    dn <- freqs > mid & freqs <= hi & hi > mid
    .morie_fsum(power[up] * (freqs[up] - lo) / (mid - lo)) +
      .morie_fsum(power[dn] * (hi - freqs[dn]) / (hi - mid))
  }, numeric(1))
  floor_ <- 1e-300
  logs <- log(pmax(energies, floor_))
  coeffs <- vapply(0:(nc - 1L), function(k) {
    .morie_fsum(logs * cos(pi * k * (seq_len(nf) - 0.5) / nf))
  }, numeric(1))
  list(
    mfcc = coeffs, filterbank_energies = energies, log_energies = logs,
    edges = edges, n_filters = nf, n_coeffs = nc, fs = fsv,
    empty_filters = sum(energies <= floor_), c0_is_energy = TRUE,
    method = paste(
      "Davis and Mermelstein (1980); a mel-warped,",
      "DCT-based cepstrum, not the homomorphic cepstrum",
      "of Rangayyan (2024) Section 4.7"
    )
  )
}

# pre-policy spellings
morie_cepstrum <- Cepstrum
morie_ch4_complex_cepstrum <- CCepstrum
morie_complex_cepstrum <- CCepX
morie_ch4_multiplicative_model <- MultModel
morie_ch4_log_separation <- LogSep
morie_ch4_convolution_model <- ConvModel
morie_ch4_complex_cepstra_sum <- CCepSum
morie_ch4_rational_z_form <- RatZ
morie_ch4_complex_cepstrum_closed <- CCepClosed
morie_ch4_complex_cepstrum_decay <- CCepDecay
morie_ch4_log_echo_series <- EchoSeries
morie_ch4_power_cepstrum <- PCepstrum
morie_ch4_power_cepstrum_sum <- PCepSum
morie_ch4_power_cepstrum_relation <- PCepRel
morie_liftering <- Lifter
morie_homomorphic <- HomoFilt
morie_homomorphic_deconv <- HomDeconv
morie_homomorphic_pred <- HomPred
morie_vocal_tract <- VocalTract
morie_min_phase <- MinPhase
morie_mfcc <- Mfcc

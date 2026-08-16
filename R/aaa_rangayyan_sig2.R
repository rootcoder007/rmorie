# Rangayyan signals and systems -- linear convolution, LSI systems in
# series and parallel, the LTI transform-domain property, and the
# modulated signal models.  Mirror of the Python bsasig module.
#
# Two placeholder formulas were wrong and are corrected here: the AM
# model (Section 5.5.1 uses the suppressed-carrier y = x cos(wc t), not
# A(1 + m) cos), and "periodic convolution", which is eq (3.90) -- the
# equation CircConv already implements, so it delegates.

#' Eqs (3.36)-(3.37): y(n) = sum_k x(k) h(n-k) = sum_k h(k) x(n-k),
#'
#' causality assumed as the book states under eq (3.37).  eq (3.39)
#' reads the same sum as delayed, weighted copies of h; those copies are
#' returned so the overlap of Figure 3.19 is visible.
#'
#' @param x Coerced to numeric by the body, with \code{as.numeric}.
#' @param h Coerced to numeric by the body, with \code{as.numeric}.
#' @param causal A flag; the body branches on it. Defaults to \code{TRUE}.
#' @return A list with \code{y}, \code{n}, \code{n_x}, \code{n_h}, \code{contributions}, \code{commutes}, \code{causal}, \code{method}.
#' @export
LinConv <- function(x, h, causal = TRUE) {
  # eqs (3.36)-(3.37): y(n) = sum_k x(k) h(n-k) = sum_k h(k) x(n-k),
  # causality assumed as the book states under eq (3.37).  eq (3.39)
  # reads the same sum as delayed, weighted copies of h; those copies
  # are returned so the overlap of Figure 3.19 is visible.
  xs <- as.numeric(x)
  hs <- as.numeric(h)
  if (!length(xs) || !length(hs)) {
    stop("both sequences need at least one sample")
  }
  n <- length(xs)
  m <- length(hs)
  y <- .morie_rg_conv(xs, hs)
  swapped <- .morie_rg_conv(hs, xs)
  contributions <- matrix(0, nrow = n, ncol = n + m - 1L)
  for (i in seq_len(n)) contributions[i, i:(i + m - 1L)] <- xs[i] * hs
  list(
    y = y, n = n + m - 1L, n_x = n, n_h = m,
    contributions = contributions,
    commutes = max(abs(y - swapped)) <= 1e-12 * (1 + max(abs(y))),
    causal = isTRUE(causal),
    method = "Rangayyan (2024) eqs. (3.36)-(3.39)"
  )
}

#' Eqs (3.43)-(3.45): s = x*h1, y = s*h2 = x*h, h = h1*h2.  One method,
#'
#' so one function; the equivalence in eq (3.44) is measured, not
#' asserted -- filtering twice is compared against filtering once with
#' h.
#'
#' @param x Coerced to numeric by the body, with \code{as.numeric}.
#' @param h1 Coerced to numeric by the body, with \code{as.numeric}.
#' @param h2 Coerced to numeric by the body, with \code{as.numeric}.
#' @return A list with \code{s}, \code{y}, \code{h}, \code{y_via_combined}, \code{max_difference}, \code{equivalent}, \code{method}.
#' @export
LsiSer <- function(x, h1, h2) {
  # eqs (3.43)-(3.45): s = x*h1, y = s*h2 = x*h, h = h1*h2.  One method,
  # so one function; the equivalence in eq (3.44) is measured, not
  # asserted -- filtering twice is compared against filtering once with h.
  xs <- as.numeric(x)
  a <- as.numeric(h1)
  b <- as.numeric(h2)
  if (!length(xs) || !length(a) || !length(b)) {
    stop("input and both impulse responses need samples")
  }
  s <- .morie_rg_conv(xs, a)
  y <- .morie_rg_conv(s, b)
  h <- .morie_rg_conv(a, b)
  direct <- .morie_rg_conv(xs, h)
  gap <- max(abs(y - direct))
  list(
    s = s, y = y, h = h, y_via_combined = direct, max_difference = gap,
    equivalent = gap <= 1e-9 * (1 + max(abs(y))),
    method = "Rangayyan (2024) eqs. (3.43)-(3.45)"
  )
}

#' Eq (3.44): the cascade output, read off LsiSer rather than convolved
#'
#' a second time -- the content of the equation is that the cascade IS a
#' single filter, not that there are two ways to compute it.
#'
#' @param x See Usage.
#' @param h1 See Usage.
#' @param h2 See Usage.
#' @return A list with \code{y}, \code{h}, \code{s}, \code{equivalent}, \code{max_difference}, \code{method}.
#' @export
LsiSerY <- function(x, h1, h2) {
  # eq (3.44): the cascade output, read off LsiSer rather than convolved
  # a second time -- the content of the equation is that the cascade IS a
  # single filter, not that there are two ways to compute it.
  r <- LsiSer(x, h1, h2)
  list(
    y = r$y, h = r$h, s = r$s, equivalent = r$equivalent,
    max_difference = r$max_difference,
    method = "Rangayyan (2024) eqs. (3.44)-(3.45)"
  )
}

#' Eqs (3.46)-(3.49): s1 = x*h1, s2 = x*h2, y = s1 + s2 = x*(h1+h2)
#'
#' The shorter response is zero-extended before the addition; truncating
#' instead would silently drop the tail of the longer filter.
#'
#' @param x Coerced to numeric by the body, with \code{as.numeric}.
#' @param h1 Coerced to numeric by the body, with \code{as.numeric}.
#' @param h2 Coerced to numeric by the body, with \code{as.numeric}.
#' @return A list with \code{s1}, \code{s2}, \code{y}, \code{h}, \code{y_via_combined}, \code{max_difference}, \code{equivalent}, \code{method}.
#' @export
LsiPar <- function(x, h1, h2) {
  # eqs (3.46)-(3.49): s1 = x*h1, s2 = x*h2, y = s1 + s2 = x*(h1+h2).
  # The shorter response is zero-extended before the addition; truncating
  # instead would silently drop the tail of the longer filter.
  xs <- as.numeric(x)
  a <- as.numeric(h1)
  b <- as.numeric(h2)
  if (!length(xs) || !length(a) || !length(b)) {
    stop("input and both impulse responses need samples")
  }
  m <- max(length(a), length(b))
  h <- c(a, numeric(m - length(a))) + c(b, numeric(m - length(b)))
  s1 <- .morie_rg_conv(xs, a)
  s2 <- .morie_rg_conv(xs, b)
  ny <- max(length(s1), length(s2))
  y <- c(s1, numeric(ny - length(s1))) + c(s2, numeric(ny - length(s2)))
  direct <- .morie_rg_conv(xs, h)
  gap <- max(abs(y - direct))
  list(
    s1 = s1, s2 = s2, y = y, h = h, y_via_combined = direct,
    max_difference = gap, equivalent = gap <= 1e-9 * (1 + max(abs(y))),
    method = "Rangayyan (2024) eqs. (3.46)-(3.49)"
  )
}

#' Eq (3.47): identical in form to eq (3.46) -- both branches of a
#'
#' parallel structure see the same input.
#'
#' @param x Coerced to numeric by the body, with \code{as.numeric}.
#' @param h2 Coerced to numeric by the body, with \code{as.numeric}.
#' @return A list with \code{s2}, \code{n}, \code{method}.
#' @export
LsiPar2 <- function(x, h2) {
  # eq (3.47): identical in form to eq (3.46) -- both branches of a
  # parallel structure see the same input.
  xs <- as.numeric(x)
  b <- as.numeric(h2)
  if (!length(xs) || !length(b)) {
    stop("input and impulse response need samples")
  }
  out <- .morie_rg_conv(xs, b)
  list(s2 = out, n = length(out), method = "Rangayyan (2024) eq. (3.47)")
}

#' Eqs (3.48)-(3.49): the parallel counterpart of eq (3.44) -- here the
#'
#' impulse responses ADD where a cascade convolves them.
#'
#' @param x See Usage.
#' @param h1 See Usage.
#' @param h2 See Usage.
#' @return A list with \code{y}, \code{h}, \code{s1}, \code{s2}, \code{equivalent}, \code{max_difference}, \code{method}.
#' @export
LsiParY <- function(x, h1, h2) {
  # eqs (3.48)-(3.49): the parallel counterpart of eq (3.44) -- here the
  # impulse responses ADD where a cascade convolves them.
  r <- LsiPar(x, h1, h2)
  list(
    y = r$y, h = r$h, s1 = r$s1, s2 = r$s2, equivalent = r$equivalent,
    max_difference = r$max_difference,
    method = "Rangayyan (2024) eqs. (3.48)-(3.49)"
  )
}

#' Eqs (3.50), (3.53): convolution in time is multiplication in the s
#'
#' and omega domains.  s = j omega recovers the frequency-domain form,
#' so one function covers both.
#'
#' @param x Coerced to numeric by the body, with \code{as.numeric}.
#' @param h Coerced to numeric by the body, with \code{as.numeric}.
#' @param s Optional; may be \code{NULL}. Coerced to complex by the body, with \code{as.complex}.
#' @param omega Optional; may be \code{NULL}. Coerced to numeric by the body, with \code{as.numeric}.
#' @param dt Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{1}.
#' @return A list with \code{y}, \code{Y}, \code{X}, \code{H}, \code{XH}, \code{s}, \code{max_difference}, \code{holds}, \code{method}.
#' @export
LtiProd <- function(x, h, s = NULL, omega = NULL, dt = 1) {
  # eqs (3.50), (3.53): convolution in time is multiplication in the s
  # and omega domains.  s = j omega recovers the frequency-domain form,
  # so one function covers both.
  xs <- as.numeric(x)
  hs <- as.numeric(h)
  if (!length(xs) || !length(hs)) {
    stop("both signals need at least one sample")
  }
  if (is.null(s) == is.null(omega)) stop("give exactly one of s, omega")
  step <- as.numeric(dt)
  if (step <= 0) stop("dt must be positive")
  pts <- if (!is.null(s)) {
    as.complex(s)
  } else {
    complex(real = 0, imaginary = as.numeric(omega))
  }
  y <- .morie_rg_conv(xs, hs) * step
  lap <- function(sig, sv) {
    t <- (seq_along(sig) - 1) * step
    sum(sig * exp(-Re(sv) * t) *
      complex(
        real = cos(-Im(sv) * t),
        imaginary = sin(-Im(sv) * t)
      )) * step
  }
  Y <- vapply(pts, function(p) lap(y, p), complex(1))
  X <- vapply(pts, function(p) lap(xs, p), complex(1))
  H <- vapply(pts, function(p) lap(hs, p), complex(1))
  prod <- X * H
  gap <- max(Mod(Y - prod))
  one <- length(pts) == 1L
  list(
    y = y, Y = if (one) Y[[1]] else Y, X = if (one) X[[1]] else X,
    H = if (one) H[[1]] else H, XH = if (one) prod[[1]] else prod,
    s = if (one) pts[[1]] else pts, max_difference = gap,
    holds = gap <= 1e-8 * (1 + max(Mod(prod))),
    method = "Rangayyan (2024) eqs. (3.50), (3.53)"
  )
}

#' Eq (3.90); the same equation CircConv implements, so it delegates --
#'
#' two copies of one equation is how the two drift apart.
#'
#' @param x See Usage.
#' @param h See Usage.
#' @param npoints Defaults to \code{NULL}.
#' @return The value of \code{r}, as built in the body.
#' @export
PerConv <- function(x, h, npoints = NULL) {
  # eq (3.90); the same equation CircConv implements, so it delegates --
  # two copies of one equation is how the two drift apart.
  r <- CircConv(x, h, npoints = npoints)
  r$method <- "Rangayyan (2024) eq. (3.90)"
  r
}

#' Section 5.5.1: y(t) = x(t) cos(wc t) -- double-sideband SUPPRESSED
#'
#' carrier -- with synchronous demodulation x_d = y cos(wc t) = 0.5 x +
#' 0.5 x cos(2 wc t).  The placeholder stated the conventional
#' large-carrier form A(1 + m) cos(wc t), which is a different model; it
#' is available under conventional=TRUE rather than substituted.
#'
#' @param x Coerced to numeric by the body, with \code{as.numeric}.
#' @param fc Coerced to numeric by the body, with \code{as.numeric}.
#' @param fs Coerced to numeric by the body, with \code{as.numeric}.
#' @param conventional A flag; the body branches on it. Defaults to \code{FALSE}.
#' @param depth Numeric; combined arithmetically in the body. Defaults to \code{1}.
#' @return A list with \code{y}, \code{carrier}, \code{demodulated}, \code{fc}, \code{fs}, \code{suppressed_carrier}, \code{baseband_gain}, \code{image_frequency}, \code{method}.
#' @export
AmSig <- function(x, fc, fs, conventional = FALSE, depth = 1) {
  # Section 5.5.1: y(t) = x(t) cos(wc t) -- double-sideband SUPPRESSED
  # carrier -- with synchronous demodulation x_d = y cos(wc t) =
  # 0.5 x + 0.5 x cos(2 wc t).  The placeholder stated the conventional
  # large-carrier form A(1 + m) cos(wc t), which is a different model;
  # it is available under conventional=TRUE rather than substituted.
  xs <- as.numeric(x)
  if (!length(xs)) stop("need at least one sample")
  fsv <- as.numeric(fs)
  fcv <- as.numeric(fc)
  if (fsv <= 0) stop("fs must be positive")
  if (!(fcv > 0 && fcv < fsv / 2)) {
    stop(sprintf(
      "the carrier must satisfy 0 < fc < fs/2, got fc=%g with fs=%g",
      fcv, fsv
    ))
  }
  w <- 2 * pi * fcv / fsv
  carrier <- cos(w * (seq_along(xs) - 1))
  y <- if (conventional) (1 + depth * xs) * carrier else xs * carrier
  list(
    y = y, carrier = carrier, demodulated = y * carrier, fc = fcv,
    fs = fsv, suppressed_carrier = !conventional, baseband_gain = 0.5,
    image_frequency = 2 * fcv,
    method = "Rangayyan (2024) Section 5.5.1"
  )
}

#' Rangayyan names FM as a signal model but prints no equation for it,
#'
#' unlike AM in Section 5.5.1, so the standard definition is used and
#' said so: y = A cos(2 pi fc t + 2 pi kf integral m), f_inst = fc + kf
#' m. The phase integral is trapezoidal; a plain running sum biases it
#' by half a sample of m at each end, which drifts over a long record.
#'
#' @param m Coerced to numeric by the body, with \code{as.numeric}.
#' @param fc Coerced to numeric by the body, with \code{as.numeric}.
#' @param fs Coerced to numeric by the body, with \code{as.numeric}.
#' @param kf Numeric; combined arithmetically in the body. Defaults to \code{1}.
#' @param amplitude Numeric; combined arithmetically in the body. Defaults to \code{1}.
#' @return A list with \code{y}, \code{phase}, \code{instantaneous_frequency}, \code{fc}, \code{fs}, \code{kf}, \code{max_instantaneous_frequency}, \code{min_instantaneous_frequency}, \code{aliases}, \code{method}.
#' @export
FmSig <- function(m, fc, fs, kf = 1, amplitude = 1) {
  # Rangayyan names FM as a signal model but prints no equation for it,
  # unlike AM in Section 5.5.1, so the standard definition is used and
  # said so:  y = A cos(2 pi fc t + 2 pi kf integral m),
  #           f_inst = fc + kf m.
  # The phase integral is trapezoidal; a plain running sum biases it by
  # half a sample of m at each end, which drifts over a long record.
  ms <- as.numeric(m)
  if (!length(ms)) stop("need at least one sample")
  fsv <- as.numeric(fs)
  fcv <- as.numeric(fc)
  if (fsv <= 0) stop("fs must be positive")
  if (!(fcv > 0 && fcv < fsv / 2)) {
    stop("the carrier must satisfy 0 < fc < fs/2")
  }
  dt <- 1 / fsv
  acc <- c(0, cumsum(0.5 * (ms[-1] + ms[-length(ms)]) * dt))
  phase <- 2 * pi * fcv * (seq_along(ms) - 1) * dt + 2 * pi * kf * acc
  finst <- fcv + kf * ms
  list(
    y = amplitude * cos(phase), phase = phase,
    instantaneous_frequency = finst, fc = fcv, fs = fsv,
    kf = as.numeric(kf), max_instantaneous_frequency = max(finst),
    min_instantaneous_frequency = min(finst),
    aliases = max(abs(finst)) >= fsv / 2,
    method = paste(
      "standard FM model; Rangayyan (2024) names FM as a",
      "signal model without printing this equation"
    )
  )
}

#' A time-variant system needs h(n, m), one response per output instant:
#'
#' y(n) = sum_m h(n, m) x(n - m). The convolution sum still holds
#' instant by instant, but the kernel is no longer shift-invariant, so
#' no single transfer function describes it -- the contrast with eq
#' (3.36) is the point.
#'
#' @param x Coerced to numeric by the body, with \code{as.numeric}.
#' @param h A list; the body checks with \code{is.list}.
#' @return A list with \code{y}, \code{n}, \code{kernel_lengths}, \code{shift_invariant}, \code{method}.
#' @export
TvLsi <- function(x, h) {
  # A time-variant system needs h(n, m), one response per output instant:
  #   y(n) = sum_m h(n, m) x(n - m).
  # The convolution sum still holds instant by instant, but the kernel is
  # no longer shift-invariant, so no single transfer function describes
  # it -- the contrast with eq (3.36) is the point.
  xs <- as.numeric(x)
  n <- length(xs)
  if (!n) stop("need at least one sample")
  rows <- if (is.list(h)) {
    lapply(h, as.numeric)
  } else {
    rep(list(as.numeric(h)), n)
  }
  if (length(rows) == 1L) rows <- rep(rows, n)
  if (length(rows) != n) {
    stop(sprintf(
      "give one impulse response per sample (%d), or one for all; got %d",
      n, length(rows)
    ))
  }
  y <- vapply(seq_len(n), function(i) {
    row <- rows[[i]]
    mm <- seq_along(row) - 1L
    keep <- (i - mm) >= 1L & (i - mm) <= n
    if (!any(keep)) 0 else .morie_fsum(row[keep] * xs[i - mm[keep]])
  }, numeric(1))
  first <- rows[[1]]
  invariant <- all(vapply(rows, function(r) {
    length(r) == length(first) && all(abs(r - first) < 1e-12)
  }, logical(1)))
  list(
    y = y, n = n, kernel_lengths = vapply(rows, length, integer(1)),
    shift_invariant = invariant,
    method = "time-variant convolution; contrast Rangayyan (2024) eq. (3.36)"
  )
}

# pre-policy spellings
morie_linear_convolution <- LinConv
morie_ch3_lsi_series <- LsiSer
morie_ch3_lsi_series_total <- LsiSerY
morie_ch3_lsi_parallel <- LsiPar
morie_ch3_lsi_parallel_branch_2 <- LsiPar2
morie_ch3_lsi_parallel_total <- LsiParY
morie_ch3_lti_convolution_property <- LtiProd
morie_ch3_periodic_convolution <- PerConv
morie_am_signal <- AmSig
morie_fm_signal <- FmSig
morie_tvlsi <- TvLsi

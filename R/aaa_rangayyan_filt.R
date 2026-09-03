# Rangayyan filter design: LSI combination, the Laplace route, the
# generic IIR and pole-zero responses, the moving average, the Hann
# filter, and the order-statistic filters.  Mirror of the Python bsafilt
# module (chunk A1 + Section 3.8).
#
# Equation numbers verified in the PDF: 3.45, 3.49, 3.50, 3.52, 3.67,
# 3.68, 3.72, 3.73, 3.97-3.107, and Section 3.8.

#' .morie_rg_polyz
#'
#' A step of the rangayyan_filt implementation. Called by \code{BwDigital}, \code{BwHp},
#' \code{Comb} and 6 others in the module.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param coefs A vector; its length is taken.
#' @param z Coerced to complex by the body, with \code{as.complex}.
#' @return A vector, from \code{vapply}.
#' @export
.morie_rg_polyz <- function(coefs, z) {
  zc <- as.complex(z)
  if (any(zc == 0)) {
    stop("z = 0 is a pole of a causal transfer function")
  }
  vapply(zc, function(zz) {
    sum(coefs * zz^(-(seq_along(coefs) - 1L)))
  }, complex(1))
}

#' Eq (3.45): cascading two LSI systems convolves their impulse
#'
#' responses.  Convolution commutes, so a filter chain may be reordered,
#' and the result is len(h1) + len(h2) - 1 long.
#'
#' @param h_1 Coerced to numeric by the body, with \code{as.numeric}.
#' @param h_2 Coerced to numeric by the body, with \code{as.numeric}.
#' @param n Optional; may be \code{NULL}. Coerced to integer by the body, with \code{as.integer}.
#' @return A list with \code{h}, \code{n_taps}, \code{value}, \code{index},
#' \code{commutes}, \code{longer_than_either_input}, \code{method}.
#' @export
LsiSerH <- function(h_1, h_2, n = NULL) {
  # eq (3.45): cascading two LSI systems convolves their impulse
  # responses.  Convolution commutes, so a filter chain may be reordered,
  # and the result is len(h1) + len(h2) - 1 long.
  a <- as.numeric(h_1)
  b <- as.numeric(h_2)
  if (!length(a) || !length(b)) {
    stop("both impulse responses need at least one tap")
  }
  m <- length(a) + length(b) - 1L
  out <- vapply(seq_len(m) - 1L, function(i) {
    j <- seq.int(max(0L, i - length(b) + 1L), min(i, length(a) - 1L))
    .morie_fsum(a[j + 1L] * b[i - j + 1L])
  }, numeric(1))
  val <- NULL
  if (!is.null(n)) {
    k <- as.integer(n)
    if (k < 0L) stop("n must be a nonnegative index")
    val <- if (k < m) out[k + 1L] else 0
  }
  list(
    h = out, n_taps = m, value = val, index = n, commutes = TRUE,
    longer_than_either_input = TRUE,
    method = "Rangayyan (2024) eq. (3.45); series LSI systems convolve"
  )
}

#' Eq (3.49): parallel branches add their impulse responses, and the
#'
#' result is as long as the LONGER branch -- not longer, the contrast
#' with the series case where the lengths add.
#'
#' @param h_1 Coerced to numeric by the body, with \code{as.numeric}.
#' @param h_2 Coerced to numeric by the body, with \code{as.numeric}.
#' @param n Optional; may be \code{NULL}. Coerced to integer by the body, with \code{as.integer}.
#' @return A list with \code{h}, \code{n_taps}, \code{value}, \code{index},
#' \code{length_is_the_longer_branch}, \code{method}.
#' @export
LsiParH <- function(h_1, h_2, n = NULL) {
  # eq (3.49): parallel branches add their impulse responses, and the
  # result is as long as the LONGER branch -- not longer, the contrast
  # with the series case where the lengths add.
  a <- as.numeric(h_1)
  b <- as.numeric(h_2)
  if (!length(a) || !length(b)) {
    stop("both impulse responses need at least one tap")
  }
  m <- max(length(a), length(b))
  pad <- function(v) c(v, rep(0, m - length(v)))
  out <- pad(a) + pad(b)
  val <- NULL
  if (!is.null(n)) {
    k <- as.integer(n)
    if (k < 0L) stop("n must be a nonnegative index")
    val <- if (k < m) out[k + 1L] else 0
  }
  list(
    h = out, n_taps = m, value = val, index = n,
    length_is_the_longer_branch = TRUE,
    method = "Rangayyan (2024) eq. (3.49); parallel LSI systems add"
  )
}

#' Eq (3.50), by the trapezoidal rule over the samples supplied.  What
#'
#' is returned is the transform OF THE SAMPLED RECORD over the interval
#' it covers, which is why the limits come back with it.
#'
#' @param h Coerced to numeric by the body, with \code{as.numeric}.
#' @param t Coerced to numeric by the body, with \code{as.numeric}.
#' @param s Coerced to complex by the body, with \code{as.complex}.
#' @return A list with \code{H}, \code{s}, \code{t_min}, \code{t_max}, \code{n},
#' \code{trapezoidal}, \code{over_the_sampled_interval_only}, \code{method}.
#' @export
Laplace <- function(h, t, s) {
  # eq (3.50), by the trapezoidal rule over the samples supplied.  What
  # is returned is the transform OF THE SAMPLED RECORD over the interval
  # it covers, which is why the limits come back with it.
  hs <- as.numeric(h)
  ts <- as.numeric(t)
  if (length(hs) != length(ts)) {
    stop("h and t must have the same length")
  }
  if (length(hs) < 2L) stop("need at least two samples to integrate")
  if (any(diff(ts) <= 0)) stop("t must be strictly increasing")
  sv <- as.complex(s)
  out <- vapply(sv, function(z) {
    f <- hs * exp(-z * ts)
    sum(0.5 * (f[-length(f)] + f[-1]) * diff(ts))
  }, complex(1))
  list(
    H = if (length(out) == 1L) out[[1]] else out, s = s,
    t_min = ts[1], t_max = ts[length(ts)], n = length(ts),
    trapezoidal = TRUE, over_the_sampled_interval_only = TRUE,
    method = "Rangayyan (2024) eq. (3.50)"
  )
}

#' Eq (3.52): H(omega) = H(s) at s = j omega.  Legitimate only when the
#'
#' imaginary axis lies inside the region of convergence, which holds for
#' a causal stable system and fails for an unstable one.
#'
#' @param h Coerced to numeric by the body, with \code{as.numeric}.
#' @param omega Coerced to numeric by the body, with \code{as.numeric}.
#' @param t Optional; may be \code{NULL}. Coerced to numeric by the body, with \code{as.numeric}.
#' @param T Optional; may be \code{NULL}. Coerced to numeric by the body, with \code{as.numeric}.
#' @return A list with \code{H}, \code{omega}, \code{magnitude}, \code{phase},
#' \code{t_min}, \code{t_max}, \code{valid_only_inside_the_roc}, \code{method}.
#' @export
LaplaceFr <- function(h, omega, t = NULL, T = NULL) {
  # eq (3.52): H(omega) = H(s) at s = j omega.  Legitimate only when the
  # imaginary axis lies inside the region of convergence, which holds for
  # a causal stable system and fails for an unstable one.
  hs <- as.numeric(h)
  if (length(hs) < 2L) stop("need at least two samples")
  if (is.null(t)) {
    if (is.null(T)) stop("give either the sample times t or the duration T")
    Tv <- as.numeric(T)
    if (Tv <= 0) stop("T must be positive")
    ts <- (seq_along(hs) - 1L) * (Tv / (length(hs) - 1L))
  } else {
    ts <- as.numeric(t)
    if (length(ts) != length(hs)) {
      stop("h and t must have the same length")
    }
  }
  ws <- as.numeric(omega)
  out <- vapply(ws, function(w) {
    f <- complex(real = hs * cos(-w * ts), imaginary = hs * sin(-w * ts))
    sum(0.5 * (f[-length(f)] + f[-1]) * diff(ts))
  }, complex(1))
  scalar <- length(out) == 1L
  list(
    H = if (scalar) out[[1]] else out, omega = omega,
    magnitude = if (scalar) Mod(out[[1]]) else Mod(out),
    phase = if (scalar) Arg(out[[1]]) else Arg(out),
    t_min = ts[1], t_max = ts[length(ts)],
    valid_only_inside_the_roc = TRUE,
    method = "Rangayyan (2024) eq. (3.52)"
  )
}

#' Eq (3.67).  The leading 1 of the denominator is part of the equation,
#'
#' so a_k is a_1..a_M WITHOUT it; passing a vector that already carries
#' a leading 1 silently doubles the order, so the denominator actually
#' used comes back for checking.
#'
#' @param b_k Coerced to numeric by the body, with \code{as.numeric}.
#' @param a_k Optional; may be \code{NULL}. Coerced to numeric by the body, with \code{as.numeric}.
#' @param z Passed to \code{.morie_rg_polyz}.
#' @param N Optional; may be \code{NULL}. Coerced to integer by the body, with \code{as.integer}.
#' @param M Optional; may be \code{NULL}. Coerced to integer by the body, with \code{as.integer}.
#' @return A list with \code{H}, \code{z}, \code{numerator}, \code{denominator},
#' \code{N}, \code{M}, \code{leading_one_is_implicit}, \code{method}.
#' @export
IirTf <- function(b_k, a_k, z, N = NULL, M = NULL) {
  # eq (3.67).  The leading 1 of the denominator is part of the equation,
  # so a_k is a_1..a_M WITHOUT it; passing a vector that already carries
  # a leading 1 silently doubles the order, so the denominator actually
  # used comes back for checking.
  b <- as.numeric(b_k)
  a <- if (is.null(a_k)) numeric(0) else as.numeric(a_k)
  if (!length(b)) stop("need at least one numerator coefficient")
  if (!is.null(N) && as.integer(N) != length(b) - 1L) {
    stop("N must be len(b_k) - 1")
  }
  if (!is.null(M) && as.integer(M) != length(a)) {
    stop("M must be len(a_k)")
  }
  den_coefs <- c(1, a)
  den <- .morie_rg_polyz(den_coefs, z)
  if (any(Mod(den) <= 1e-300)) {
    stop("z is a pole of H(z); the transfer function is unbounded there")
  }
  H <- .morie_rg_polyz(b, z) / den
  list(
    H = if (length(H) == 1L) H[[1]] else H, z = z,
    numerator = b, denominator = den_coefs,
    N = length(b) - 1L, M = length(a),
    leading_one_is_implicit = TRUE,
    method = "Rangayyan (2024) eq. (3.67)"
  )
}

#' IirDiff
#'
#' A step of the rangayyan_filt implementation. Called by \code{IirDiffGen}.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param x Coerced to numeric by the body, with \code{as.numeric}.
#' @param b_k Coerced to numeric by the body, with \code{as.numeric}.
#' @param a_k Optional; may be \code{NULL}. Coerced to numeric by the body, with \code{as.numeric}.
#' @param y Optional; may be \code{NULL}. Coerced to numeric by the body, with \code{as.numeric}.
#' @param N Accepted by the signature and not used anywhere in the body.
#' @param M Accepted by the signature and not used anywhere in the body.
#' @param n Optional; may be \code{NULL}. Coerced to integer by the body, with \code{as.integer}.
#' @return A list with \code{y}, \code{value}, \code{index}, \code{N}, \code{M},
#' \code{recursive}, \code{feedback_is_subtracted}, \code{method}.
#' @export
IirDiff <- function(x, b_k, a_k = NULL, y = NULL, N = NULL, M = NULL,
                    n = NULL) {
  # eq (3.68).  The MINUS on the feedback term is the equation's and is
  # the one thing to get right: with a plus the recursion is a different
  # filter and usually an unstable one.
  xs <- as.numeric(x)
  b <- as.numeric(b_k)
  a <- if (is.null(a_k)) numeric(0) else as.numeric(a_k)
  if (!length(xs)) stop("need at least one input sample")
  if (!length(b)) stop("need at least one numerator coefficient")
  out <- if (is.null(y)) numeric(0) else as.numeric(y)
  for (i in seq.int(length(out) + 1L, length(xs))) {
    kb <- which(i - (seq_along(b) - 1L) >= 1L)
    acc <- .morie_fsum(b[kb] * xs[i - (kb - 1L)])
    fb <- 0
    if (length(a)) {
      ka <- which(i - seq_along(a) >= 1L)
      if (length(ka)) fb <- .morie_fsum(a[ka] * out[i - ka])
    }
    out[i] <- acc - fb
  }
  val <- NULL
  if (!is.null(n)) {
    idx <- as.integer(n)
    if (idx < 0L || idx >= length(out)) {
      stop("n is outside the computed output")
    }
    val <- out[idx + 1L]
  }
  list(
    y = out, value = val, index = n, N = length(b) - 1L, M = length(a),
    recursive = length(a) > 0L, feedback_is_subtracted = TRUE,
    method = "Rangayyan (2024) eq. (3.68)"
  )
}

#' Eq (3.72): the geometric reading of a filter.  Approaching a zero
#'
#' drives the response to nought, approaching a pole drives it up, and a
#' pole ON the evaluation point makes it unbounded -- which is why a
#' vanishing pole distance is refused rather than returning Inf.
#'
#' @param l_k Optional; may be \code{NULL}. Coerced to numeric by the body, with \code{as.numeric}.
#' @param r_k Optional; may be \code{NULL}. Coerced to numeric by the body, with \code{as.numeric}.
#' @param N Optional; may be \code{NULL}. Coerced to integer by the body, with \code{as.integer}.
#' @param M Optional; may be \code{NULL}. Coerced to integer by the body, with \code{as.integer}.
#' @return A list with \code{magnitude}, \code{zero_product}, \code{pole_product},
#' \code{n_zeros}, \code{n_poles}, \code{on_a_zero}, \code{method}.
#' @export
PzMag <- function(l_k, r_k, N = NULL, M = NULL) {
  # eq (3.72): the geometric reading of a filter.  Approaching a zero
  # drives the response to nought, approaching a pole drives it up, and a
  # pole ON the evaluation point makes it unbounded -- which is why a
  # vanishing pole distance is refused rather than returning Inf.
  ls <- if (is.null(l_k)) numeric(0) else as.numeric(l_k)
  rs <- if (is.null(r_k)) numeric(0) else as.numeric(r_k)
  if (!is.null(N) && as.integer(N) != length(ls)) {
    stop("N must be the number of zero distances")
  }
  if (!is.null(M) && as.integer(M) != length(rs)) {
    stop("M must be the number of pole distances")
  }
  if (any(c(ls, rs) < 0)) stop("a distance cannot be negative")
  if (any(rs <= 1e-300)) {
    stop(
      "a pole lies on the evaluation point; the magnitude response ",
      "is unbounded there"
    )
  }
  num <- prod(ls)
  den <- prod(rs)
  list(
    magnitude = num / den, zero_product = num, pole_product = den,
    n_zeros = length(ls), n_poles = length(rs),
    on_a_zero = any(ls <= 1e-300),
    method = "Rangayyan (2024) eq. (3.72)"
  )
}

#' Eq (3.73).  The (M - N) angle(z_0) term accounts for the zeros or
#'
#' poles at the origin that balance the orders; dropping it is easy,
#' since it vanishes when N = M, and leaves the phase wrong by a
#' multiple of angle(z_0) whenever they differ.
#'
#' @param z_0 Coerced to complex by the body, with \code{as.complex}.
#' @param alpha_k Optional; may be \code{NULL}. Coerced to numeric by the body, with
#' \code{as.numeric}.
#' @param beta_k Optional; may be \code{NULL}. Coerced to numeric by the body, with
#' \code{as.numeric}.
#' @param N Optional; may be \code{NULL}. Coerced to integer by the body, with \code{as.integer}.
#' @param M Optional; may be \code{NULL}. Coerced to integer by the body, with \code{as.integer}.
#' @return A list with \code{phase}, \code{wrapped}, \code{origin_term},
#' \code{zero_angle_sum}, \code{pole_angle_sum}, \code{z_0_angle}, \code{n_zeros},
#' \code{n_poles}, \code{origin_term_vanishes_when_orders_match}, \code{method}.
#' @export
PzPhase <- function(z_0, alpha_k, beta_k, N = NULL, M = NULL) {
  # eq (3.73).  The (M - N) angle(z_0) term accounts for the zeros or
  # poles at the origin that balance the orders; dropping it is easy,
  # since it vanishes when N = M, and leaves the phase wrong by a
  # multiple of angle(z_0) whenever they differ.
  al <- if (is.null(alpha_k)) numeric(0) else as.numeric(alpha_k)
  be <- if (is.null(beta_k)) numeric(0) else as.numeric(beta_k)
  n <- if (is.null(N)) length(al) else as.integer(N)
  m <- if (is.null(M)) length(be) else as.integer(M)
  if (n != length(al) || m != length(be)) {
    stop("N and M must match the number of angles given")
  }
  zc <- as.complex(z_0)
  if (zc == 0) stop("z_0 = 0 has no defined angle")
  ang <- Arg(zc)
  origin <- (m - n) * ang
  phase <- origin + .morie_fsum(al) - .morie_fsum(be)
  list(
    phase = phase, wrapped = (phase + pi) %% (2 * pi) - pi,
    origin_term = origin, zero_angle_sum = .morie_fsum(al),
    pole_angle_sum = .morie_fsum(be), z_0_angle = ang,
    n_zeros = n, n_poles = m,
    origin_term_vanishes_when_orders_match = n == m,
    method = "Rangayyan (2024) eq. (3.73)"
  )
}

#' Eqs (3.97)-(3.99).  With no coefficients the equal-weight boxcar is
#'
#' used.  Equal weights are the worst choice for stopband attenuation --
#' a rectangle\'s sidelobes fall off slowly -- which is what the window
#' functions of Section 3.4 exist to fix.
#'
#' @param x Coerced to numeric by the body, with \code{as.numeric}.
#' @param b_k Optional; may be \code{NULL}. Coerced to numeric by the body, with \code{as.numeric}.
#' @param N Optional; may be \code{NULL}. Coerced to integer by the body, with \code{as.integer}.
#' @param n Optional; may be \code{NULL}. Coerced to integer by the body, with \code{as.integer}.
#' @return A list with \code{y}, \code{value}, \code{index}, \code{b}, \code{N},
#' \code{settled_from}, \code{dc_gain}, \code{equal_weights}, \code{delay_samples},
#' \code{method}.
#' @export
MaFir <- function(x, b_k = NULL, N = NULL, n = NULL) {
  # eqs (3.97)-(3.99).  With no coefficients the equal-weight boxcar is
  # used.  Equal weights are the worst choice for stopband attenuation --
  # a rectangle's sidelobes fall off slowly -- which is what the window
  # functions of Section 3.4 exist to fix.
  xs <- as.numeric(x)
  if (!length(xs)) stop("need at least one sample")
  if (is.null(b_k)) {
    if (is.null(N)) stop("give either the coefficients b_k or the order N")
    m <- as.integer(N)
    if (m < 0L) stop("N must be nonnegative")
    b <- rep(1 / (m + 1), m + 1L)
  } else {
    b <- as.numeric(b_k)
    if (!length(b)) stop("need at least one coefficient")
    if (!is.null(N) && as.integer(N) != length(b) - 1L) {
      stop("N must be len(b_k) - 1")
    }
  }
  out <- vapply(seq_along(xs), function(i) {
    k <- which(i - (seq_along(b) - 1L) >= 1L)
    .morie_fsum(b[k] * xs[i - (k - 1L)])
  }, numeric(1))
  val <- NULL
  if (!is.null(n)) {
    idx <- as.integer(n)
    if (idx < 0L || idx >= length(out)) stop("n is outside the record")
    val <- out[idx + 1L]
  }
  list(
    y = out, value = val, index = n, b = b, N = length(b) - 1L,
    settled_from = length(b) - 1L, dc_gain = .morie_fsum(b),
    equal_weights = is.null(b_k),
    delay_samples = if (is.null(b_k)) (length(b) - 1) / 2 else NULL,
    method = "Rangayyan (2024) eqs. (3.97)-(3.99)"
  )
}

#' Eq (3.99): a polynomial in z^-1 with no poles away from the origin,
#'
#' so an FIR filter is stable whatever its coefficients.
#'
#' @param b_k Coerced to numeric by the body, with \code{as.numeric}.
#' @param z Passed to \code{.morie_rg_polyz}.
#' @param N Optional; may be \code{NULL}. Coerced to integer by the body, with \code{as.integer}.
#' @return A list with \code{H}, \code{z}, \code{b}, \code{N}, \code{dc_gain},
#' \code{always_stable}, \code{poles_only_at_the_origin}, \code{method}.
#' @export
MaTf <- function(b_k, z, N = NULL) {
  # eq (3.99): a polynomial in z^-1 with no poles away from the origin,
  # so an FIR filter is stable whatever its coefficients.
  b <- as.numeric(b_k)
  if (!length(b)) stop("need at least one coefficient")
  if (!is.null(N) && as.integer(N) != length(b) - 1L) {
    stop("N must be len(b_k) - 1")
  }
  H <- .morie_rg_polyz(b, z)
  list(
    H = if (length(H) == 1L) H[[1]] else H, z = z, b = b,
    N = length(b) - 1L, dc_gain = .morie_fsum(b),
    always_stable = TRUE, poles_only_at_the_origin = TRUE,
    method = "Rangayyan (2024) eq. (3.99)"
  )
}

#' Eq (3.100): y(n) = (1/4)\[x(n) + 2x(n-1) + x(n-2)\].  Three taps in the
#'
#' ratio 1:2:1.  It is a delaying smoother, not a symmetric one: the
#' output lags the input by exactly one sample.
#'
#' @param x Coerced to numeric by the body, with \code{as.numeric}.
#' @param n Optional; may be \code{NULL}. Coerced to integer by the body, with \code{as.integer}.
#' @return A list with \code{y}, \code{value}, \code{index}, \code{n}, \code{taps},
#' \code{delay_samples}, \code{settled_from}, \code{dc_gain}, \code{method}.
#' @export
HannFilt <- function(x, n = NULL) {
  # eq (3.100): y(n) = (1/4)[x(n) + 2x(n-1) + x(n-2)].  Three taps in the
  # ratio 1:2:1.  It is a delaying smoother, not a symmetric one: the
  # output lags the input by exactly one sample.
  xs <- as.numeric(x)
  if (!length(xs)) stop("need at least one sample")
  out <- vapply(seq_along(xs), function(i) {
    a <- xs[i]
    b <- if (i >= 2L) xs[i - 1L] else 0
    cc <- if (i >= 3L) xs[i - 2L] else 0
    0.25 * (a + 2 * b + cc)
  }, numeric(1))
  val <- NULL
  if (!is.null(n)) {
    idx <- as.integer(n)
    if (idx < 0L || idx >= length(out)) stop("n is outside the record")
    val <- out[idx + 1L]
  }
  list(
    y = out, value = val, index = n, n = length(out),
    taps = c(0.25, 0.5, 0.25), delay_samples = 1, settled_from = 2L,
    dc_gain = 1, method = "Rangayyan (2024) eq. (3.100)"
  )
}

#' Eq (3.101): three nonzero taps and nothing else.  The response is
#'
#' FINITE, which is what "FIR" names, and it sums to 1, so a constant
#' passes through unchanged.
#'
#' @param n Optional; may be \code{NULL}. Coerced to integer by the body, with \code{as.integer}.
#' @return A list with \code{h}, \code{value}, \code{index}, \code{n_taps}, \code{sum},
#' \code{finite}, \code{symmetric}, \code{method}.
#' @export
HannImp <- function(n = NULL) {
  # eq (3.101): three nonzero taps and nothing else.  The response is
  # FINITE, which is what "FIR" names, and it sums to 1, so a constant
  # passes through unchanged.
  taps <- c(0.25, 0.5, 0.25)
  val <- NULL
  if (!is.null(n)) {
    idx <- as.integer(n)
    val <- if (idx >= 0L && idx < 3L) taps[idx + 1L] else 0
  }
  list(
    h = taps, value = val, index = n, n_taps = 3L, sum = 1,
    finite = TRUE, symmetric = TRUE,
    method = "Rangayyan (2024) eq. (3.101)"
  )
}

#' Eq (3.102): convolution in time is multiplication in z, so the whole
#'
#' filter is one factor multiplying the input transform.  Dividing out
#' X(z) gives eq (3.103) -- which is why the transfer function does not
#' depend on the input.
#'
#' @param X Coerced to complex by the body, with \code{as.complex}.
#' @param z Coerced to complex by the body, with \code{as.complex}.
#' @return A list with \code{Y}, \code{H}, \code{X}, \code{z},
#' \code{transfer_function_is_input_independent}, \code{method}.
#' @export
HannZ <- function(X, z) {
  # eq (3.102): convolution in time is multiplication in z, so the whole
  # filter is one factor multiplying the input transform.  Dividing out
  # X(z) gives eq (3.103) -- which is why the transfer function does not
  # depend on the input.
  zc <- as.complex(z)
  if (zc == 0) stop("z = 0 is a pole of a causal transfer function")
  H <- 0.25 * (1 + 2 * zc^-1 + zc^-2)
  list(
    Y = H * as.complex(X), H = H, X = as.complex(X), z = zc,
    transfer_function_is_input_independent = TRUE,
    method = "Rangayyan (2024) eq. (3.102)"
  )
}

#' Eq (3.103): (1/4)(1 + z^-1)^2, a DOUBLE zero at z = -1, that is at
#'
#' Nyquist.  The double zero is why the response reaches nought there
#' smoothly and why the attenuation nearby is second order.
#'
#' @param z Coerced to complex by the body, with \code{as.complex}.
#' @return A list with \code{H}, \code{z}, \code{zeros}, \code{zero_multiplicity},
#' \code{zeros_at_nyquist}, \code{dc_gain}, \code{method}.
#' @export
HannTf <- function(z) {
  # eq (3.103): (1/4)(1 + z^-1)^2, a DOUBLE zero at z = -1, that is at
  # Nyquist.  The double zero is why the response reaches nought there
  # smoothly and why the attenuation nearby is second order.
  zc <- as.complex(z)
  if (any(zc == 0)) {
    stop("z = 0 is a pole of a causal transfer function")
  }
  H <- 0.25 * (1 + zc^-1)^2
  list(
    H = if (length(H) == 1L) H[[1]] else H, z = z,
    zeros = c(-1, -1), zero_multiplicity = 2L,
    zeros_at_nyquist = TRUE, dc_gain = 1,
    method = "Rangayyan (2024) eq. (3.103)"
  )
}

#' Eq (3.104): the transfer function on the unit circle, raw form
#'
#' A step of the rangayyan_filt implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param omega Coerced to numeric by the body, with \code{as.numeric}.
#' @return A list with \code{H}, \code{omega}, \code{magnitude},
#' \code{on_the_unit_circle}, \code{method}.
#' @export
HannFr <- function(omega) {
  # eq (3.104): the transfer function on the unit circle, raw form.
  w <- as.numeric(omega)
  H <- 0.25 * (1 + 2 * complex(real = cos(-w), imaginary = sin(-w)) +
    complex(real = cos(-2 * w), imaginary = sin(-2 * w)))
  scalar <- length(H) == 1L
  list(
    H = if (scalar) H[[1]] else H, omega = omega,
    magnitude = if (scalar) Mod(H[[1]]) else Mod(H),
    on_the_unit_circle = TRUE,
    method = "Rangayyan (2024) eq. (3.104)"
  )
}

#' Eq (3.105): a REAL nonnegative factor times a pure one-sample delay
#'
#' That factorization is the point -- it proves the filter has exactly
#' linear phase, so the waveform is shifted, not distorted.
#'
#' @param omega Coerced to numeric by the body, with \code{as.numeric}.
#' @return A list with \code{H}, \code{omega}, \code{envelope},
#' \code{max_difference_from_eq_3_104}, \code{agrees_with_raw_form},
#' \code{real_factor_times_a_pure_delay}, \code{linear_phase}, \code{method}.
#' @export
HannFrs <- function(omega) {
  # eq (3.105): a REAL nonnegative factor times a pure one-sample delay.
  # That factorization is the point -- it proves the filter has exactly
  # linear phase, so the waveform is shifted, not distorted.
  w <- as.numeric(omega)
  H <- 0.5 * (1 + cos(w)) * complex(real = cos(-w), imaginary = sin(-w))
  raw <- 0.25 * (1 + 2 * complex(real = cos(-w), imaginary = sin(-w)) +
    complex(real = cos(-2 * w), imaginary = sin(-2 * w)))
  gap <- max(Mod(H - raw))
  scalar <- length(H) == 1L
  list(
    H = if (scalar) H[[1]] else H, omega = omega,
    envelope = 0.5 * (1 + cos(w)),
    max_difference_from_eq_3_104 = gap,
    agrees_with_raw_form = gap <= 1e-12,
    real_factor_times_a_pure_delay = TRUE, linear_phase = TRUE,
    method = "Rangayyan (2024) eq. (3.105)"
  )
}

#' Eq (3.106): unity at DC, exactly nought at omega = pi, monotone
#'
#' between.  The book\'s absolute value is redundant -- 1 + cos is never
#' negative -- and is kept only because the book writes it.
#'
#' @param omega Coerced to numeric by the body, with \code{as.numeric}.
#' @return A list with \code{magnitude}, \code{omega}, \code{dc_gain},
#' \code{nyquist_gain}, \code{lowpass}, \code{absolute_value_is_redundant},
#' \code{method}.
#' @export
HannMag <- function(omega) {
  # eq (3.106): unity at DC, exactly nought at omega = pi, monotone
  # between.  The book's absolute value is redundant -- 1 + cos is never
  # negative -- and is kept only because the book writes it.
  w <- as.numeric(omega)
  mag <- abs(0.5 * (1 + cos(w)))
  list(
    magnitude = if (length(mag) == 1L) mag[[1]] else mag,
    omega = omega, dc_gain = 1, nyquist_gain = 0, lowpass = TRUE,
    absolute_value_is_redundant = TRUE,
    method = "Rangayyan (2024) eq. (3.106)"
  )
}

#' Eq (3.107): exactly -omega, slope -1, a constant group delay of one
#'
#' sample at every frequency.  Constant group delay is what "no phase
#' distortion" means.
#'
#' @param omega Coerced to numeric by the body, with \code{as.numeric}.
#' @return A list with \code{phase}, \code{omega}, \code{group_delay}, \code{slope},
#' \code{linear_phase}, \code{constant_group_delay}, \code{method}.
#' @export
HannPh <- function(omega) {
  # eq (3.107): exactly -omega, slope -1, a constant group delay of one
  # sample at every frequency.  Constant group delay is what "no phase
  # distortion" means.
  w <- as.numeric(omega)
  ph <- -w
  list(
    phase = if (length(ph) == 1L) ph[[1]] else ph, omega = omega,
    group_delay = 1, slope = -1, linear_phase = TRUE,
    constant_group_delay = TRUE,
    method = "Rangayyan (2024) eq. (3.107)"
  )
}

#' OsFilt
#'
#' A step of the rangayyan_filt implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param x Coerced to numeric by the body, with \code{as.numeric}.
#' @param window Coerced to integer by the body, with \code{as.integer}.
#' @param kind One of \code{"l"}, \code{"order"}, \code{"trimmed"}. Defaults to \code{"median"}.
#' @param alpha Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{0}.
#' @param weights Optional; may be \code{NULL}. Coerced to numeric by the body, with
#' \code{as.numeric}.
#' @param order Optional; may be \code{NULL}. Coerced to integer by the body, with
#' \code{as.integer}.
#' @return A list with \code{y}, \code{n}, \code{window}, \code{kind}, \code{alpha},
#' \code{trimmed_each_end}, \code{order}, \code{nonlinear}, \code{no_frequency_response},
#' \code{edges}, \code{method}.
#' @export
OsFilt <- function(x, window, kind = "median", alpha = 0, weights = NULL,
                   order = NULL) {
  # Section 3.8.  Rank the window, then take one entry or a combination:
  # min (removes high-valued impulses), max (low-valued), min/max, median
  # (the book's "most popular"), the alpha-trimmed mean, or an L-filter.
  # All are NONLINEAR, so as the book notes none can be analysed with the
  # Fourier transform and no frequency response is reported.
  xs <- as.numeric(x)
  if (!length(xs)) stop("need at least one sample")
  w <- as.integer(window)
  if (w < 1L) stop("the window must hold at least one sample")
  if (w %% 2L == 0L) {
    stop("the window must be odd so it can be centred, got ", w)
  }
  if (w > length(xs)) stop("the window is longer than the record")
  kinds <- c("min", "max", "minmax", "median", "trimmed", "l", "order")
  if (!kind %in% kinds) {
    stop("kind must be one of ", paste(kinds, collapse = ", "))
  }
  av <- as.numeric(alpha)
  if (kind == "trimmed" && !(av >= 0 && av < 0.5)) {
    stop(
      "the book writes 0 <= alpha < 0.5; at 0.5 the whole list is ",
      "trimmed away, got ", av
    )
  }
  if (kind == "l") {
    if (is.null(weights)) stop("the L-filter needs one weight per rank")
    wts <- as.numeric(weights)
    if (length(wts) != w) {
      stop("the L-filter needs ", w, " weights, one per rank")
    }
    tot <- .morie_fsum(wts)
    if (abs(tot) <= 1e-300) stop("the L-filter weights sum to zero")
  }
  if (kind == "order") {
    if (is.null(order)) stop("kind='order' needs the rank to take")
    i_ord <- as.integer(order)
    if (i_ord < 1L || i_ord > w) stop("order must lie in 1..", w)
  }
  half <- w %/% 2L

  padded <- function(seq) {
    # whole-sample symmetric reflection: the edge value is repeated, so a
    # monotone run passes through a median filter untouched
    if (half == 0L) {
      return(seq)
    }
    c(
      rev(seq[seq_len(half)]), seq,
      rev(seq[seq.int(length(seq) - half + 1L, length(seq))])
    )
  }

  rank_pass <- function(seq, take) {
    pad <- padded(seq)
    vapply(
      seq_along(seq), function(i) take(sort(pad[i:(i + w - 1L)])),
      numeric(1)
    )
  }

  out <- switch(kind,
    min = rank_pass(xs, function(r) r[1]),
    max = rank_pass(xs, function(r) r[w]),
    minmax = rank_pass(
      rank_pass(xs, function(r) r[1]),
      function(r) r[w]
    ),
    median = rank_pass(xs, function(r) r[half + 1L]),
    order = rank_pass(xs, function(r) r[i_ord]),
    trimmed = {
      drop <- as.integer(av * w)
      if (2L * drop >= w) drop <- (w - 1L) %/% 2L
      rank_pass(xs, function(r) {
        .morie_fsum(r[seq.int(drop + 1L, w - drop)]) / (w - 2L * drop)
      })
    },
    l = rank_pass(xs, function(r) .morie_fsum(wts * r) / tot)
  )

  list(
    y = out, n = length(out), window = w, kind = kind,
    alpha = if (kind == "trimmed") av else NULL,
    trimmed_each_end = if (kind == "trimmed") {
      as.integer(av * w)
    } else {
      NULL
    },
    order = if (kind == "order") i_ord else NULL,
    nonlinear = TRUE, no_frequency_response = TRUE,
    edges = "symmetric reflection",
    method = "Rangayyan (2024) Section 3.8 (order-statistic filters)"
  )
}

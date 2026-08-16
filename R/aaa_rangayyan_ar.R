# Rangayyan parametric modelling -- Levinson-Durbin, LPC/AR, all-pole
# PSD, order selection, and the pole-zero forms of Section 3.4.3.
# Mirror of the Python bsaar module.
#
# Sign convention, from eqs (7.17)-(7.18):
#   y~(n) = - sum a_k y(n-k),  e(n) = y(n) + sum a_k y(n-k)
# so A(z) = 1 + sum a_k z^-k.  The placeholder for the LPC module stated
# e(n) = y(n) - sum a_k y(n-k), the opposite sign; every function here
# follows the book, because the two conventions differ by the sign of
# every coefficient.

#' Eqs (7.37)-(7.39).  Init eps_0 = phi(0); for i = 1..P:
#'
#' gamma_i = -(1/eps_{i-1})[phi(i) + sum_j a_{i-1,j} phi(i-j)] a_{i,i} =
#' gamma_i; a_{i,j} = a_{i-1,j} + gamma_i a_{i-1,i-j} eps_i = (1 -
#' gamma_i^2) eps_{i-1} The book states the error is monotone and the
#' model is stable exactly when every |gamma_i| < 1; both are checked,
#' not trusted.
#'
#' @param acf Coerced to numeric by the body, with \code{as.numeric}.
#' @param order Optional; may be \code{NULL}. Coerced to integer by the body, with \code{as.integer}.
#' @return A list with \code{a}, \code{reflection}, \code{error}, \code{errors}, \code{gain}, \code{order}, \code{stable}, \code{monotone}, \code{normalized_error}, \code{sign_convention}, \code{method}.
#' @export
Levinson <- function(acf, order = NULL) {
  # eqs (7.37)-(7.39).  Init eps_0 = phi(0); for i = 1..P:
  #   gamma_i = -(1/eps_{i-1})[phi(i) + sum_j a_{i-1,j} phi(i-j)]
  #   a_{i,i} = gamma_i;  a_{i,j} = a_{i-1,j} + gamma_i a_{i-1,i-j}
  #   eps_i = (1 - gamma_i^2) eps_{i-1}
  # The book states the error is monotone and the model is stable
  # exactly when every |gamma_i| < 1; both are checked, not trusted.
  r <- as.numeric(acf)
  if (length(r) < 2L) stop("need phi(0) and at least one lag")
  p <- if (is.null(order)) length(r) - 1L else as.integer(order)
  if (p < 1L) stop("order must be at least 1")
  if (p > length(r) - 1L) {
    stop(sprintf(
      "order %d needs %d ACF lags, got %d", p, p + 1L,
      length(r) - 1L
    ))
  }
  if (r[1] <= 0) stop("phi(0) must be positive")
  a <- numeric(0)
  eps <- r[1]
  errors <- eps
  gammas <- numeric(0)
  for (i in seq_len(p)) {
    acc <- r[i + 1L]
    if (i > 1L) acc <- acc + .morie_fsum(a[1:(i - 1L)] * r[i:2])
    g <- -acc / eps
    gammas <- c(gammas, g)
    new <- if (i > 1L) a[1:(i - 1L)] + g * rev(a[1:(i - 1L)]) else numeric(0)
    a <- c(new, g)
    eps <- (1 - g * g) * eps
    errors <- c(errors, eps)
  }
  list(
    a = a, reflection = gammas, error = eps, errors = errors,
    gain = if (eps > 0) sqrt(eps) else 0, order = p,
    stable = all(abs(gammas) < 1),
    monotone = all(diff(errors) <= 1e-12),
    normalized_error = eps / r[1],
    sign_convention = "A(z) = 1 + sum a_k z^-k, per eq. (7.18)",
    method = "Rangayyan (2024) eqs. (7.37)-(7.39)"
  )
}

#' Eqs (7.17)-(7.18), (7.25), (7.35).  The ACF is the BIASED estimator
#'
#' (divide by N): that is what makes the Toeplitz system
#' positive-definite and hence the model stable; 1/(N-m) does not.
#'
#' @param x Coerced to numeric by the body, with \code{as.numeric}.
#' @param order Coerced to integer by the body, with \code{as.integer}.
#' @param method Compared against \code{"autocorrelation"}. Defaults to \code{"autocorrelation"}.
#' @return A list with \code{a}, \code{gain}, \code{error}, \code{reflection}, \code{acf}, \code{order}, \code{residual}, \code{residual_energy}, \code{stable}, \code{normalized_error}, \code{sign_convention}, \code{method}.
#' @export
Lpc <- function(x, order, method = "autocorrelation") {
  # eqs (7.17)-(7.18), (7.25), (7.35).  The ACF is the BIASED estimator
  # (divide by N): that is what makes the Toeplitz system
  # positive-definite and hence the model stable; 1/(N-m) does not.
  xs <- as.numeric(x)
  n <- length(xs)
  p <- as.integer(order)
  if (p < 1L) stop("order must be at least 1")
  if (n <= p) stop(sprintf("need more samples (%d) than the order (%d)", n, p))
  if (method != "autocorrelation") {
    stop(
      "only the autocorrelation method is implemented; eq. (7.40)'s ",
      "covariance method is not"
    )
  }
  acf <- vapply(0:p, function(m) {
    .morie_fsum(xs[seq_len(n - m)] * xs[seq_len(n - m) + m]) / n
  }, numeric(1))
  if (acf[1] <= 0) stop("the signal has zero energy")
  lev <- Levinson(acf, order = p)
  a <- lev$a
  resid <- vapply(seq_len(n), function(i) {
    acc <- xs[i]
    for (k in seq_len(p)) if (i - k >= 1L) acc <- acc + a[k] * xs[i - k]
    acc
  }, numeric(1))
  list(
    a = a, gain = lev$gain, error = lev$error,
    reflection = lev$reflection, acf = acf, order = p, residual = resid,
    residual_energy = .morie_fsum(resid[(p + 1L):n]^2),
    stable = lev$stable, normalized_error = lev$normalized_error,
    sign_convention = "A(z) = 1 + sum a_k z^-k, per eq. (7.18)",
    method = "Rangayyan (2024) eqs. (7.17)-(7.18), (7.25), (7.35)"
  )
}

#' Inverting eq (7.18): y(n) = G e(n) - sum a_k y(n-k).  The minus
#'
#' follows from A(z) = 1 + sum a_k z^-k.  Coefficients from the other
#' convention give a filter with different poles, usually unstable, so
#' divergence is reported rather than a wall of infinities returned.
#'
#' @param a Coerced to numeric by the body, with \code{as.numeric}.
#' @param excitation Coerced to numeric by the body, with \code{as.numeric}.
#' @param gain Numeric; combined arithmetically in the body. Defaults to \code{1}.
#' @param initial Optional; may be \code{NULL}. Coerced to numeric by the body, with \code{as.numeric}.
#' @return A list with \code{y}, \code{n}, \code{order}, \code{gain}, \code{diverged}, \code{sign_convention}, \code{method}.
#' @export
LpcSynth <- function(a, excitation, gain = 1, initial = NULL) {
  # Inverting eq (7.18): y(n) = G e(n) - sum a_k y(n-k).  The minus
  # follows from A(z) = 1 + sum a_k z^-k.  Coefficients from the other
  # convention give a filter with different poles, usually unstable, so
  # divergence is reported rather than a wall of infinities returned.
  ak <- as.numeric(a)
  e <- as.numeric(excitation)
  if (!length(ak)) stop("need at least one AR coefficient")
  if (!length(e)) stop("need an excitation sequence")
  p <- length(ak)
  hist <- if (is.null(initial)) numeric(p) else as.numeric(initial)
  if (length(hist) != p) stop(sprintf("initial state must hold %d samples", p))
  y <- numeric(0)
  limit <- 1e12 * (1 + max(abs(e)))
  diverged <- FALSE
  for (v in e) {
    acc <- gain * v - .morie_fsum(ak * hist)
    if (!(abs(acc) < limit)) {
      diverged <- TRUE
      y <- c(y, if (acc > 0) Inf else -Inf)
      break
    }
    y <- c(y, acc)
    hist <- c(acc, hist[-p])
  }
  list(
    y = y, n = length(y), order = p, gain = as.numeric(gain),
    diverged = diverged,
    sign_convention = "y(n) = G e(n) - sum a_k y(n-k)",
    method = "Rangayyan (2024) Section 7.5 (all-pole synthesis)"
  )
}

#' Section 7.5: S(f) = G^2 / |A(exp(-j 2 pi f / fs))|^2, a smooth
#'
#' spectrum from P+1 parameters.  That smoothness is also the trap: the
#' model can only make P/2 peaks, so extra resonances merge silently.
#'
#' @param x Passed to \code{Lpc}.
#' @param order Passed to \code{Lpc}.
#' @param fs Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{1}.
#' @param nfreq Coerced to integer by the body, with \code{as.integer}. Defaults to \code{256}.
#' @return The value of \code{fit}, as built in the body.
#' @export
ArFit <- function(x, order, fs = 1, nfreq = 256) {
  # Section 7.5: S(f) = G^2 / |A(exp(-j 2 pi f / fs))|^2, a smooth
  # spectrum from P+1 parameters.  That smoothness is also the trap: the
  # model can only make P/2 peaks, so extra resonances merge silently.
  fit <- Lpc(x, order)
  a <- fit$a
  g2 <- fit$error
  fsv <- as.numeric(fs)
  if (fsv <= 0) stop("fs must be positive")
  k <- as.integer(nfreq)
  if (k < 2L) stop("nfreq must be at least 2")
  freqs <- 0.5 * fsv * (0:(k - 1L)) / (k - 1L)
  psd <- vapply(freqs, function(f) {
    w <- 2 * pi * f / fsv
    j <- seq_along(a)
    re <- 1 + .morie_fsum(a * cos(-w * j))
    im <- .morie_fsum(a * sin(-w * j))
    d <- re * re + im * im
    if (d > 0) g2 / d else Inf
  }, numeric(1))
  fit$freqs <- freqs
  fit$psd <- psd
  fit$fs <- fsv
  fit$max_peaks <- length(a) %/% 2L
  fit$method <- "Rangayyan (2024) Section 7.5 (all-pole PSD)"
  fit
}

#' Akaike (1970): FPE(p) = sigma_p^2 (N+p+1)/(N-p-1).  The residual
#'
#' variance falls monotonically with p (eq 7.39), so without a penalty
#' the criterion would always pick the largest order offered. Rangayyan
#' gives AIC at eq (7.60); FPE is not printed in the book.
#'
#' @param errors Coerced to numeric by the body, with \code{as.numeric}.
#' @param n_samples Coerced to integer by the body, with \code{as.integer}.
#' @return A list with \code{order}, \code{criterion}, \code{n}, \code{start_order}, \code{method}.
#' @export
FpeOrder <- function(errors, n_samples) {
  # Akaike (1970): FPE(p) = sigma_p^2 (N+p+1)/(N-p-1).  The residual
  # variance falls monotonically with p (eq 7.39), so without a penalty
  # the criterion would always pick the largest order offered.
  # Rangayyan gives AIC at eq (7.60); FPE is not printed in the book.
  eps <- as.numeric(errors)
  if (!length(eps)) stop("need at least one error value")
  if (any(eps <= 0)) stop("residual variances must be positive")
  n <- as.integer(n_samples)
  if (n <= length(eps) + 1L) {
    stop("N must exceed the largest order by more than 1")
  }
  i <- seq_along(eps)
  crit <- eps * (n + i + 1) / (n - i - 1)
  list(
    order = which.min(crit), criterion = crit, n = n, start_order = 1L,
    method = paste(
      "Akaike (1970) FPE; Rangayyan (2024) Section 7.5.2",
      "gives AIC at eq. (7.60) instead"
    )
  )
}

#' Rissanen (1978): MDL(p) = N log(sigma_p^2) + p log(N).  The penalty
#'
#' per parameter is log(N), larger than AIC\'s 2 for any N > 7, so MDL
#' picks the same order or a lower one -- and unlike AIC it is
#' consistent.  AIC is returned alongside for comparison.
#'
#' @param errors Coerced to numeric by the body, with \code{as.numeric}.
#' @param n_samples Coerced to integer by the body, with \code{as.integer}.
#' @return A list with \code{order}, \code{criterion}, \code{aic}, \code{aic_order}, \code{n}, \code{start_order}, \code{penalty_per_parameter}, \code{stricter_than_aic}, \code{method}.
#' @export
MdlOrder <- function(errors, n_samples) {
  # Rissanen (1978): MDL(p) = N log(sigma_p^2) + p log(N).  The penalty
  # per parameter is log(N), larger than AIC's 2 for any N > 7, so MDL
  # picks the same order or a lower one -- and unlike AIC it is
  # consistent.  AIC is returned alongside for comparison.
  eps <- as.numeric(errors)
  if (!length(eps)) stop("need at least one error value")
  if (any(eps <= 0)) stop("residual variances must be positive")
  n <- as.integer(n_samples)
  if (n < 2L) stop("N must be at least 2")
  i <- seq_along(eps)
  mdl <- n * log(eps) + i * log(n)
  aic <- n * log(eps) + 2 * i
  list(
    order = which.min(mdl), criterion = mdl, aic = aic,
    aic_order = which.min(aic), n = n, start_order = 1L,
    penalty_per_parameter = log(n), stricter_than_aic = log(n) > 2,
    method = paste(
      "Rissanen (1978) MDL; Rangayyan (2024) Section 7.5.2",
      "gives AIC at eq. (7.60) instead"
    )
  )
}

#' Eq (3.69): H(z) = prod (1 - z_k z^-1) / prod (1 - p_k z^-1).  A pole
#'
#' on the unit circle makes H undefined there; outside it, a causal
#' system is unstable.  Both are reported.
#'
#' @param zeros Coerced to complex by the body, with \code{as.complex}.
#' @param poles Coerced to complex by the body, with \code{as.complex}.
#' @param z Optional; may be \code{NULL}. Coerced to complex by the body, with \code{as.complex}.
#' @param gain Coerced to complex by the body, with \code{as.complex}. Defaults to \code{1}.
#' @return The value of \code{out}, as built in the body.
#' @export
PzForm <- function(zeros, poles, z = NULL, gain = 1) {
  # eq (3.69): H(z) = prod (1 - z_k z^-1) / prod (1 - p_k z^-1).  A pole
  # on the unit circle makes H undefined there; outside it, a causal
  # system is unstable.  Both are reported.
  zs <- as.complex(zeros)
  ps <- as.complex(poles)
  out <- list(
    zeros = zs, poles = ps, n_zeros = length(zs),
    n_poles = length(ps), gain = as.complex(gain),
    stable = all(Mod(ps) < 1),
    poles_on_unit_circle = ps[abs(Mod(ps) - 1) < 1e-12],
    method = "Rangayyan (2024) eq. (3.69)"
  )
  if (is.null(z)) {
    out$H <- NULL
    return(out)
  }
  pts <- as.complex(z)
  if (any(pts == 0)) {
    stop("the z^-1 form of eq. (3.69) is undefined at z = 0; use PzFormZ")
  }
  vals <- vapply(pts, function(zv) {
    num <- as.complex(gain)
    for (zk in zs) num <- num * (1 - zk / zv)
    den <- as.complex(1)
    for (pk in ps) den <- den * (1 - pk / zv)
    if (den == 0) stop("z coincides with a pole of H")
    num / den
  }, complex(1))
  one <- length(pts) == 1L
  out$H <- if (one) vals[[1]] else vals
  out$z <- if (one) pts[[1]] else pts
  out
}

#' Eq (3.70): H(z) = z^(M-N) prod (z - z_k) / prod (z - p_k), the same
#'
#' function as eq (3.69) rewritten in z.  The z^(M-N) factor is exactly
#' what the change of variable produces; dropping it multiplies H by a
#' pure delay, invisible in the magnitude and fatal to the phase.
#'
#' @param zeros Coerced to complex by the body, with \code{as.complex}.
#' @param poles Coerced to complex by the body, with \code{as.complex}.
#' @param z Optional; may be \code{NULL}. Coerced to complex by the body, with \code{as.complex}.
#' @param gain Coerced to complex by the body, with \code{as.complex}. Defaults to \code{1}.
#' @return The value of \code{out}, as built in the body.
#' @export
PzFormZ <- function(zeros, poles, z = NULL, gain = 1) {
  # eq (3.70): H(z) = z^(M-N) prod (z - z_k) / prod (z - p_k), the same
  # function as eq (3.69) rewritten in z.  The z^(M-N) factor is exactly
  # what the change of variable produces; dropping it multiplies H by a
  # pure delay, invisible in the magnitude and fatal to the phase.
  zs <- as.complex(zeros)
  ps <- as.complex(poles)
  n <- length(zs)
  m <- length(ps)
  out <- list(
    zeros = zs, poles = ps, exponent = m - n,
    gain = as.complex(gain), stable = all(Mod(ps) < 1),
    method = "Rangayyan (2024) eq. (3.70)"
  )
  if (is.null(z)) {
    out$H <- NULL
    return(out)
  }
  pts <- as.complex(z)
  vals <- vapply(pts, function(zv) {
    num <- as.complex(gain) * zv^(m - n)
    for (zk in zs) num <- num * (zv - zk)
    den <- as.complex(1)
    for (pk in ps) den <- den * (zv - pk)
    if (den == 0) stop("z coincides with a pole of H")
    num / den
  }, complex(1))
  other <- vapply(
    pts, function(zv) PzForm(zs, ps, z = zv, gain = gain)$H,
    complex(1)
  )
  gap <- max(Mod(vals - other))
  scale <- max(Mod(vals))
  if (scale == 0) scale <- 1
  one <- length(pts) == 1L
  out$H <- if (one) vals[[1]] else vals
  out$H_from_eq369 <- if (one) other[[1]] else other
  out$max_difference <- gap
  out$agrees_with_eq369 <- gap <= 1e-9 * scale
  out$z <- if (one) pts[[1]] else pts
  out
}

#' Eqs (3.71)-(3.73): on the unit circle the magnitude is the product
#'
#' of distances to the zeros over the product of distances to the poles,
#' and the phase is (M-N) angle(z0) plus the zero angles minus the pole
#' angles.  A zero ON the circle sends one distance to zero -- a
#' spectral null; a pole near it makes one distance small -- a
#' resonance.  The distances are returned so that reading can be made.
#'
#' @param zeros Coerced to complex by the body, with \code{as.complex}.
#' @param poles Coerced to complex by the body, with \code{as.complex}.
#' @param omega Coerced to numeric by the body, with \code{as.numeric}.
#' @param gain Numeric; passed to \code{abs}. Defaults to \code{1}.
#' @return A list with \code{H}, \code{magnitude}, \code{phase}, \code{zero_distances}, \code{pole_distances}, \code{omega}, \code{magnitude_matches_product}, \code{method}.
#' @export
PzResp <- function(zeros, poles, omega, gain = 1) {
  # eqs (3.71)-(3.73): on the unit circle the magnitude is the product
  # of distances to the zeros over the product of distances to the
  # poles, and the phase is (M-N) angle(z0) plus the zero angles minus
  # the pole angles.  A zero ON the circle sends one distance to zero --
  # a spectral null; a pole near it makes one distance small -- a
  # resonance.  The distances are returned so that reading can be made.
  zs <- as.complex(zeros)
  ps <- as.complex(poles)
  n <- length(zs)
  m <- length(ps)
  ws <- as.numeric(omega)
  H <- complex(length(ws))
  mags <- numeric(length(ws))
  phases <- numeric(length(ws))
  dist_z <- vector("list", length(ws))
  dist_p <- vector("list", length(ws))
  for (i in seq_along(ws)) {
    z0 <- complex(real = cos(ws[i]), imaginary = sin(ws[i]))
    lk <- Mod(z0 - zs)
    rk <- Mod(z0 - ps)
    if (any(rk == 0)) stop("a pole lies exactly on the evaluation point")
    num <- as.complex(gain) * z0^(m - n)
    for (zk in zs) num <- num * (z0 - zk)
    den <- as.complex(1)
    for (pk in ps) den <- den * (z0 - pk)
    H[i] <- num / den
    mags[i] <- abs(gain) * prod(lk) / prod(rk)
    phases[i] <- (m - n) * ws[i] + sum(Arg(z0 - zs)) - sum(Arg(z0 - ps))
    dist_z[[i]] <- lk
    dist_p[[i]] <- rk
  }
  gap <- max(abs(Mod(H) - mags))
  one <- length(ws) == 1L
  list(
    H = if (one) H[[1]] else H,
    magnitude = if (one) mags[[1]] else mags,
    phase = if (one) phases[[1]] else phases,
    zero_distances = if (one) dist_z[[1]] else dist_z,
    pole_distances = if (one) dist_p[[1]] else dist_p,
    omega = if (one) ws[[1]] else ws,
    magnitude_matches_product = gap <= 1e-9 * (1 + max(mags)),
    method = "Rangayyan (2024) eqs. (3.71)-(3.73)"
  )
}

#' Eqs (3.67), (3.69): zeros are the roots of the numerator, poles the
#'
#' roots of the denominator.  The denominator is in the book\'s
#' normalized form, so pass `a` WITHOUT the leading 1.
#'
#' @param b Coerced to numeric by the body, with \code{as.numeric}.
#' @param a Optional; may be \code{NULL}. Coerced to numeric by the body, with \code{as.numeric}.
#' @return A list with \code{zeros}, \code{poles}, \code{n_zeros}, \code{n_poles}, \code{stable}, \code{minimum_phase}, \code{zeros_on_unit_circle}, \code{method}.
#' @export
PoleZero <- function(b, a = NULL) {
  # eqs (3.67), (3.69): zeros are the roots of the numerator, poles the
  # roots of the denominator.  The denominator is in the book's
  # normalized form, so pass `a` WITHOUT the leading 1.
  bs <- as.numeric(b)
  if (!length(bs)) stop("need at least one numerator coefficient")
  as_ <- if (is.null(a)) numeric(0) else as.numeric(a)
  roots_of <- function(coeffs) {
    cc <- coeffs
    while (length(cc) > 1L && cc[length(cc)] == 0) cc <- cc[-length(cc)]
    if (length(cc) < 2L) {
      return(complex(0))
    }
    # sum_k c_k z^-k = 0  <=>  sum_k c_k z^(deg-k) = 0; polyroot takes
    # ascending powers of z, so the coefficient vector is reversed
    polyroot(rev(cc))
  }
  zeros <- roots_of(bs)
  poles <- roots_of(c(1, as_))
  list(
    zeros = zeros, poles = poles, n_zeros = length(zeros),
    n_poles = length(poles), stable = all(Mod(poles) < 1),
    minimum_phase = all(Mod(zeros) < 1),
    zeros_on_unit_circle = zeros[abs(Mod(zeros) - 1) < 1e-9],
    method = "Rangayyan (2024) eqs. (3.67), (3.69)"
  )
}

#' Section 7.7: H(z) = B(z)/A(z), needed when the signal has spectral
#'
#' nulls as well as resonances -- an all-pole model can only make peaks.
#' Fitted in two stages (AR from the ACF, MA from the residual ACF),
#' which is NOT joint maximum likelihood and is biased when the zeros
#' sit close to the poles.  The stage structure is stated, not presented
#' as an optimal fit.
#'
#' @param x Coerced to numeric by the body, with \code{as.numeric}.
#' @param p Coerced to integer by the body, with \code{as.integer}.
#' @param q Coerced to integer by the body, with \code{as.integer}.
#' @param fs Accepted by the signature and not used anywhere in the body. Defaults to \code{1}.
#' @return A list with \code{a}, \code{b}, \code{p}, \code{q}, \code{gain}, \code{poles}, \code{zeros}, \code{stable}, \code{ar_error}, \code{two_stage}, \code{method}.
#' @export
ArmaFit <- function(x, p, q, fs = 1) {
  # Section 7.7: H(z) = B(z)/A(z), needed when the signal has spectral
  # nulls as well as resonances -- an all-pole model can only make
  # peaks.  Fitted in two stages (AR from the ACF, MA from the residual
  # ACF), which is NOT joint maximum likelihood and is biased when the
  # zeros sit close to the poles.  The stage structure is stated, not
  # presented as an optimal fit.
  xs <- as.numeric(x)
  pi_ <- as.integer(p)
  qi <- as.integer(q)
  if (pi_ < 1L) stop("the AR order p must be at least 1")
  if (qi < 0L) stop("the MA order q cannot be negative")
  ar <- Lpc(xs, pi_)
  resid <- ar$residual[(pi_ + 1L):length(ar$residual)]
  n <- length(resid)
  if (n <= qi) {
    stop(sprintf(
      "too few residual samples (%d) for MA order %d",
      n, qi
    ))
  }
  if (qi == 0L) {
    b <- ar$gain
  } else {
    long_order <- min(4L * qi, n - 1L)
    acf_long <- vapply(
      0:long_order, function(m) {
        .morie_fsum(resid[seq_len(n - m)] * resid[seq_len(n - m) + m]) / n
      },
      numeric(1)
    )
    inner <- Levinson(acf_long, order = long_order)
    b <- c(ar$gain, Levinson(c(1, inner$a[seq_len(qi)]), order = qi)$a)
  }
  pz <- PoleZero(b, ar$a)
  list(
    a = ar$a, b = b, p = pi_, q = qi, gain = ar$gain,
    poles = pz$poles, zeros = pz$zeros, stable = pz$stable,
    ar_error = ar$error, two_stage = TRUE,
    method = paste(
      "Rangayyan (2024) Section 7.7 (pole-zero model),",
      "fitted AR-then-MA rather than jointly"
    )
  )
}

#' Chapter 7: the poles of an all-pole PCG model track the resonances
#'
#' of S1 and S2.  A pole p gives a resonance at (fs/2pi) Arg(p) with
#' bandwidth -(fs/pi) log|p|.  Only the upper half plane is reported:
#' for a real signal the poles are conjugate pairs, and listing both
#' halves would double-count every resonance.
#'
#' @param x Coerced to numeric by the body, with \code{as.numeric}.
#' @param fs Coerced to numeric by the body, with \code{as.numeric}.
#' @param order Optional; may be \code{NULL}. Coerced to integer by the body, with \code{as.integer}.
#' @param segment Optional; may be \code{NULL}. A vector; indexed elementwise.
#' @return The value of \code{fit}, as built in the body.
#' @export
PcgAr <- function(x, fs, order = NULL, segment = NULL) {
  # Chapter 7: the poles of an all-pole PCG model track the resonances
  # of S1 and S2.  A pole p gives a resonance at (fs/2pi) Arg(p) with
  # bandwidth -(fs/pi) log|p|.  Only the upper half plane is reported:
  # for a real signal the poles are conjugate pairs, and listing both
  # halves would double-count every resonance.
  xs <- as.numeric(x)
  fsv <- as.numeric(fs)
  if (fsv <= 0) stop("fs must be positive")
  if (!is.null(segment)) xs <- xs[(segment[1] + 1L):segment[2]]
  if (length(xs) < 16L) stop("need at least sixteen samples in the segment")
  p <- if (!is.null(order)) {
    as.integer(order)
  } else {
    max(4L, as.integer(round(2 + fsv / 1000)))
  }
  fit <- ArFit(xs, p, fs = fsv)
  pz <- PoleZero(1, fit$a)
  keep <- Arg(pz$poles) > 0 & Mod(pz$poles) > 0
  res <- lapply(pz$poles[keep], function(pole) {
    list(
      frequency = fsv * Arg(pole) / (2 * pi),
      bandwidth = -fsv * log(Mod(pole)) / pi,
      radius = Mod(pole), pole = pole
    )
  })
  res <- res[order(vapply(res, function(d) d$frequency, numeric(1)))]
  fit$poles <- pz$poles
  fit$resonances <- res
  fit$order <- p
  fit$stable <- pz$stable
  fit$fs <- fsv
  fit$method <- "Rangayyan (2024) Chapter 7 (AR modelling of the PCG)"
  fit
}

#' The RR series is unevenly sampled by construction, so it is
#'
#' resampled onto a uniform grid (4 Hz, comfortably above the 0.4 Hz top
#' of the HF band) and modelled all-pole per Section 7.5.  Band edges
#' follow the Task Force of the ESC and NASPE (1996): VLF 0.003-0.04, LF
#' 0.04-0.15, HF 0.15-0.40 Hz. The mean is removed first: a mean RR of
#' ~0.8 s dwarfs the variability and would leak a huge DC pole into the
#' VLF band.
#'
#' @param rr Coerced to numeric by the body, with \code{as.numeric}.
#' @param order Coerced to integer by the body, with \code{as.integer}. Defaults to \code{16}.
#' @param fs Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{4}.
#' @param nfreq Coerced to integer by the body, with \code{as.integer}. Defaults to \code{512}.
#' @return The value of \code{fit}, as built in the body.
#' @export
HrvAr <- function(rr, order = 16, fs = 4, nfreq = 512) {
  # The RR series is unevenly sampled by construction, so it is
  # resampled onto a uniform grid (4 Hz, comfortably above the 0.4 Hz
  # top of the HF band) and modelled all-pole per Section 7.5.  Band
  # edges follow the Task Force of the ESC and NASPE (1996):
  #   VLF 0.003-0.04, LF 0.04-0.15, HF 0.15-0.40 Hz.
  # The mean is removed first: a mean RR of ~0.8 s dwarfs the
  # variability and would leak a huge DC pole into the VLF band.
  intervals <- as.numeric(rr)
  if (length(intervals) < 8L) stop("need at least eight RR intervals")
  if (any(intervals <= 0)) stop("RR intervals must be positive")
  fsv <- as.numeric(fs)
  if (fsv <= 0) stop("fs must be positive")
  beats <- cumsum(intervals)
  duration <- beats[length(beats)] - beats[1]
  if (duration <= 0) stop("the RR series has zero duration")
  n <- max(16L, as.integer(duration * fsv))
  grid <- beats[1] + (0:(n - 1L)) / fsv
  series <- stats_free_interp(beats, intervals, grid)
  mu <- .morie_fsum(series) / length(series)
  series <- series - mu
  p <- min(as.integer(order), length(series) - 1L)
  fit <- ArFit(series, p, fs = fsv, nfreq = as.integer(nfreq))
  bands <- list(vlf = c(0.003, 0.04), lf = c(0.04, 0.15), hf = c(0.15, 0.40))
  df <- if (length(fit$freqs) > 1L) fit$freqs[2] - fit$freqs[1] else 0
  power <- lapply(bands, function(b) {
    .morie_fsum(fit$psd[fit$freqs >= b[1] & fit$freqs < b[2]] * df)
  })
  total <- power$vlf + power$lf + power$hf
  fit$mean_rr <- mu
  fit$resampled <- series
  fit$resample_fs <- fsv
  fit$vlf <- power$vlf
  fit$lf <- power$lf
  fit$hf <- power$hf
  fit$total_power <- total
  fit$lf_hf_ratio <- if (power$hf > 0) power$lf / power$hf else NULL
  fit$lf_nu <- if ((power$lf + power$hf) > 0) {
    100 * power$lf / (power$lf + power$hf)
  } else {
    NULL
  }
  fit$bands <- bands
  fit$order <- p
  fit$method <- paste(
    "Rangayyan (2024) Section 7.5 AR model; bands per",
    "Task Force of the ESC and NASPE (1996)"
  )
  fit
}

#' Piecewise-linear interpolation onto `grid`, written out rather than
#'
#' delegated so the R and Python arms resample identically
#'
#' @param beats A vector; its length is taken and its elements indexed.
#' @param values A vector; its length is taken and its elements indexed.
#' @param grid A vector; its length is taken and its elements indexed.
#' @return The value of \code{out}, as built in the body.
#' @export
stats_free_interp <- function(beats, values, grid) {
  # piecewise-linear interpolation onto `grid`, written out rather than
  # delegated so the R and Python arms resample identically
  out <- numeric(length(grid))
  j <- 1L
  nb <- length(beats)
  for (i in seq_along(grid)) {
    tv <- grid[i]
    while (j < nb - 1L && beats[j + 1L] < tv) j <- j + 1L
    t0 <- beats[j]
    t1 <- beats[j + 1L]
    v0 <- values[j]
    v1 <- values[min(j + 1L, length(values))]
    w <- if (t1 == t0) 0 else (tv - t0) / (t1 - t0)
    out[i] <- v0 + w * (v1 - v0)
  }
  out
}

#' LF/HF from the AR model PSD.  Often called a sympathovagal balance;
#'
#' that reading is contested -- HF is reasonably vagal, but LF reflects
#' both branches plus the baroreflex.  The components are returned
#' individually and in normalized units so no interpretation has to rest
#' on the ratio alone.
#'
#' @param rr Passed to \code{HrvAr}.
#' @param order Passed to \code{HrvAr}. Defaults to \code{16}.
#' @param fs Passed to \code{HrvAr}. Defaults to \code{4}.
#' @return A list with \code{lf_hf_ratio}, \code{lf}, \code{hf}, \code{vlf}, \code{total_power}, \code{lf_nu}, \code{hf_nu}, \code{order}, \code{bands}, \code{interpretation_caveat}, \code{method}.
#' @export
HrvRatio <- function(rr, order = 16, fs = 4) {
  # LF/HF from the AR model PSD.  Often called a sympathovagal balance;
  # that reading is contested -- HF is reasonably vagal, but LF reflects
  # both branches plus the baroreflex.  The components are returned
  # individually and in normalized units so no interpretation has to
  # rest on the ratio alone.
  r <- HrvAr(rr, order = order, fs = fs)
  list(
    lf_hf_ratio = r$lf_hf_ratio, lf = r$lf, hf = r$hf, vlf = r$vlf,
    total_power = r$total_power, lf_nu = r$lf_nu,
    hf_nu = if (!is.null(r$lf_nu)) 100 - r$lf_nu else NULL,
    order = r$order, bands = r$bands,
    interpretation_caveat = paste(
      "LF reflects both autonomic branches",
      "and the baroreflex; the ratio is not",
      "a clean index of sympathovagal balance"
    ),
    method = r$method
  )
}

# pre-policy spellings
morie_levinson_durbin <- Levinson
morie_lpc_analysis <- Lpc
morie_lpc_synthesis <- LpcSynth
morie_parametric_sysid <- ArFit
morie_ar_order_fpe <- FpeOrder
morie_ar_order_mdl <- MdlOrder
morie_ch3_pole_zero_factored <- PzForm
morie_ch3_pole_zero_factored_z <- PzFormZ
morie_ch3_response_from_pole_zero <- PzResp
morie_pole_zero_plot <- PoleZero
morie_pole_zero_model <- ArmaFit
morie_pcg_ar_model <- PcgAr
morie_hrv_ar_model <- HrvAr
morie_hrv_ar_ratio <- HrvRatio

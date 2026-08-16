# The 14 Rangayyan methods that were still stubs inside four modules
# reported complete.  Mirror of the Python bsaadapt / bsacorr / bsasig /
# bsastat additions.  Equations verified in the PDF: 8.30, 8.31, 4.24-4.25.

# ---------------------------------------------------------------- bsaadapt
#' Bsaadapt
#'
#' A step of the rangayyan_ccf implementation. Called by \code{.morie_rg_H}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param seg A vector; its length is taken and its elements indexed.
#' @param p A count; the body uses it as \code{seq_len(...)}.
#' @return A numeric value.
#' @export
.morie_rg_tse <- function(seg, p) {
  # AR prediction-error energy, eq (7.19), by Levinson-Durbin.
  k <- length(seg)
  r <- vapply(0:p, function(t) {
    if (t >= k) {
      return(0)
    }
    .morie_fsum(seg[seq_len(k - t)] * seg[seq_len(k - t) + t]) / k
  }, numeric(1))
  if (r[1L] <= 0) {
    return(0)
  }
  a <- c(1, rep(0, p))
  e <- r[1L]
  for (i in seq_len(p)) {
    acc <- .morie_fsum(a[seq_len(i)] * r[i - seq_len(i) + 2L])
    kk <- if (e > 0) -acc / e else 0
    new <- a
    if (i > 1L) for (j in seq_len(i - 1L)) new[j + 1L] <- a[j + 1L] + kk * a[i - j + 1L]
    new[i + 1L] <- kk
    a <- new
    e <- e * (1 - kk * kk)
    if (e <= 0) {
      return(0)
    }
  }
  e * k
}

#' .morie_rg_H
#'
#' A step of the rangayyan_ccf implementation. Called by \code{Glr}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param seg A vector; its length is taken.
#' @param p Passed to \code{.morie_rg_tse}.
#' @return A numeric value.
#' @export
.morie_rg_H <- function(seg, p) {
  L <- length(seg)
  t <- .morie_rg_tse(seg, p)
  if (L <= 0 || t <= 0) {
    return(0)
  }
  L * log(t / L)
}

#' Eqs (8.30)-(8.31), after Appel and v. Brandt.  The reference window
#'
#' GROWS from the start of the segment -- that is what separates GLR
#' from the SEM and ACF methods.
#'
#' @param x Coerced to numeric by the body, with \code{as.numeric}.
#' @param m Coerced to integer by the body, with \code{as.integer}.
#' @param n Optional; may be \code{NULL}. Coerced to integer by the body, with \code{as.integer}.
#' @param order Coerced to integer by the body, with \code{as.integer}. Defaults to \code{4}.
#' @return A list with \code{d}, \code{h_pooled}, \code{h_reference}, \code{h_test}, \code{m}, \code{n}, \code{order}, \code{n_reference}, \code{n_test}, \code{reference_window_grows}, \code{near_zero_when_one_model_explains_both}, \code{method}.
#' @export
Glr <- function(x, m, n = NULL, order = 4) {
  # eqs (8.30)-(8.31), after Appel and v. Brandt.  The reference window
  # GROWS from the start of the segment -- that is what separates GLR from
  # the SEM and ACF methods.
  xs <- as.numeric(x)
  N <- length(xs)
  mv <- as.integer(m)
  nv <- if (is.null(n)) N else as.integer(n)
  p <- as.integer(order)
  if (p < 1L) stop("order must be at least 1")
  if (!(mv >= 2L && mv <= nv && nv <= N)) {
    stop("need 2 <= m <= n <= length(x)")
  }
  if (mv - 1L <= p || nv - mv + 1L <= p) {
    stop("each window must hold more samples than the AR order")
  }
  ref <- xs[seq_len(mv - 1L)]
  test <- xs[mv:nv]
  pooled <- xs[seq_len(nv)]
  h_ref <- .morie_rg_H(ref, p)
  h_test <- .morie_rg_H(test, p)
  h_pool <- .morie_rg_H(pooled, p)
  d <- h_pool - (h_ref + h_test)
  list(
    d = d, h_pooled = h_pool, h_reference = h_ref, h_test = h_test,
    m = mv, n = nv, order = p,
    n_reference = length(ref), n_test = length(test),
    reference_window_grows = TRUE,
    near_zero_when_one_model_explains_both = abs(d) < 1e-6,
    method = paste(
      "Rangayyan (2024) eqs. (8.30)-(8.31), after Appel",
      "and v. Brandt"
    )
  )
}

#' EegAdapt
#'
#' A step of the rangayyan_ccf implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param x Coerced to numeric by the body, with \code{as.numeric}.
#' @param fs Coerced to numeric by the body, with \code{as.numeric}.
#' @param window Optional; may be \code{NULL}. Coerced to integer by the body, with \code{as.integer}.
#' @param step Optional; may be \code{NULL}. Coerced to integer by the body, with \code{as.integer}.
#' @param order Coerced to integer by the body, with \code{as.integer}. Defaults to \code{4}.
#' @param threshold Optional; may be \code{NULL}. Coerced to numeric by the body, with \code{as.numeric}.
#' @return A list with \code{d}, \code{d_fixed_reference}, \code{times}, \code{boundaries}, \code{n_boundaries}, \code{threshold}, \code{median}, \code{mad}, \code{window}, \code{step}, \code{order}, \code{fs}, \code{reference_restarts_at_boundaries}, \code{robust_threshold}, \code{method}.
#' @export
EegAdapt <- function(x, fs, window = NULL, step = NULL, order = 4,
                     threshold = NULL) {
  # Section 8.5.3.  On a boundary the reference window RESTARTS; without
  # that, every window after the first change looks like a boundary.
  xs <- as.numeric(x)
  fsv <- as.numeric(fs)
  if (fsv <= 0) stop("fs must be positive")
  N <- length(xs)
  p <- as.integer(order)
  w <- if (!is.null(window)) {
    as.integer(window)
  } else {
    max(4L * (p + 1L), as.integer(0.5 * fsv))
  }
  hop <- if (!is.null(step)) as.integer(step) else max(1L, w %/% 4L)
  if (w > N) stop("the window is longer than the record")
  if (w <= p) stop("the window must hold more samples than the order")
  if (hop < 1L) stop("step must be at least one sample")

  dists <- numeric(0)
  times <- numeric(0)
  start <- 0L
  pos <- w
  while (pos + w <= N) {
    seg <- xs[(start + 1L):(pos + w)]
    mrel <- pos - start + 1L
    d <- if (mrel - 1L > p && w > p) {
      Glr(seg, mrel, length(seg), order = p)$d
    } else {
      0
    }
    dists <- c(dists, d)
    times <- c(times, pos / fsv)
    pos <- pos + hop
  }
  if (!length(dists)) {
    stop("the record is too short for even one test window")
  }
  med <- stats::median(dists)
  mad <- stats::median(abs(dists - med))
  thr <- if (!is.null(threshold)) {
    as.numeric(threshold)
  } else {
    med + 3 * 1.4826 * mad
  }

  start <- 0L
  pos <- w
  adaptive <- numeric(0)
  bounds <- integer(0)
  while (pos + w <= N) {
    seg <- xs[(start + 1L):(pos + w)]
    mrel <- pos - start + 1L
    d <- if (mrel - 1L > p && w > p) {
      Glr(seg, mrel, length(seg), order = p)$d
    } else {
      0
    }
    adaptive <- c(adaptive, d)
    if (d > thr) {
      bounds <- c(bounds, pos)
      start <- pos # the reference window restarts here
      pos <- start + w
    } else {
      pos <- pos + hop
    }
  }
  list(
    d = adaptive, d_fixed_reference = dists, times = times,
    boundaries = bounds, n_boundaries = length(bounds),
    threshold = thr, median = med, mad = mad,
    window = w, step = hop, order = p, fs = fsv,
    reference_restarts_at_boundaries = TRUE,
    robust_threshold = is.null(threshold),
    method = "Rangayyan (2024) Section 8.5.3 (GLR adaptive segmentation)"
  )
}

# ----------------------------------------------------------------- bsacorr
#' Bsacorr
#'
#' A step of the rangayyan_ccf implementation. Called by \code{CorrConv}, \code{XCorrProc}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param x Coerced to numeric by the body, with \code{as.numeric}.
#' @param y Coerced to numeric by the body, with \code{as.numeric}.
#' @param maxlag Optional; may be \code{NULL}. Coerced to integer by the body, with \code{as.integer}.
#' @param normalize A flag; the body branches on it. Defaults to \code{FALSE}.
#' @param biased A flag; the body branches on it. Defaults to \code{TRUE}.
#' @return A list with \code{ccf}, \code{lags}, \code{peak}, \code{peak_lag}, \code{n}, \code{biased}, \code{normalized}, \code{positive_lag_means_y_trails_x}, \code{biased_keeps_nonnegative_definiteness}, \code{method}.
#' @export
XCorr <- function(x, y, maxlag = NULL, normalize = FALSE, biased = TRUE) {
  # R_xy(m) = (1/N) sum_n x(n) y(n + m).  A POSITIVE lag means y trails x.
  xs <- as.numeric(x)
  ys <- as.numeric(y)
  if (!length(xs) || !length(ys)) stop("need at least one sample")
  n <- length(xs)
  lim <- if (is.null(maxlag)) (n - 1L) else as.integer(maxlag)
  if (lim < 0L) stop("maxlag must not be negative")
  lags <- (-lim):lim
  vals <- vapply(lags, function(k) {
    i <- seq_len(n)
    j <- i + k
    keep <- j >= 1L & j <= length(ys)
    if (!any(keep)) {
      return(0)
    }
    s <- .morie_fsum(xs[i[keep]] * ys[j[keep]])
    if (biased) s / n else s / sum(keep)
  }, numeric(1))
  if (normalize) {
    ex <- .morie_fsum(xs * xs)
    ey <- .morie_fsum(ys * ys)
    if (ex > 0 && ey > 0) vals <- vals * (n / sqrt(ex * ey))
  }
  k <- which.max(abs(vals))
  list(
    ccf = vals, lags = lags, peak = vals[k], peak_lag = lags[k],
    n = n, biased = isTRUE(biased), normalized = isTRUE(normalize),
    positive_lag_means_y_trails_x = TRUE,
    biased_keeps_nonnegative_definiteness = isTRUE(biased),
    method = "Rangayyan (2024) Ch. 3-4 (cross-correlation)"
  )
}

#' The RAW sum, not divided by N: the matched-filter output AT each
#'
#' instant IS this sum, so its peak locates the pattern.  Neither XCorr
#' mode gives it (biased divides by n, unbiased by the overlap count),
#' so it is computed here rather than delegated.
#'
#' @param x Coerced to numeric by the body, with \code{as.numeric}.
#' @param y Coerced to numeric by the body, with \code{as.numeric}.
#' @param delays Optional; may be \code{NULL}. A vector; its length is taken.
#' @return A list with \code{ccf}, \code{lags}, \code{peak}, \code{peak_lag}, \code{normalized}, \code{is_the_matched_filter_output}, \code{method}.
#' @export
XCorrDisc <- function(x, y, delays = NULL) {
  # The RAW sum, not divided by N: the matched-filter output AT each
  # instant IS this sum, so its peak locates the pattern.  Neither XCorr
  # mode gives it (biased divides by n, unbiased by the overlap count),
  # so it is computed here rather than delegated.
  xs <- as.numeric(x)
  ys <- as.numeric(y)
  if (!length(xs) || !length(ys)) {
    stop("both signals need at least one sample")
  }
  ks <- if (is.null(delays)) {
    (-(length(xs) - 1L)):(length(ys) - 1L)
  } else {
    as.integer(delays)
  }
  out <- vapply(ks, function(k) {
    i <- seq_len(length(xs))
    j <- i + k
    keep <- j >= 1L & j <= length(ys)
    if (!any(keep)) 0 else .morie_fsum(xs[i[keep]] * ys[j[keep]])
  }, numeric(1))
  # the LARGEST value, not the largest magnitude: a strongly negative
  # correlation is not a match.
  j <- which.max(out)
  scalar <- !is.null(delays) && length(delays) == 1L
  list(
    ccf = if (scalar) out[[1L]] else out, lags = ks,
    peak = out[[j]], peak_lag = ks[[j]], normalized = FALSE,
    is_the_matched_filter_output = TRUE,
    method = "Rangayyan (2024) Ch. 4 (discrete CCF with delay)"
  )
}

#' The continuous-time form, evaluated by the trapezoidal rule on the
#'
#' samples that BOTH signals cover.  Long delays use less data, so the
#' overlap fraction travels with the estimate.
#'
#' @param x Coerced to numeric by the body, with \code{as.numeric}.
#' @param y Coerced to numeric by the body, with \code{as.numeric}.
#' @param t Coerced to numeric by the body, with \code{as.numeric}.
#' @param delays Coerced to numeric by the body, with \code{as.numeric}.
#' @return A list with \code{ccf}, \code{delays}, \code{overlap_fraction}, \code{interpolated}, \code{trapezoidal}, \code{long_delays_use_less_data}, \code{method}.
#' @export
XCorrCont <- function(x, y, t, delays) {
  # The continuous-time form, evaluated by the trapezoidal rule on the
  # samples that BOTH signals cover.  Long delays use less data, so the
  # overlap fraction travels with the estimate.
  xs <- as.numeric(x)
  ys <- as.numeric(y)
  tv <- as.numeric(t)
  if (length(xs) != length(tv) || length(ys) != length(tv)) {
    stop("x, y and t must have the same length")
  }
  dl <- as.numeric(delays)
  span <- tv[length(tv)] - tv[1L]
  out <- vapply(dl, function(d) {
    ok <- (tv + d) >= tv[1L] & (tv + d) <= tv[length(tv)]
    if (sum(ok) < 2L) {
      return(0)
    }
    yi <- stats::approx(tv, ys, xout = tv[ok] + d)$y
    xi <- xs[ok]
    ti <- tv[ok]
    .morie_fsum(diff(ti) * (xi[-1L] * yi[-1L] +
      xi[-length(xi)] * yi[-length(yi)]) / 2)
  }, numeric(1))
  frac <- vapply(dl, function(d) {
    ok <- (tv + d) >= tv[1L] & (tv + d) <= tv[length(tv)]
    if (sum(ok) < 2L) 0 else (max(tv[ok]) - min(tv[ok])) / span
  }, numeric(1))
  list(
    ccf = out, delays = dl, overlap_fraction = if (length(frac) == 1L) {
      frac[[1L]]
    } else {
      frac
    },
    interpolated = TRUE, trapezoidal = TRUE,
    long_delays_use_less_data = TRUE,
    method = "Rangayyan (2024) Ch. 3 (continuous-time cross-correlation)"
  )
}

#' The ensemble expectation is estimated by a TIME average, which is
#' only
#'
#' legitimate under joint stationarity and ergodicity.
#'
#' @param x Coerced to numeric by the body, with \code{as.numeric}.
#' @param y Coerced to numeric by the body, with \code{as.numeric}.
#' @param lags Optional; may be \code{NULL}. Coerced to integer by the body, with \code{as.integer}.
#' @param remove_mean A flag; the body branches on it. Defaults to \code{TRUE}.
#' @return A list with \code{ccf}, \code{lags}, \code{means}, \code{mean_product}, \code{mean_removed}, \code{is_cross_covariance_when_mean_removed}, \code{expectation_estimated_by_time_average}, \code{requires_joint_stationarity_and_ergodicity}, \code{method}.
#' @export
XCorrProc <- function(x, y, lags = NULL, remove_mean = TRUE) {
  # The ensemble expectation is estimated by a TIME average, which is only
  # legitimate under joint stationarity and ergodicity.
  xs <- as.numeric(x)
  ys <- as.numeric(y)
  if (length(xs) != length(ys)) stop("x and y must have the same length")
  n <- length(xs)
  lim <- if (is.null(lags)) (n - 1L) else as.integer(lags)
  mx <- .morie_fsum(xs) / n
  my <- .morie_fsum(ys) / n
  ax <- if (remove_mean) xs - mx else xs
  ay <- if (remove_mean) ys - my else ys
  r <- XCorr(ax, ay, maxlag = lim)
  list(
    ccf = r$ccf, lags = r$lags, means = c(mx, my),
    mean_product = mx * my, mean_removed = isTRUE(remove_mean),
    is_cross_covariance_when_mean_removed = isTRUE(remove_mean),
    expectation_estimated_by_time_average = TRUE,
    requires_joint_stationarity_and_ergodicity = TRUE,
    method = "Rangayyan (2024) Ch. 3 (CCF of two random processes)"
  )
}

#' Correlation IS convolution with one sequence reversed
#'
#' A step of the rangayyan_ccf implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param x Coerced to numeric by the body, with \code{as.numeric}.
#' @param y Coerced to numeric by the body, with \code{as.numeric}.
#' @return A list with \code{ccf}, \code{via_convolution}, \code{lags}, \code{max_difference}, \code{identity_holds}, \code{correlation_is_convolution_with_one_reversed}, \code{method}.
#' @export
CorrConv <- function(x, y) {
  # Correlation IS convolution with one sequence reversed.
  xs <- as.numeric(x)
  ys <- as.numeric(y)
  r <- XCorr(xs, ys, biased = FALSE)
  direct <- vapply(r$lags, function(k) {
    i <- seq_len(length(xs))
    j <- i + k
    keep <- j >= 1L & j <= length(ys)
    if (!any(keep)) 0 else .morie_fsum(xs[i[keep]] * ys[j[keep]])
  }, numeric(1))
  # conv(x, rev(y))[i] = sum_j x[j] y[m + 1 - i + j], so the lag-k
  # correlation sits at i = m + 1 - k.  Written out rather than handed to
  # stats::convolve so the index algebra is visible and checkable.
  ry <- rev(ys)
  m <- length(ys)
  nx <- length(xs)
  conv <- vapply(seq_len(nx + m - 1L), function(i) {
    .morie_fsum(vapply(seq_len(nx), function(j) {
      k <- i - j + 1L
      if (k >= 1L && k <= m) xs[j] * ry[k] else 0
    }, numeric(1)))
  }, numeric(1))
  via <- vapply(r$lags, function(k) {
    i <- m - k
    if (i >= 1L && i <= length(conv)) conv[[i]] else 0
  }, numeric(1))
  gap <- max(abs(direct - via))
  list(
    ccf = direct, via_convolution = via, lags = r$lags,
    max_difference = gap, identity_holds = gap < 1e-9,
    correlation_is_convolution_with_one_reversed = TRUE,
    method = "Rangayyan (2024) Ch. 3 (correlation-convolution identity)"
  )
}

#' Normalized per shift, so a LOUD stretch cannot outscore a matching
#' one
#'
#' -- that is the whole reason to normalize rather than take the raw
#' CCF.
#'
#' @param x Coerced to numeric by the body, with \code{as.numeric}.
#' @param template Coerced to numeric by the body, with \code{as.numeric}.
#' @return A list with \code{gamma}, \code{shifts}, \code{peak}, \code{peak_shift}, \code{normalized_per_shift}, \code{bounded_in_unit_interval}, \code{loud_beats_matching_without_normalization}, \code{method}.
#' @export
NccfTpl <- function(x, template) {
  # Normalized per shift, so a LOUD stretch cannot outscore a matching one
  # -- that is the whole reason to normalize rather than take the raw CCF.
  xs <- as.numeric(x)
  tp <- as.numeric(template)
  m <- length(tp)
  n <- length(xs)
  if (m < 1L) stop("the template needs at least one sample")
  if (m > n) stop("the template is longer than the signal")
  et <- .morie_fsum(tp * tp)
  if (et <= 0) stop("a template with no energy has no direction")
  shifts <- 0:(n - m)
  g <- vapply(shifts, function(s) {
    w <- xs[(s + 1L):(s + m)]
    ew <- .morie_fsum(w * w)
    if (ew <= 0) 0 else .morie_fsum(w * tp) / sqrt(ew * et)
  }, numeric(1))
  k <- which.max(g)
  list(
    gamma = g, shifts = shifts, peak = g[[k]], peak_shift = shifts[[k]],
    normalized_per_shift = TRUE,
    bounded_in_unit_interval = all(abs(g) <= 1 + 1e-12),
    loud_beats_matching_without_normalization = TRUE,
    method = "Rangayyan (2024) Ch. 4 (normalized template CCF)"
  )
}

#' Eq (4.25).  Delegates to DotProd -- one copy of the arithmetic
#'
#' A step of the rangayyan_ccf implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param x Passed to \code{DotProd}.
#' @param y Passed to \code{DotProd}.
#' @return A list with \code{gamma}, \code{dot_product}, \code{norms}, \code{pearson}, \code{is_a_cosine_not_an_agreement}, \code{unity_for_a_positive_rescaling}, \code{method}.
#' @export
CorrDot <- function(x, y) {
  # eq (4.25).  Delegates to DotProd -- one copy of the arithmetic.
  raw <- DotProd(x, y)
  if (is.null(raw$gamma)) stop("a signal with no energy has no direction")
  centred <- DotProd(x, y, subtract_mean = TRUE)
  list(
    gamma = raw$gamma, dot_product = raw$dot_product,
    norms = c(sqrt(raw$energy_x), sqrt(raw$energy_y)),
    pearson = centred$gamma,
    is_a_cosine_not_an_agreement = TRUE,
    unity_for_a_positive_rescaling = TRUE,
    method = paste(
      "Rangayyan (2024) Ch. 4 (correlation as a",
      "normalized dot product), via eqs. (4.24)-(4.25)"
    )
  )
}

#' Phi(m) = (1/N) sum_n x(n) x(n + m).  BOTH the raw phi and the
#'
#' normalized rho are returned: rho is what a threshold can be set on,
#' phi is what the equation says, and conflating them makes any
#' threshold depend on the signal amplitude.
#'
#' @param x Coerced to numeric by the body, with \code{as.numeric}.
#' @param fs Coerced to numeric by the body, with \code{as.numeric}.
#' @param maxlag Optional; may be \code{NULL}. Coerced to integer by the body, with \code{as.integer}.
#' @return A list with \code{acf}, \code{normalized}, \code{lags}, \code{fs}, \code{peak_lag}, \code{peak_lag_seconds}, \code{implied_frequency_hz}, \code{peak_value}, \code{a_rhythm_gives_a_periodic_acf}, \code{robust_to_amplitude_variation}, \code{method}.
#' @export
EegAcf <- function(x, fs, maxlag = NULL) {
  # phi(m) = (1/N) sum_n x(n) x(n + m).  BOTH the raw phi and the
  # normalized rho are returned: rho is what a threshold can be set on,
  # phi is what the equation says, and conflating them makes any
  # threshold depend on the signal amplitude.
  xs <- as.numeric(x)
  fsv <- as.numeric(fs)
  if (fsv <= 0) stop("fs must be positive")
  n <- length(xs)
  if (n < 4L) stop("need at least four samples")
  lim <- if (is.null(maxlag)) (n - 1L) else as.integer(maxlag)
  if (!(lim >= 1L && lim <= n - 1L)) stop("maxlag must lie in 1..n-1")
  mu <- .morie_fsum(xs) / n
  cx <- xs - mu
  phi <- vapply(
    0:lim, function(m) {
      .morie_fsum(cx[seq_len(n - m)] * cx[seq_len(n - m) + m]) / n
    },
    numeric(1)
  )
  if (phi[1L] <= 0) stop("the epoch has no variance")
  rho <- phi / phi[1L]
  pk <- NULL
  for (m in 2:(lim - 1L)) {
    if (rho[m + 1L] > rho[m] && rho[m + 1L] >= rho[m + 2L] &&
      rho[m + 1L] > 0) {
      pk <- m
      break
    }
  }
  list(
    acf = phi, normalized = rho, lags = 0:lim, fs = fsv,
    peak_lag = pk,
    peak_lag_seconds = if (is.null(pk)) NULL else pk / fsv,
    implied_frequency_hz = if (is.null(pk)) NULL else fsv / pk,
    peak_value = if (is.null(pk)) NULL else rho[pk + 1L],
    a_rhythm_gives_a_periodic_acf = TRUE,
    robust_to_amplitude_variation = TRUE,
    method = "Rangayyan (2024) Ch. 4 (EEG rhythm detection by ACF)"
  )
}

#' The alpha band by convention; the SAME test serves the other bands,
#'
#' so the band is an argument, not a constant.  Both the band AND the
#' amplitude have to be satisfied -- a lag in band with a tiny peak is
#' not a rhythm.
#'
#' @param x Coerced to numeric by the body, with \code{as.numeric}.
#' @param fs Coerced to numeric by the body, with \code{as.numeric}.
#' @param band Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{c(8, 13)}.
#' @param threshold Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{0.3}.
#' @return A list with \code{acf}, \code{band}, \code{lag_range}, \code{peak_lag}, \code{peak}, \code{frequency_hz}, \code{threshold}, \code{present}, \code{needs_both_the_band_and_the_amplitude}, \code{same_test_serves_other_bands}, \code{fs}, \code{method}.
#' @export
AlphaRhy <- function(x, fs, band = c(8, 13), threshold = 0.3) {
  # The alpha band by convention; the SAME test serves the other bands,
  # so the band is an argument, not a constant.  Both the band AND the
  # amplitude have to be satisfied -- a lag in band with a tiny peak is
  # not a rhythm.
  xs <- as.numeric(x)
  fsv <- as.numeric(fs)
  if (fsv <= 0) stop("fs must be positive")
  lo <- as.numeric(band)[1L]
  hi <- as.numeric(band)[2L]
  if (!(lo > 0 && hi > lo)) stop("the band must be an increasing pair")
  klo <- max(1L, as.integer(floor(fsv / hi)))
  khi <- as.integer(ceiling(fsv / lo))
  r <- EegAcf(xs, fsv, maxlag = min(khi + 2L, length(xs) - 1L))
  a <- r$normalized
  rng <- klo:min(khi, length(a) - 1L)
  vals <- a[rng + 1L]
  k <- rng[which.max(vals)]
  peak <- max(vals)
  list(
    acf = a, band = c(lo, hi), lag_range = c(klo, khi),
    peak_lag = k, peak = peak, frequency_hz = fsv / k,
    threshold = as.numeric(threshold),
    present = peak >= as.numeric(threshold),
    needs_both_the_band_and_the_amplitude = TRUE,
    same_test_serves_other_bands = TRUE, fs = fsv,
    method = "Rangayyan (2024) Ch. 4 (alpha rhythm by the ACF)"
  )
}

# ------------------------------------------------------------------ bsasig
#' Bsasig
#'
#' A step of the rangayyan_ccf implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param n Optional; may be \code{NULL}. Coerced to integer by the body, with \code{as.integer}.
#' @param f1 Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{5}.
#' @param f2 Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{20}.
#' @param a1 Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{1}.
#' @param a2 Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{1}.
#' @param fs Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{100}.
#' @param duration Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{1}.
#' @return A list with \code{x}, \code{t}, \code{n}, \code{fs}, \code{f1}, \code{f2}, \code{a1}, \code{a2}, \code{components_are_known_by_construction}, \code{method}.
#' @export
SinCosTest <- function(n = NULL, f1 = 5, f2 = 20, a1 = 1, a2 = 1,
                       fs = 100, duration = 1) {
  # A test signal whose components are known BY CONSTRUCTION, which is
  # what makes it useful for checking a spectral estimate.
  fsv <- as.numeric(fs)
  if (fsv <= 0) stop("fs must be positive")
  nv <- if (!is.null(n)) {
    as.integer(n)
  } else {
    as.integer(round(as.numeric(duration) * fsv))
  }
  if (nv < 1L) stop("need at least one sample")
  t <- (seq_len(nv) - 1L) / fsv
  x <- as.numeric(a1) * sin(2 * pi * as.numeric(f1) * t) +
    as.numeric(a2) * cos(2 * pi * as.numeric(f2) * t)
  list(
    x = x, t = t, n = nv, fs = fsv, f1 = as.numeric(f1),
    f2 = as.numeric(f2), a1 = as.numeric(a1), a2 = as.numeric(a2),
    components_are_known_by_construction = TRUE,
    method = "Rangayyan (2024) Ch. 3 (sine-plus-cosine test signal)"
  )
}

#' A composite of shifted, scaled copies of one pattern.  Copies closer
#'
#' together than the pattern is long OVERLAP, and the count is reported
#' because an overlap is what breaks a peak-picking detector.
#'
#' @param g Coerced to numeric by the body, with \code{as.numeric}.
#' @param shifts Coerced to integer by the body, with \code{as.integer}.
#' @param scales Optional; may be \code{NULL}. Coerced to numeric by the body, with \code{as.numeric}.
#' @param n Optional; may be \code{NULL}. Coerced to integer by the body, with \code{as.integer}.
#' @return A list with \code{x}, \code{n}, \code{shifts}, \code{scales}, \code{peaks_expected_at}, \code{overlapping_pairs}, \code{pattern}, \code{pattern_length}, \code{n_copies}, \code{copies_add}, \code{overlap_breaks_peak_picking}, \code{method}.
#' @export
CompSig <- function(g, shifts, scales = NULL, n = NULL) {
  # A composite of shifted, scaled copies of one pattern.  Copies closer
  # together than the pattern is long OVERLAP, and the count is reported
  # because an overlap is what breaks a peak-picking detector.
  gv <- as.numeric(g)
  sh <- as.integer(shifts)
  sc <- if (is.null(scales)) rep(1, length(sh)) else as.numeric(scales)
  if (length(sc) != length(sh)) {
    stop("scales must match shifts in length")
  }
  m <- length(gv)
  nv <- if (!is.null(n)) as.integer(n) else (max(sh) + m)
  x <- numeric(nv)
  for (i in seq_along(sh)) {
    for (j in seq_len(m)) {
      k <- sh[i] + j
      if (k >= 1L && k <= nv) x[k] <- x[k] + sc[i] * gv[j]
    }
  }
  # The peak of the MATCHED-FILTER OUTPUT, not of the composite itself:
  # h(n) = g(M-1-n), so the correlation completes at d + M - 1.
  ov <- 0L
  if (length(sh) > 1L) {
    for (i in seq_len(length(sh) - 1L)) {
      for (j in (i + 1L):length(sh)) {
        if (abs(sh[i] - sh[j]) < m) ov <- ov + 1L
      }
    }
  }
  list(
    x = x, n = nv, shifts = sh, scales = sc,
    peaks_expected_at = sh + m - 1L, overlapping_pairs = as.integer(ov),
    pattern = gv, pattern_length = m, n_copies = length(sh),
    copies_add = TRUE, overlap_breaks_peak_picking = ov > 0,
    method = "Rangayyan (2024) Ch. 3-4 (composite of shifted copies)"
  )
}

# ----------------------------------------------------------------- bsastat
#' Bsastat
#'
#' A step of the rangayyan_ccf implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param x Coerced to numeric by the body, with \code{as.numeric}.
#' @param y Coerced to numeric by the body, with \code{as.numeric}.
#' @return A list with \code{r}, \code{r_squared}, \code{n}, \code{means}, \code{cosine_without_removing_means}, \code{means_are_removed}, \code{invariant_to_positive_affine_change}, \code{says_nothing_about_agreement}, \code{method}.
#' @export
CorrCoef <- function(x, y) {
  # Pearson's r.  The MEANS ARE REMOVED, which is what separates it from
  # the Chapter 4 dot-product cosine; both are returned so the difference
  # is visible.  Delegates to DotProd -- one copy of eqs (4.24)-(4.25).
  xs <- as.numeric(x)
  ys <- as.numeric(y)
  if (length(xs) != length(ys)) stop("x and y must have the same length")
  n <- length(xs)
  if (n < 2L) stop("need at least two paired observations")
  centred <- DotProd(xs, ys, subtract_mean = TRUE)
  if (is.null(centred$gamma)) {
    stop("a variable is constant; the correlation is undefined")
  }
  r <- centred$gamma
  list(
    r = r, r_squared = r * r, n = n,
    means = c(.morie_fsum(xs) / n, .morie_fsum(ys) / n),
    cosine_without_removing_means = DotProd(xs, ys)$gamma,
    means_are_removed = TRUE,
    invariant_to_positive_affine_change = TRUE,
    says_nothing_about_agreement = TRUE,
    method = "Pearson correlation; Rangayyan (2024) Ch. 5"
  )
}

# pre-policy spellings kept as aliases
morie_rangayyan_glr <- Glr
morie_rangayyan_eeg_adaptive_seg <- EegAdapt
morie_rangayyan_ccf <- XCorr
morie_rangayyan_ccf_discrete <- XCorrDisc
morie_rangayyan_ccf_continuous <- XCorrCont
morie_rangayyan_ccf_processes <- XCorrProc
morie_rangayyan_corr_conv <- CorrConv
morie_rangayyan_nccf_template <- NccfTpl
morie_rangayyan_corr_dot <- CorrDot
morie_rangayyan_eeg_acf <- EegAcf
morie_rangayyan_alpha_rhythm <- AlphaRhy
morie_rangayyan_sin_cos_test <- SinCosTest
morie_rangayyan_composite_signal <- CompSig
morie_correlation_coeff <- CorrCoef

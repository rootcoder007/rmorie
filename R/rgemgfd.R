# SPDX-License-Identifier: AGPL-3.0-or-later
#' Fractal dimension of the EMG against force
#'
#' Rangayyan and Krishnan, Biomedical Signal Analysis, 3rd ed., Wiley-IEEE
#' Press, 2024, Section 5.13.2 "Fractal dimension", "Higuchi's method", p. 304,
#' equations (5.39)-(5.41), and Section 5.13.4 "Fractal analysis of EMG
#' signals", p. 305.  Page 304 was read as a rendered page image rather than
#' from an extracted text layer.
#'
#' Equation (5.39): x_k(m) = x(m), x(m+k), x(m+2k), ...,
#' x(m + floor((N-m)/k) k), for m = 1, 2, ..., k.
#' Equation (5.40): L(m,k) = (1/k) (N-1)/(k floor((N-m)/k))
#' sum_{i=1}^{floor((N-m)/k)} |x(m+ik) - x[m+(i-1)k]|.
#' Equation (5.41): L(k) = (1/k) sum_{m=1}^{k} L(m,k).
#' "The slope of a straight-line fit to a log-log plot of L(k) against 1/k
#' gives the FD of the original signal."
#'
#' Note the LEADING 1/k of equation (5.40), which is Higuchi (1988) eq. (1).
#' Dropping it multiplies L(k) by k and reduces the fitted slope by exactly
#' one, so a straight line, whose fractal dimension is 1, comes out as 0.
#'
#' Section 5.13.4, p. 305: "Segments of duration 1 s were cut for each level of
#' contraction to estimate FD.  It is evident that FD increases with the level
#' of contraction ... with high correlation."  Figure 5.25 reports r^2 = 0.95.
#' The goodness of fit is equation (5.28), p. 292.
#'
#' Primary source for the estimator: Higuchi, T. (1988), "Approach to an
#' irregular time series on the basis of the fractal theory", Physica D
#' 31:277-283.
#'
#' @param emg the EMG signal.
#' @param force the commanded contraction level, one value per EMG sample.
#' @param fs sampling rate in Hz; round(fs) samples are cut from the start of
#'   each level, as Section 5.13.4 describes.
#' @param kmax the largest lag k of equations (5.39)-(5.41).
#' @param rest_level levels at or below this are rest and are excluded.
#' @return list: estimate, levels, fd, intervals, slope, intercept, r2, kmax,
#'   segment_samples, n_levels, n, fs, method.
#' @keywords internal
#' @examples
#' f <- rep(c(20, 40, 60, 80), each = 32)
#' e <- rep(c(1, -1), length.out = 128) * f
#' rgemgfd(e, f, 32, kmax = 4)$n_levels
#' @export
rgemgfd <- function(emg, force, fs, kmax = 10L, rest_level = 0) {
  e <- as.numeric(emg)
  f <- as.numeric(force)
  n <- length(e)
  if (n == 0L) stop("rangayyan_emg_fractal_dim: emg is empty")
  if (length(f) != n) stop("rangayyan_emg_fractal_dim: emg and force must have the same length")
  fsv <- as.numeric(fs)
  if (fsv <= 0) stop("rangayyan_emg_fractal_dim: fs must be positive")
  seglen <- as.integer(round(fsv))
  if (seglen < 4L) stop("rangayyan_emg_fractal_dim: fs is too low for a one-second segment")
  lev <- numeric(0); fd <- numeric(0); ivs <- list()
  for (r in .rgisint_runs(f)) {
    a <- r[1]; b <- r[2]
    if (f[a] <= rest_level) next
    stop_i <- min(b, a + seglen - 1L)
    if (stop_i - a + 1L < 4L) next
    lev <- c(lev, f[a])
    fd <- c(fd, .rg_higuchi_fd(e[a:stop_i], kmax))
    ivs[[length(ivs) + 1L]] <- c(a - 1L, stop_i)
  }
  if (length(lev) < 2L)
    stop("rangayyan_emg_fractal_dim: need at least two usable contraction levels")
  r2 <- .rgemgf_r2(lev, fd)
  fit <- .rgemgf_linfit(lev, fd)
  list(estimate = r2, levels = lev, fd = fd, intervals = ivs,
       slope = fit[1], intercept = fit[2], r2 = r2, kmax = as.integer(kmax),
       segment_samples = seglen, n_levels = length(lev), n = n, fs = fsv,
       method = "Rangayyan (2024) eqs. (5.39)-(5.41) p.304 Higuchi FD per 1 s segment, Sect. 5.13.4 p.305; eq. (5.28) for r^2")
}

#' .rg_higuchi_fd
#'
#' A step of the rgemgfd implementation. Called by \code{rgemgfd}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param x Coerced to numeric by the body, with \code{as.numeric}.
#' @param kmax Coerced to integer by the body, with \code{as.integer}.
#' @return The value of \code{[}.
#' @export
.rg_higuchi_fd <- function(x, kmax) {
  xs <- as.numeric(x)
  N <- length(xs)
  kk <- as.integer(kmax)
  if (N < 4L) stop("higuchi_fd: need at least four samples")
  if (kk < 2L) stop("higuchi_fd: kmax must be at least two")
  kk <- min(kk, N %/% 2L)
  Lk <- numeric(0); ks <- integer(0)
  for (k in seq_len(kk)) {
    acc <- 0; used <- 0L
    for (m in seq_len(k)) {
      idx <- seq.int(m, N, by = k)          # eq (5.39)
      if (length(idx) < 2L) next
      terms <- length(idx) - 1L             # = floor((N-m)/k)
      s <- sum(abs(diff(xs[idx])))
      acc <- acc + (s / k) * ((N - 1) / (k * terms))   # eq (5.40)
      used <- used + 1L
    }
    if (used == 0L) next
    Lk <- c(Lk, acc / used)                 # eq (5.41)
    ks <- c(ks, k)
  }
  keep <- Lk > 0
  if (sum(keep) < 2L) stop("higuchi_fd: the signal has no measurable length")
  .rgemgf_linfit(log(1 / ks[keep]), log(Lk[keep]))[1]
}

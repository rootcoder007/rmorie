# SPDX-License-Identifier: AGPL-3.0-or-later
#' EMG correlates of isometric muscular contraction
#'
#' Rangayyan and Krishnan, Biomedical Signal Analysis, 3rd ed., Wiley-IEEE
#' Press, 2024, Section 5.11 "Application: Electrical and Mechanical Correlates
#' of Muscular Contraction", pp. 294-296, with Figures 5.20 and 5.21.
#'
#' "the subjects performed isometric contraction (that is, with no movement of
#' the associated leg) of the rectus femoris (thigh) muscle to different levels
#' of torque ... Four levels of contraction were performed from 20% to 80% of
#' the MVC level of the individual subject ... Each contraction was held for a
#' duration of about 6 s ... RMS values were computed for each contraction
#' level over a duration of 5 s ... The almost-linear trends of the RMS values
#' ... with muscular contraction indicate the usefulness of the derived
#' parameter ... It should, however, be noted that the relationship between RMS
#' values and contraction may not follow the same (linear) pattern for
#' different muscles."
#'
#' Because the contraction is isometric the commanded level is held constant
#' for each trial, so the levels are read off the force channel as maximal runs
#' of a constant commanded value.  That is what separates this function from
#' rgemgf, which delineates intervals from a continuously varying force trace
#' by the 10% MVC rule of Section 5.9.  The trend is quantified by equation
#' (5.28), p. 292; Figure 5.21 is the reason r^2 is returned and not only the
#' slope, since for the biceps and the deltoid the relationship is not linear.
#'
#' @param emg the EMG signal.
#' @param force the commanded contraction level, one value per EMG sample,
#'   held constant within each trial.
#' @param fs sampling rate in Hz.
#' @param rest_level levels at or below this are rest and are excluded.
#' @return list: estimate, levels, rms, durations, intervals, slope, intercept,
#'   r2, n_levels, n, fs, method.
#' @keywords internal
#' @examples
#' f <- rep(c(20, 40, 60, 80), each = 10)
#' e <- rep(c(1, -1), length.out = 40) * f
#' rgisint(e, f, 10)$slope
#' @export
rgisint <- function(emg, force, fs, rest_level = 0) {
  e <- as.numeric(emg)
  f <- as.numeric(force)
  n <- length(e)
  if (n == 0L) stop("rangayyan_isometric_contraction: emg is empty")
  if (length(f) != n) stop("rangayyan_isometric_contraction: emg and force must have the same length")
  fsv <- as.numeric(fs)
  if (fsv <= 0) stop("rangayyan_isometric_contraction: fs must be positive")
  lev <- numeric(0)
  rv <- numeric(0)
  dur <- numeric(0)
  ivs <- list()
  for (r in .rgisint_runs(f)) {
    a <- r[1]
    b <- r[2]
    if (f[a] <= rest_level) next
    lev <- c(lev, f[a])
    rv <- c(rv, Rms(e[a:b])$rms)
    dur <- c(dur, (b - a + 1L) / fsv)
    ivs[[length(ivs) + 1L]] <- c(a - 1L, b)
  }
  if (length(lev) < 2L)
    stop("rangayyan_isometric_contraction: need at least two held contraction levels")
  r2 <- .rgemgf_r2(lev, rv)
  fit <- .rgemgf_linfit(lev, rv)
  list(estimate = r2, levels = lev, rms = rv, durations = dur, intervals = ivs,
       slope = fit[1], intercept = fit[2], r2 = r2, n_levels = length(lev),
       n = n, fs = fsv,
       method = "Rangayyan (2024) Sect. 5.11 pp.294-296, RMS per held isometric level; eq. (5.28) for r^2")
}

#' .rgisint_runs
#'
#' A step of the rgisint implementation. Called by \code{rgemgfd}, \code{rgisint}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param f A vector; its length is taken and its elements indexed.
#' @return The value of \code{out}, as built in the body.
#' @export
.rgisint_runs <- function(f) {
  out <- list()
  i <- 1L
  n <- length(f)
  while (i <= n) {
    j <- i
    while (j <= n && f[j] == f[i]) j <- j + 1L
    out[[length(out) + 1L]] <- c(i, j - 1L)
    i <- j
  }
  out
}

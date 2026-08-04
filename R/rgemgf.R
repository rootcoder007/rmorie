# SPDX-License-Identifier: AGPL-3.0-or-later
#' EMG parameters in relation to the force exerted
#'
#' Rangayyan and Krishnan, Biomedical Signal Analysis, 3rd ed.,
#' Wiley-IEEE Press, 2024, Section 5.9 "Application: Quantitative Analysis of
#' the EMG in Relation to Force Exerted", pp. 290-292, with equation (5.28) on
#' p. 292, read as a rendered page image rather than from an extracted text
#' layer.
#'
#' The p. 290 procedure is followed literally: "starting from the first
#' sample, the point where the force signal increased beyond 10% MVC was
#' identified.  Then, the next point where the signal dropped below 10% MVC
#' was identified.  This process was repeated until the end of the signal.  To
#' refine the definition of each interval of contraction, a threshold was
#' defined as 0.7 times the maximum level of contraction within the interval.
#' Then, the smaller extent of each interval previously identified, within
#' which the force remained above the threshold, was detected."  Within each
#' interval the average force, the RMS value, the ZCR and the turns count
#' divided by the duration (the TCR) are computed, the significant-turn
#' threshold being 100 microvolts in the book's own experiment.
#'
#' The goodness of fit is equation (5.28), p. 292:
#' r^2 = [sum x(n) y(n) - N xbar ybar]^2 /
#'       ([sum x^2(n) - N xbar^2][sum y^2(n) - N ybar^2]).
#' The captions of Figures 5.15-5.17 give r^2 = 0.98 for RMS, 0.78 for ZCR and
#' 0.97 for TCR: RMS and TCR track force closely, ZCR does not.
#'
#' The RMS and the turns count are not reimplemented here; Rms and TurnsCount
#' already own Sections 5.6.1 and 5.6.3 and are called.
#'
#' @param emg the EMG signal.
#' @param force the simultaneous force signal, same length and sampling rate;
#'   its own maximum is taken as the MVC level.
#' @param fs sampling rate in Hz.
#' @param window optional length in samples of the causal short-time window of
#'   Section 5.6; it plays no part in the per-interval parameters.
#' @param turn_threshold significant-turn threshold of Section 5.6.3.
#' @return list: estimate, intervals, n_intervals, force_levels, rms, zcr, tcr,
#'   r2_rms, r2_zcr, r2_tcr, the three straight-line fits, mvc, n, fs, method.
#' @keywords internal
#' @examples
#' f <- c(rep(0, 5), rep(20, 10), rep(0, 5), rep(80, 10), rep(0, 5))
#' e <- rep(c(1, -1), length.out = 35) * (f + 1)
#' rgemgf(e, f, 100)$n_intervals
#' @export
rgemgf <- function(emg, force, fs, window = NULL, turn_threshold = 100) {
  e <- as.numeric(emg)
  f <- as.numeric(force)
  n <- length(e)
  if (n == 0L) stop("rangayyan_emg_force: emg is empty")
  if (length(f) != n) stop("rangayyan_emg_force: emg and force must have the same length")
  fsv <- as.numeric(fs)
  if (fsv <= 0) stop("rangayyan_emg_force: fs must be positive")
  if (max(f) <= 0) stop("rangayyan_emg_force: the force signal has no positive excursion")
  if (turn_threshold < 0) stop("rangayyan_emg_force: turn_threshold must be nonnegative")

  ivs <- .rgemgf_intervals(f)
  lev <- numeric(0); rv <- numeric(0); zv <- numeric(0); tv <- numeric(0)
  for (q in seq_along(ivs)) {
    a <- ivs[[q]][1]; b <- ivs[[q]][2]
    sf <- f[(a + 1L):b]
    se <- e[(a + 1L):b]
    m <- length(se)
    lev <- c(lev, sum(sf) / m)
    rv <- c(rv, Rms(se)$rms)
    zv <- c(zv, if (m >= 2L) .rgemgf_zcr(se) / (m - 1) * fsv else NA_real_)
    tv <- c(tv, if (m >= 3L) TurnsCount(se, threshold = turn_threshold)$turns / (m / fsv) else NA_real_)
  }
  if (length(ivs) >= 2L) {
    r2r <- .rgemgf_r2(lev, rv); r2z <- .rgemgf_r2(lev, zv); r2t <- .rgemgf_r2(lev, tv)
    fr <- .rgemgf_linfit(lev, rv); fz <- .rgemgf_linfit(lev, zv); ft <- .rgemgf_linfit(lev, tv)
  } else {
    r2r <- NA_real_; r2z <- NA_real_; r2t <- NA_real_
    fr <- c(NA_real_, NA_real_); fz <- fr; ft <- fr
  }
  out <- list(estimate = r2r,
              intervals = ivs, n_intervals = length(ivs),
              force_levels = lev, rms = rv, zcr = zv, tcr = tv,
              r2_rms = r2r, r2_zcr = r2z, r2_tcr = r2t,
              slope_rms = fr[1], intercept_rms = fr[2],
              slope_zcr = fz[1], intercept_zcr = fz[2],
              slope_tcr = ft[1], intercept_tcr = ft[2],
              mvc = max(f), n = n, fs = fsv,
              method = "Rangayyan (2024) Sect. 5.9 pp.290-292, interval delineation at 10% MVC refined at 0.7 of the interval peak; eq. (5.28) for r^2")
  if (!is.null(window)) {
    w <- as.integer(window)
    out$short_time_rms <- Rms(e, window = w)$short_time
    out$short_time_turns <- TurnsCount(e, threshold = turn_threshold, window = w)$short_time
    out$window <- w
  }
  out
}

# fraction of the MVC that opens an interval, and the refining fraction
.RGEMGF_MVC_FRACTION <- 0.10
.RGEMGF_REFINE_FRACTION <- 0.70

.rgemgf_intervals <- function(f) {
  # the p. 290 two-stage delineation; ranges are returned zero-based and
  # half-open, c(start, stop), to match the Python arm exactly
  n <- length(f)
  thr <- .RGEMGF_MVC_FRACTION * max(f)
  coarse <- list()
  i <- 1L
  while (i <= n) {
    if (f[i] > thr) {
      j <- i
      while (j <= n && f[j] > thr) j <- j + 1L
      coarse[[length(coarse) + 1L]] <- c(i, j - 1L)
      i <- j
    } else i <- i + 1L
  }
  fine <- list()
  for (q in seq_along(coarse)) {
    a <- coarse[[q]][1]; b <- coarse[[q]][2]
    seg <- f[a:b]
    t2 <- .RGEMGF_REFINE_FRACTION * max(seg)
    best <- NULL
    k <- 1L
    m <- length(seg)
    while (k <= m) {
      if (seg[k] >= t2) {
        p <- k
        while (p <= m && seg[p] >= t2) p <- p + 1L
        if (is.null(best) || (p - k) > (best[2] - best[1])) best <- c(k, p)
        k <- p
      } else k <- k + 1L
    }
    if (!is.null(best))
      fine[[length(fine) + 1L]] <- c(a + best[1] - 2L, a + best[2] - 2L)
  }
  fine
}

.rgemgf_zcr <- function(x) {
  s <- ifelse(x >= 0, 1, -1)
  sum(s[-1] != s[-length(s)])
}

.rgemgf_r2 <- function(x, y) {
  # equation (5.28), p. 292, written exactly as the book prints it
  n <- length(x)
  xbar <- sum(x) / n
  ybar <- sum(y) / n
  num <- sum(x * y) - n * xbar * ybar
  den <- (sum(x * x) - n * xbar * xbar) * (sum(y * y) - n * ybar * ybar)
  if (is.na(den) || den <= 0) return(NA_real_)
  (num * num) / den
}

.rgemgf_linfit <- function(x, y) {
  n <- length(x)
  den <- n * sum(x * x) - sum(x)^2
  if (is.na(den) || den == 0) return(c(NA_real_, NA_real_))
  slope <- (n * sum(x * y) - sum(x) * sum(y)) / den
  c(slope, (sum(y) - slope * sum(x)) / n)
}

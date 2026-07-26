#' Pan-Tompkins QRS detector -- Rangayyan Ch 6
#'
#' Pan & Tompkins (1985) QRS detector: bandpass 5-15 Hz -> differentiate ->
#' square -> 150-ms moving-window integration -> adaptive threshold (30 %
#' of integrated max, 200-ms refractory).
#'
#' @param x Numeric ECG vector.
#' @param fs Sampling rate (Hz, default 360 -- MIT-BIH).
#' @return Named list `r_peaks` (sample indices), `rr_intervals_ms`,
#'   `heart_rate_bpm`, `integrated`, `fs`.
#' @references Pan & Tompkins (1985), IEEE TBME 32:230. Rangayyan Ch 6.
#' @export
#' @examples
#' \donttest{
#' if (requireNamespace("signal", quietly = TRUE)) {
#'   fs <- 360
#'   t <- seq(0, 5, length.out = 5 * fs)
#'   ecg <- rowSums(sapply(
#'     seq(0.5, 4.5, by = 1.0),
#'     function(tk) exp(-((t - tk) * 30)^2)
#'   ))
#'   rgqrs(ecg, fs = fs)$r_peaks
#' }
#' }
rgqrs <- function(x, fs = 360.0) {
  x <- as.numeric(x)
  if (fs <= 0) stop(sprintf("`fs` must be positive, got %g.", fs))
  ## filtfilt needs more samples than its padlen, and a QRS detector needs
  ## enough signal to hold a beat regardless. One second is the floor: below
  ## it the 150 ms integration window and 200 ms refractory period are
  ## meaningless. Without this the failure came out of the filter internals
  ## with a message that says nothing about ECG.
  min_samples <- max(as.integer(round(fs)), 40L)
  if (length(x) < min_samples) {
    stop(sprintf(
      "need at least %d samples (1 s at fs=%g Hz) to detect QRS complexes, got %d.",
      min_samples, fs, length(x)
    ))
  }
  nyq <- 0.5 * fs
  bf <- .morie_dsp_butter(3L, c(5, min(15, nyq * 0.95)) / nyq, type = "pass")
  bp <- as.numeric(.morie_dsp_filtfilt(bf$b, bf$a, x))
  der <- numeric(length(bp))
  for (n in 5:length(bp)) {
    der[n] <- (1 / 8) * (2 * bp[n] + bp[n - 1] - bp[n - 3] - 2 * bp[n - 4])
  }
  sq <- der^2
  W <- max(1L, as.integer(round(0.150 * fs)))
  k <- rep(1 / W, W)
  integ <- as.numeric(stats::filter(sq, k, sides = 2))
  integ[is.na(integ)] <- 0
  N <- length(integ)
  refractory <- as.integer(round(0.200 * fs))
  thr <- 0.30 * max(integ, na.rm = TRUE)
  ## Match scipy.signal.find_peaks(height = thr, distance = refractory), which
  ## the Python side uses: collect every local maximum above the threshold,
  ## then resolve the minimum-separation constraint by keeping the TALLEST
  ## peak first and discarding any lower peak within `refractory` of one
  ## already kept.
  ##
  ## The previous scan walked left to right, kept the FIRST local maximum
  ## above the threshold, and skipped a refractory period. The 150 ms
  ## integration window is 38 samples wide at fs = 250, so its rising edge
  ## carries small local maxima that clear a 30% threshold well before the
  ## true apex. On a synthetic ECG with beats at samples 100, 300, 500, ...
  ## this latched onto 83 instead of 102, and the +/-50 ms refinement window
  ## [71, 95] could then not reach the R peak at 101 -- every reported peak
  ## came out 11 samples early, corrupting every RR interval and the heart
  ## rate derived from them.
  cand <- if (N >= 3L) {
    ii <- seq.int(2L, N - 1L)
    ii[integ[ii] > thr & integ[ii] > integ[ii - 1L] & integ[ii] >= integ[ii + 1L]]
  } else {
    integer(0)
  }
  peaks <- integer(0)
  for (p in cand[order(integ[cand], decreasing = TRUE)]) {
    if (all(abs(p - peaks) >= refractory)) peaks <- c(peaks, p)
  }
  peaks <- sort(peaks)
  # refine each to local |bp| max within +/-50 ms
  half <- as.integer(round(0.05 * fs))
  refined <- vapply(peaks, function(p) {
    lo <- max(1L, p - half)
    hi <- min(N, p + half)
    lo + which.max(abs(bp[lo:hi])) - 1L
  }, integer(1))
  rr_ms <- if (length(refined) > 1) diff(refined) * 1000 / fs else numeric(0)
  hr <- if (length(rr_ms)) 60000 / mean(rr_ms) else NA_real_
  list(
    r_peaks = refined, rr_intervals_ms = rr_ms,
    heart_rate_bpm = hr, integrated = integ, fs = fs
  )
}

#' @rdname rgqrs
#' @keywords internal
#' @export
morie_rangayyan_qrs_detect <- rgqrs

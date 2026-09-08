#' Butterworth IIR filter -- Rangayyan & Krishnan Sec 3.7.1 / 3.7.2
#'
#' Zero-phase Butterworth IIR filter (`butter` + `filtfilt`).
#'
#' KNOWN CROSS-LANGUAGE GAP, measured 2026-07-26. The transfer function
#' matches SciPy's `butter`: on 500 Gaussian samples at fs = 100 Hz with a
#' 10 Hz lowpass, the interior (samples 50-449) agrees with the Python path
#' to 1.8e-06. The EDGES do not. SciPy's `sosfiltfilt` pads with an odd
#' extension of length 3*(2*n_sections) before filtering; this native
#' `filtfilt` does not use the same padding, and the boundary samples differ
#' by up to 1.0e-01 (worst at sample 497). Interior results are
#' interchangeable; do not compare the first or last ~50 samples across
#' languages. Aligning the padding is tracked separately because
#' `.morie_dsp_filtfilt` is shared with rgqrs and rgfir.
#' The previous text claimed the two paths simply agree, without qualifying
#' the edges.
#'
#' @param x Numeric vector.
#' @param cutoff Cutoff frequency (Hz). For `"pass"`/`"stop"`, a length-2
#'   numeric vector `c(low, high)`.
#' @param order Filter order (default 4).
#' @param fs Sampling rate (Hz).
#' @param btype One of `"low"`, `"high"`, `"pass"` (bandpass), `"stop"`
#'   (bandstop).
#' @return Named list `signal`, `order`, `cutoff`, `fs`, `btype`.
#' @references Rangayyan, R. M., & Krishnan, S. (2024). Biomedical Signal
#'   Analysis, 3rd ed. Wiley-IEEE Press. Sec 3.7.1 "Removal of high-frequency
#'   noise: Butterworth lowpass filters", p.154; Sec 3.7.2 (highpass), p.161.
#' @export
#' @examples
#' \donttest{
#' if (requireNamespace("signal", quietly = TRUE)) {
#'   t <- seq(0, 1, length.out = 500)
#'   x <- sin(2 * pi * 5 * t) + 0.5 * sin(2 * pi * 40 * t)
#'   r <- rgiir(x, cutoff = 10, order = 4, fs = 500, btype = "low")
#'   length(r$signal)
#' }
#' }
rgiir <- function(x, cutoff, order = 4L, fs = 1.0, btype = c("low", "high", "pass", "stop")) {
  btype <- match.arg(btype)
  nyq <- 0.5 * fs
  ## Validate in the caller's own units. Without this the failure surfaced
  ## from inside the Butterworth design, naming neither `cutoff` nor `fs`;
  ## with the default fs = 1.0 any cutoff given in Hz above 0.5 trips it,
  ## which is the commonest caller mistake.
  if (any(!is.finite(cutoff)) || any(cutoff <= 0) || any(cutoff >= nyq)) {
    stop(sprintf(
      "cutoff must satisfy 0 < cutoff < fs/2 (Nyquist); got cutoff=%s with fs=%g (Nyquist=%g)",
      paste(format(cutoff), collapse = ", "), fs, nyq
    ))
  }
  if (length(cutoff) == 2L && !(cutoff[1] < cutoff[2])) {
    stop(sprintf("band cutoffs must be increasing, got cutoff=%s",
                 paste(format(cutoff), collapse = ", ")))
  }
  wn <- cutoff / nyq
  bf <- .morie_dsp_butter(as.integer(order), wn, type = btype)
  y <- as.numeric(.morie_dsp_filtfilt(bf$b, bf$a, x))
  list(
    signal = y, order = as.integer(order), cutoff = cutoff,
    fs = fs, btype = btype
  )
}

#' @rdname rgiir
#' @keywords internal
#' @export
morie_rangayyan_iir_filter <- rgiir

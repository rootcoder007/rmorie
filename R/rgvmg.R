# SPDX-License-Identifier: AGPL-3.0-or-later
#' Vibromyogram (VMG) signal characterization
#'
#' Rangayyan and Krishnan, Biomedical Signal Analysis, 3rd ed., Wiley-IEEE
#' Press, 2024.  Section 1.2.15 "The vibromyogram (VMG)", p. 52: "The VMG is
#' the direct mechanical manifestation of contraction of a skeletal muscle and
#' is a vibration signal ... The frequency and intensity of the VMG have been
#' shown to vary in direct proportion to the contraction level."  Section 2.2.6
#' "The EMG and VMG", p. 77: "it has been shown that the RMS and mean frequency
#' parameters of the VMG signal increase with muscle force output, in patterns
#' that parallel those of the EMG."  Section 5.11, p. 295: "The VMG signals
#' were filtered to the bandwidth 3 - 100 Hz ... sampled at 250 Hz".
#'
#' The two parameters the book names, intensity and frequency, are therefore
#' what this function reports: the RMS value of equation (3.9) and the mean
#' frequency of the power spectrum, together with the median frequency, the
#' zero-crossing rate, and the fraction of the power in the 3-100 Hz band.
#' The power spectrum is a periodogram computed by a direct discrete Fourier
#' transform rather than by a library FFT, so that both language arms evaluate
#' literally the same sums in the same order.
#'
#' @param vmg the vibromyogram.
#' @param fs sampling rate in Hz; 250 Hz in the book's experiment.
#' @param band the passband whose power fraction is reported; the 3-100 Hz
#'   band of Section 5.11 by default.
#' @return list: estimate, rms, ms, mean_frequency, median_frequency, zcr,
#'   crossings, band_power_fraction, band, freqs, psd, total_power, n, fs,
#'   method.
#' @keywords internal
#' @examples
#' t <- seq.int(0, 63) / 64
#' rgvmg(sin(2 * pi * 8 * t), 64)$mean_frequency
#' @export
rgvmg <- function(vmg, fs, band = NULL) {
  x <- as.numeric(vmg)
  N <- length(x)
  if (N < 2L) stop("rangayyan_vmg: need at least two samples")
  fsv <- as.numeric(fs)
  if (fsv <= 0) stop("rangayyan_vmg: fs must be positive")
  lo <- if (is.null(band)) 3 else as.numeric(band[1])
  hi <- if (is.null(band)) 100 else as.numeric(band[2])
  if (!(hi > lo) || lo < 0)
    stop("rangayyan_vmg: the band must be an increasing nonnegative pair")

  ms <- sum(x * x) / N                    # eq (3.9), divisor N
  rms <- sqrt(ms)

  s <- ifelse(x >= 0, 1, -1)              # Section 5.6.2
  crossings <- sum(s[-1] != s[-N])
  zcr <- crossings / (N - 1) * fsv

  half <- N %/% 2L
  freqs <- numeric(half + 1L)
  psd <- numeric(half + 1L)
  nn <- seq_len(N) - 1L
  for (j in 0:half) {
    a <- 2 * pi * j / N * nn
    re <- sum(x * cos(a))
    im <- -sum(x * sin(a))
    freqs[j + 1L] <- j * fsv / N
    psd[j + 1L] <- (re * re + im * im) / N
  }
  total <- sum(psd)
  if (total <= 0) {
    mean_f <- NA_real_; med_f <- NA_real_; frac <- NA_real_
  } else {
    mean_f <- sum(freqs * psd) / total
    run <- 0
    med_f <- freqs[length(freqs)]
    for (j in seq_along(psd)) {
      run <- run + psd[j]
      if (run >= 0.5 * total) { med_f <- freqs[j]; break }
    }
    frac <- sum(psd[freqs >= lo & freqs <= hi]) / total
  }
  list(estimate = rms, rms = rms, ms = ms,
       mean_frequency = mean_f, median_frequency = med_f,
       zcr = zcr, crossings = crossings, band_power_fraction = frac,
       band = c(lo, hi), freqs = freqs, psd = psd, total_power = total,
       n = N, fs = fsv,
       method = "Rangayyan (2024) Sects. 1.2.15 p.52, 2.2.6 p.77 and 5.11 p.295: RMS eq. (3.9) and mean frequency of the periodogram")
}

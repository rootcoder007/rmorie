#' EMG sliding-window RMS envelope -- Rangayyan Ch 8
#'
#' \deqn{\mathrm{RMS}\[n\] = \sqrt{ (1/W) \sum_{k=n-W+1}^{n} x\[k\]^2 }}{RMS\[n\] = sqrt{ (1/W) sum_k=n-W+1^n x\[k\]^2 }}
#'
#' @param x Numeric vector.
#' @param window Window length in samples (default 64).
#' @param fs Sampling rate (Hz, for reporting only).
#' @return Named list `rms`, `window`, `fs`, `mean_rms`.
#' @references Rangayyan, R. M., & Krishnan, S. (2024). Biomedical Signal
#'   Analysis, 3rd ed. Wiley-IEEE Press. Sec 5.6.1 "The RMS value",
#'   pp.283-284, eq (5.24) -- the running RMS, which is CAUSAL. The previous
#'   reference said Ch 8.
#' @export
#' @examples
#' set.seed(0)
#' r <- rgemg(rnorm(500), window = 32)
#' length(r$rms)
rgemg <- function(x, window = 64L, fs = 1.0) {
  W <- as.integer(window)
  if (W < 1) stop("window must be >= 1")
  N <- length(x)
  sq <- x^2
  csum <- c(0, cumsum(sq))
  rms <- rep(NA_real_, N)
  for (i in W:N) rms[i] <- sqrt((csum[i + 1] - csum[i + 1 - W]) / W)
  ## The first W-1 samples stay NA. Rangayyan & Krishnan eq (5.24) is a
  ## CAUSAL window -- RMS(n) averages x(n-k) for k = 0..M-1 -- so it is
  ## undefined until n = M-1 and the book defines no warm-up value.
  ## Back-filling with rms[W] used a value computed from samples in the
  ## FUTURE of those positions: a signal silent until sample 20 reported an
  ## envelope of 0.7651 at sample 0, i.e. the envelope rose 20 samples BEFORE
  ## the burst. EMG onset detection is the main use of an RMS envelope.
  list(rms = rms, window = W, fs = fs, mean_rms = mean(rms, na.rm = TRUE))
}

#' @rdname rgemg
#' @keywords internal
#' @export
morie_rangayyan_emg_rms <- rgemg

.rsg_dft <- function(seg, nf) {
  L <- length(seg)
  k <- 0:(L - 1)
  vapply(0:(nf - 1), function(m) {
    s <- 0i
    for (i in seq_len(L)) s <- s + seg[i] * exp(-2i * pi * m * k[i] / L)
    s
  }, 0i)
}

#' Reassigned spectrogram
#'
#' Auger and Flandrin (1995) reassignment: with \eqn{X_h} the STFT under the
#' periodic Hann window, \eqn{X_{th}} under the time-weighted window and
#' \eqn{X_{dh}} under the window derivative, the cell \eqn{(t, f)} is moved to
#' \eqn{\hat t = t + \mathrm{Re}(X_{th}/X_h)} and
#' \eqn{\hat f = f - \mathrm{Im}(X_{dh}/X_h)/(2\pi)}, the local group delay and
#' instantaneous frequency. Frames start at the first sample and advance by
#' \code{hop} without padding. Identical to the Python arm
#' \code{morie.fn.rssgm.reassigned_spectrogram}.
#'
#' @param x Numeric or complex signal.
#' @param fs Sampling frequency.
#' @param window Window length in samples (capped at the signal length).
#' @param hop Hop size in samples.
#' @return A list with \code{value} (largest magnitude), \code{magnitude}
#'   (frequencies by frames), \code{frequencies}, \code{times} (frame
#'   centres), \code{t_reassigned} and \code{f_reassigned}.
#' @references Auger, F. and Flandrin, P. (1995). Improving the readability of
#'   time-frequency and time-scale representations by the reassignment
#'   method. IEEE Transactions on Signal Processing 43, 1068-1089.
#' @examples
#' tone <- cos(2 * pi * 0.25 * (0:63))
#' reassigned_spectrogram(tone, fs = 1, window = 16, hop = 8)$f_reassigned[5, 1]
#' @export
reassigned_spectrogram <- function(x, fs = 1, window = 256, hop = 128) {
  xs <- as.complex(x)
  n <- length(xs)
  L <- as.integer(min(window, n))
  hop <- as.integer(hop)
  if (L < 2 || hop < 1) stop("need window >= 2 samples and hop >= 1")
  k <- 0:(L - 1)
  h <- 0.5 - 0.5 * cos(2 * pi * k / L)
  th <- (k - L / 2) / fs * h
  dh <- pi * fs / L * sin(2 * pi * k / L)
  nf <- L %/% 2 + 1
  freqs <- (0:(nf - 1)) * fs / L
  starts <- seq(0, n - L, by = hop)
  nt <- length(starts)
  M <- RT <- RF <- matrix(0, nf, nt)
  times <- (starts + L / 2) / fs
  for (j in seq_len(nt)) {
    seg <- xs[starts[j] + seq_len(L)]
    Xh <- .rsg_dft(seg * h, nf)
    Xt <- .rsg_dft(seg * th, nf)
    Xd <- .rsg_dft(seg * dh, nf)
    peak <- max(Mod(Xh))
    if (peak == 0) peak <- 1
    M[, j] <- Mod(Xh)
    ok <- Mod(Xh) > 1e-10 * peak
    RT[, j] <- ifelse(ok, times[j] + Re(Xt / Xh), times[j])
    RF[, j] <- ifelse(ok, freqs - Im(Xd / Xh) / (2 * pi), freqs)
  }
  list(value = max(M), magnitude = M, frequencies = freqs, times = times,
       t_reassigned = RT, f_reassigned = RF)
}

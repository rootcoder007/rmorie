# SPDX-License-Identifier: AGPL-3.0-or-later

#' Discrete prolate spheroidal (Slepian) sequences
#'
#' The Slepian tapers are the eigenvectors of the symmetric tridiagonal
#' matrix with diagonal ((M - 1 - 2i) / 2)^2 cos(2 pi W) and
#' off-diagonal i (M - i) / 2, W = NW / M (Percival and Walden 1993,
#' Ch. 8); the k-th taper belongs to the k-th largest eigenvalue.
#' Signs follow scipy.signal.windows.dpss: symmetric tapers sum
#' positive and antisymmetric tapers start with a positive lobe.
#' The concentration ratio lambda_k = v' S v is the fraction of the
#' taper's energy inside [-W, W].
#'
#' @param M Integer taper length.
#' @param NW Time-bandwidth product.
#' @param Kmax Number of tapers; \code{NULL} returns one taper peak
#'   normalised as scipy's \code{norm = "approximate"}.
#' @param return_ratios Logical; also return the concentration ratios.
#' @return A \code{Kmax x M} matrix of unit-energy tapers (a vector when
#'   \code{Kmax} is \code{NULL}), or a list with \code{tapers} and
#'   \code{ratios} when \code{return_ratios = TRUE}.
#' @examples
#' w <- morie_dpss(64, 3, Kmax = 4, return_ratios = TRUE)
#' round(w$ratios, 6)
#' @export
morie_dpss <- function(M, NW, Kmax = NULL, return_ratios = FALSE) {
  M <- as.integer(M)
  if (M < 2L) stop("M must be at least 2.", call. = FALSE)
  W <- NW / M
  i <- 0:(M - 1L)
  dg <- ((M - 1 - 2 * i) / 2)^2 * cos(2 * pi * W)
  off <- (1:(M - 1L)) * (M - (1:(M - 1L))) / 2
  A <- diag(dg)
  A[cbind(1:(M - 1L), 2:M)] <- off
  A[cbind(2:M, 1:(M - 1L))] <- off
  e <- eigen(A, symmetric = TRUE)
  k <- if (is.null(Kmax)) 1L else as.integer(Kmax)
  tapers <- matrix(0, k, M)
  thresh <- max(1e-7, 1 / M)
  for (kk in seq_len(k)) {
    v <- e$vectors[, kk]
    v <- v / sqrt(sum(v^2))
    flip <- if ((kk - 1L) %% 2L == 0L) sum(v) < 0 else v[which(v^2 > thresh)[1L]] < 0
    if (isTRUE(flip)) v <- -v
    if (is.null(Kmax)) {
      corr <- if (M %% 2L == 0L) M^2 / (M^2 + NW) else 1
      v <- v / max(abs(v)) * corr
    }
    tapers[kk, ] <- v
  }
  if (!return_ratios) {
    if (is.null(Kmax)) return(tapers[1L, ])
    return(tapers)
  }
  ratios <- vapply(seq_len(k), function(kk) {
    v <- tapers[kk, ]
    nrm <- sum(v^2)
    acc <- 2 * W * nrm
    for (lag in seq_len(M - 1L)) {
      r <- sum(v[1:(M - lag)] * v[(1 + lag):M])
      acc <- acc + 2 * r * sin(2 * pi * W * lag) / (pi * lag)
    }
    acc / nrm
  }, numeric(1))
  if (is.null(Kmax)) return(list(tapers = tapers[1L, ], ratios = ratios))
  list(tapers = tapers, ratios = ratios)
}

#' Multitaper power spectral density
#'
#' Thomson's multitaper estimate: the eigenvalue-weighted average of the
#' periodograms of the signal tapered by the first K Slepian sequences,
#' sum_k lambda_k |X_k(f)|^2 / (fs sum_k lambda_k), returned as a
#' one-sided density (every bin except DC, and Nyquist for even nfft,
#' doubled, as in scipy.signal.periodogram) so that it integrates to the
#' variance of the signal.
#'
#' @param x Numeric signal.
#' @param fs Sampling frequency in Hz.
#' @param nw Time-bandwidth product.
#' @param n_tapers Number of tapers; \code{2 nw - 1} by default.
#' @param nfft FFT length; \code{length(x)} by default.
#' @return List with \code{value} (the integrated power), \code{frequencies},
#'   \code{psd}, \code{n_tapers}, \code{concentrations}.
#' @examples
#' x <- sin(2 * pi * 10 * (0:255) / 100)
#' r <- morie_psdmt(x, fs = 100)
#' r$frequencies[which.max(r$psd)]
#' @export
morie_psdmt <- function(x, fs = 1, nw = 4, n_tapers = NULL, nfft = NULL) {
  x <- as.numeric(x)
  n <- length(x)
  if (is.null(n_tapers)) n_tapers <- as.integer(2 * nw - 1)
  if (is.null(nfft)) nfft <- n
  nfft <- as.integer(nfft)
  w <- morie_dpss(n, nw, Kmax = n_tapers, return_ratios = TRUE)
  nfreqs <- nfft %/% 2L + 1L
  psd <- numeric(nfreqs)
  for (k in seq_len(nrow(w$tapers))) {
    xk <- c(w$tapers[k, ] * x, numeric(max(nfft - n, 0L)))[seq_len(nfft)]
    psd <- psd + w$ratios[k] * Mod(stats::fft(xk)[seq_len(nfreqs)])^2
  }
  psd <- psd / (fs * sum(w$ratios))
  dbl <- rep(2, nfreqs)
  dbl[1L] <- 1
  if (nfft %% 2L == 0L) dbl[nfreqs] <- 1
  psd <- psd * dbl
  freqs <- (0:(nfreqs - 1L)) * fs / nfft
  value <- sum(diff(freqs) * (psd[-1L] + psd[-nfreqs]) / 2)
  list(value = value, frequencies = freqs, psd = psd,
       n_tapers = nrow(w$tapers), concentrations = w$ratios)
}

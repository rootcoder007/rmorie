# SPDX-License-Identifier: AGPL-3.0-or-later
#' Fourier basis in the Ramsay-Silverman normalisation
#'
#' Ramsay and Silverman (2005), Functional Data Analysis, 2nd ed., Springer,
#' Chapter 3, Section 3.3.1 "The Fourier basis system for periodic data": the
#' basis is phi_0(t) = 1, phi_\{2r-1\}(t) = sin(r omega t),
#' phi_\{2r\}(t) = cos(r omega t), with omega = 2 pi / P and P the period, taken
#' here as the range of t when it is not supplied.
#'
#' This is the UNNORMALISED form of Section 3.3.1.  The 1/sqrt(P) and
#' sqrt(2/P) scaled variant lives in Fours, which follows the Montesinos
#' Lopez normalisation instead.  The two must not be confused: they differ by
#' column scaling only, but the scaling changes every coefficient.
#'
#' @param t evaluation points.
#' @param K number of sine/cosine pairs; the basis has 2*K + 1 functions.
#' @param period P; defaults to the range of t.
#' @return list: estimate, Phi, omega, period, n, nbasis, method.
#' @keywords internal
#' @examples
#' Fhar(seq(0, 1, length.out = 5), 1, 1)$omega
#' @export
Fhar <- function(t, K, period = NULL) {
  tt <- .s03vec(t)
  n <- length(tt)
  if (n == 0L) stop("fourier_basis: t is empty")
  KK <- as.integer(K)
  if (is.na(KK) || KK < 0L) stop("fourier_basis: K must be non-negative")
  P <- if (is.null(period)) max(tt) - min(tt) else as.numeric(period)
  if (P <= 0) stop("fourier_basis: the period must be positive")
  w <- 2 * pi / P
  Phi <- matrix(0, n, 2L * KK + 1L)
  for (i in seq_len(n)) {
    Phi[i, 1] <- 1
    if (KK > 0L) for (r in seq_len(KK)) {
      Phi[i, 2L * r] <- sin(r * w * tt[i])
      Phi[i, 2L * r + 1L] <- cos(r * w * tt[i])
    }
  }
  list(estimate = Phi[1, 1], Phi = Phi, omega = w, period = P, n = n,
       nbasis = 2L * KK + 1L,
       method = "Ramsay-Silverman (2005) Sect. 3.3.1 Fourier basis, 1, sin(r w t), cos(r w t)")
}

# SPDX-License-Identifier: AGPL-3.0-or-later
#' Multi-level Daubechies wavelet decomposition of a periodic signal
#'
#' SOURCE. Daubechies, I. (1992), Ten Lectures on Wavelets, CBMS-NSF
#' Regional Conference Series in Applied Mathematics 61, SIAM;
#' doi:10.1137/1.9781611970104. Chapter 5 is the multiresolution analysis
#' and the pyramid (Mallat) algorithm it implies: with scaling filter h
#' and quadrature mirror g_k = (-1)^k h_\{L-1-k\}, one level of the periodic
#' decomposition of a length-M sequence is
#' a_\{j+1\}\[i\] = sum_k h_k a_j\[(2i+k) mod M\] and
#' d_\{j+1\}\[i\] = sum_k g_k a_j\[(2i+k) mod M\], i = 0 ... M/2-1, repeated on
#' the approximation. The filters are those of Daubechies (1988),
#' doi:10.1002/cpa.3160410705, taken from \code{\link{Wave}} rather than
#' re-derived; that module carries the algebraic checks on them.
#'
#' The synthesis step is the exact adjoint,
#' a_j\[(2i+k) mod M\] += h_k a_\{j+1\}\[i\] + g_k d_\{j+1\}\[i\], which
#' reconstructs exactly because analysis is orthonormal.
#' \code{reconstruction_error} is max |x - synth(analyse(x))|.
#'
#' SCOPE. Periodic (circular) boundary handling only, and signal length a
#' power of two. Both are this implementation's scope choices.
#'
#' @param y Signal of length 2^J, J at least 1.
#' @param level Number of decomposition levels; default the full J.
#' @param wavelet "db1", "db2" or "db3".
#' @return List with \code{details}, \code{approximation},
#'   \code{coefficients}, \code{energies}, \code{approximation_energy},
#'   \code{reconstruction}, \code{reconstruction_error}, \code{h},
#'   \code{g}, \code{n}, \code{level}.
#' @references Daubechies, I. (1992). Ten Lectures on Wavelets. SIAM.
#'   doi:10.1137/1.9781611970104. Daubechies, I. (1988). Communications
#'   on Pure and Applied Mathematics 41(7):909-996.
#'   doi:10.1002/cpa.3160410705.
#' @examples
#' Wvltdb(c(1, 2, 3, 4, 5, 6, 7, 8), 2, "db1")$reconstruction_error
#' @export
Wvltdb <- function(y, level = NULL, wavelet = "db2") {
  x <- .s03vec(y)
  n <- length(x)
  J <- .dbpow2(n)
  if (J < 1L) stop("db_wavelet: length of y must be a power of two, at least 2")
  lev <- if (is.null(level)) J else as.integer(level)
  if (is.na(lev) || lev < 1L || lev > J) {
    stop("db_wavelet: level must lie in 1 .. log2(n)")
  }
  h <- .dbfilter(wavelet)
  g <- .dbmirror(h)
  if (length(h) > n %/% (2^(lev - 1L))) {
    stop("db_wavelet: filter is longer than the coarsest level")
  }
  a <- x
  details <- vector("list", lev)
  for (j in seq_len(lev)) {
    st <- .dbstep(a, h, g)
    a <- st$a
    details[[j]] <- st$d
  }
  packed <- a
  for (j in seq(lev, 1L)) packed <- c(packed, details[[j]])
  rec <- a
  for (j in seq(lev, 1L)) rec <- .dbsynth(rec, details[[j]], h, g)
  err <- 0
  for (i in seq_len(n)) {
    e <- abs(rec[i] - x[i])
    if (e > err) err <- e
  }
  energies <- vapply(details, function(d) sum(d * d), numeric(1))
  ea <- sum(a * a)
  .t1_result(estimate = ea, details = details, approximation = a,
             coefficients = packed, energies = energies,
             approximation_energy = ea, reconstruction = rec,
             reconstruction_error = err, h = h, g = g, n = n, level = lev,
             method = paste("Periodic pyramid (Mallat) multiresolution",
                            "decomposition, Daubechies (1992) Ch. 5"))
}

# Exact adjoint of .dbstep.
#' Exact adjoint of .dbstep
#'
#' A step of the wvltdb implementation. Called by \code{Wvltdb}.
#' See the file header for the source the module follows.
#' it follows.
#'
#' @param a A vector; its length is taken and its elements indexed.
#' @param d A vector; indexed elementwise.
#' @param h A vector; its length is taken and its elements indexed.
#' @param g A vector; indexed elementwise.
#' @return The value of \code{out}, as built in the body.
#' @export
.dbsynth <- function(a, d, h, g) {
  m <- length(a)
  n <- 2L * m
  L <- length(h)
  out <- numeric(n)
  for (i in seq_len(m) - 1L) {
    for (k in seq_len(L) - 1L) {
      ix <- (2L * i + k) %% n + 1L
      out[ix] <- out[ix] + h[k + 1L] * a[i + 1L] + g[k + 1L] * d[i + 1L]
    }
  }
  out
}

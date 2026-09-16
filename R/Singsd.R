# SPDX-License-Identifier: AGPL-3.0-or-later
#' Singular spectrum analysis of a univariate series
#'
#' Decomposes a series into components by diagonalising its lag-covariance
#' matrix, then reconstructs each one.  With window (embedding dimension) M
#' and series length N, Vautard, Yiou & Ghil use the Toeplitz estimate of the
#' lag-covariance matrix,
#' \code{c_j = sum_{t=1}^{N-j} z_t z_{t+j} / (N - j)} and
#' \code{C_ij = c_{|i-j|}}, on the centred series \code{z = y - mean(y)}.
#' This is the estimator they recommend over the trajectory-matrix
#' (Broomhead-King) form because it is unbiased term by term and keeps C
#' Toeplitz, so its eigenvectors stay close to sines and cosines for a
#' quasi-periodic signal.  C is symmetric and is diagonalised by Jacobi
#' rotations, eigenvalues sorted decreasing.
#'
#' The k-th principal component is the projection of the delay vectors on
#' eigenvector k, \code{a^k_t = sum_j z_{t+j-1} E_jk} for
#' \code{t = 1..N-M+1}, and the reconstructed component is recovered by
#' diagonal averaging of \code{a^k_t E_jk} over the delay index with the
#' boundary-corrected normalisation.  The reconstructed components sum
#' exactly to the centred series, reported as
#' \code{reconstruction_error}.
#'
#' Since \code{trace(C) = M c_0 = M var(y)}, the eigenvalues sum to M times
#' the series variance; \code{trace_check} reports that identity.  The
#' Toeplitz estimate is not guaranteed positive semi-definite -- each
#' \code{c_j} carries its own divisor \code{N - j} -- so a few trailing
#' eigenvalues may come out slightly negative and an individual variance
#' fraction may exceed 1.  That is a property of the estimator Vautard,
#' Yiou & Ghil chose, not an error.
#'
#' @param y Series, length N; needs \code{N > window}.
#' @param window Embedding dimension M, at least 2 and at most \code{N - 1}.
#' @return List with \code{estimate} (leading variance fraction),
#'   \code{eigenvalues}, \code{variance_fraction}, \code{reconstructed},
#'   \code{leading_fraction}, \code{pair_fraction}, \code{total_variance},
#'   \code{trace_check}, \code{reconstruction_error}, \code{c0},
#'   \code{mean}, \code{n}, \code{window}, \code{n_lagged}, \code{method}.
#' @references Vautard, R., Yiou, P. & Ghil, M. (1992). Singular-spectrum
#'   analysis: a toolkit for short, noisy chaotic signals. Physica D
#'   58(1-4), 95-126. \doi{10.1016/0167-2789(92)90103-T}
#' @export
#' @examples
#' Singsd(y = c(1, 2, 3, 4, 5, 6, 7, 8), window = 5L)
Singsd <- function(y, window) {
  x <- .s03vec(y)
  N <- length(x)
  M <- as.integer(window)
  if (N < 3L) stop("singular_spectrum: y must have at least 3 points")
  if (M < 2L || M > N - 1L)
    stop("singular_spectrum: window must satisfy 2 <= window <= len(y) - 1")

  mu <- sum(x) / N
  z <- x - mu

  cvec <- numeric(M)
  for (j in 0:(M - 1L)) {
    s <- 0
    for (t in seq_len(N - j)) s <- s + z[t] * z[t + j]
    cvec[j + 1L] <- s / (N - j)
  }
  C <- matrix(0, M, M)
  for (i in seq_len(M)) for (j in seq_len(M)) C[i, j] <- cvec[abs(i - j) + 1L]

  jc <- .s03jacobi(C)
  vals <- jc$values
  vecs <- jc$vectors
  ord <- rev(seq_len(M))
  lam <- vals[ord]
  E <- vecs[, ord, drop = FALSE]

  K <- N - M + 1L
  A <- matrix(0, K, M)
  for (t in seq_len(K)) {
    for (k in seq_len(M)) {
      s <- 0
      for (j in seq_len(M)) s <- s + z[t + j - 1L] * E[j, k]
      A[t, k] <- s
    }
  }

  reconstruct <- function(k) {
    Rv <- numeric(N)
    for (t in seq_len(N)) {
      lo <- max(1L, t - K + 1L)
      hi <- min(M, t)
      s <- 0
      cnt <- 0L
      for (j in lo:hi) {
        s <- s + A[t - j + 1L, k] * E[j, k]
        cnt <- cnt + 1L
      }
      Rv[t] <- s / cnt
    }
    Rv
  }

  R1 <- reconstruct(1L)
  total <- sum(lam)
  frac <- if (total > 0) lam / total else rep(NA_real_, M)

  full <- numeric(N)
  for (k in seq_len(M)) full <- full + reconstruct(k)
  rec_err <- max(abs(full - z))

  .t1_result(estimate = frac[1], eigenvalues = lam, variance_fraction = frac,
             reconstructed = R1, leading_fraction = frac[1],
             pair_fraction = if (M > 1L) frac[1] + frac[2] else frac[1],
             total_variance = total, trace_check = abs(total - M * cvec[1]),
             reconstruction_error = rec_err, c0 = cvec[1], mean = mu,
             n = N, window = M, n_lagged = K,
             method = "Singular spectrum analysis, Toeplitz lag covariance (Vautard, Yiou & Ghil 1992)")
}

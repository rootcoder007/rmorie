# SPDX-License-Identifier: AGPL-3.0-or-later
#' Fourier basis function expansion
#'
#' Montesinos Lopez, Montesinos Lopez and Crossa (2022), Multivariate
#' Statistical Machine Learning Methods for Genomic Prediction, Springer,
#' volume [Pages 579-631], Chapter 14, Section 14.2.1, p. 584, read as a
#' rendered page image rather than from an extracted text layer.
#'
#' Erratum in the book.  The typeset list on p. 584 prints phi_2 and phi_3
#' both as sin(w t)/sqrt(P/2), phi_4 and phi_5 both as cos(2w t)/sqrt(P/2),
#' and phi_6 and phi_7 both as cos(3w t)/sqrt(P/2), so as printed the list
#' repeats itself.  Three things on the same page show the intended list
#' alternates sine and cosine: a set with repeated elements is linearly
#' dependent and is not a basis, which is what the page defines; the page
#' says the graph "of the first five of these functions" is Fig. 14.1, which
#' plots five distinct curves, not three; and the R code the page gives to
#' reproduce that figure is create.fourier.basis(rangeval = c(0, 8),
#' nbasis = 5, period = 4) from fda, whose basis alternates.  The alternating
#' reading is implemented: phi_1 = 1/sqrt(P), phi_{2h} = sin(h w t)/sqrt(P/2),
#' phi_{2h+1} = cos(h w t)/sqrt(P/2).
#'
#' "w is related to period P by w = 2 pi / P, and in practical applications,
#' this is often taken as the range of t values where the data are observed",
#' which is the default period.
#'
#' @param t the points at which to evaluate the basis.
#' @param n_harmonics number of sine/cosine pairs; 2*n_harmonics+1 functions.
#' @param period P; defaults to the range of t, as Section 14.2.1 suggests.
#' @return list: estimate, F, omega, period, n, method.
#' @keywords internal
#' @examples
#' Fours(seq(0, 8, length.out = 9), 2, 4)$omega
#' @export
Fours <- function(t, n_harmonics, period = NULL) {
  tt <- .s03vec(t)
  n <- length(tt)
  if (n == 0L) stop("fourier_basis: t is empty")
  H <- as.integer(n_harmonics)
  if (is.na(H) || H < 0L) stop("fourier_basis: n_harmonics must be non-negative")
  P <- if (is.null(period)) max(tt) - min(tt) else as.numeric(period)
  if (P <= 0) stop("fourier_basis: the period must be positive")
  w <- 2 * pi / P
  c0 <- 1 / sqrt(P)
  ck <- 1 / sqrt(P / 2)
  Fm <- matrix(0, n, 2L * H + 1L)
  for (i in seq_len(n)) {
    Fm[i, 1] <- c0
    if (H > 0L) for (h in seq_len(H)) {
      Fm[i, 2L * h] <- ck * sin(h * w * tt[i])
      Fm[i, 2L * h + 1L] <- ck * cos(h * w * tt[i])
    }
  }
  list(estimate = Fm[1, 1], F = Fm, omega = w, period = P, n = n,
       method = "Chapter 14 Sect. 14.2.1 Fourier basis, phi_1 = 1/sqrt(P), sin/cos pairs scaled by 1/sqrt(P/2)")
}

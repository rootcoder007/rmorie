# SPDX-License-Identifier: AGPL-3.0-or-later
#' Gaussian process RKHS, Euclidean case
#'
#' For W ~ N(0, Sigma) on R^k the reproducing-kernel Hilbert space is the
#' range of Sigma, carrying the inner product
#' <Sigma a, Sigma b>_H = a' Sigma b.  This finite-dimensional case is
#' the one where every object in the general definition can be written
#' down, which is why the book uses it to fix intuition; the reproducing
#' formula h(t) = <h, K(t, .)>_H is checked coordinatewise here.
#'
#' Formula: <Sigma a, Sigma b>_H = a' Sigma b;  h = Sigma a;
#'   K(t, .) = Sigma e_t, so <h, K(t, .)>_H = a' Sigma e_t = h(t).
#'
#' @param Sigma Covariance matrix, k by k.
#' @param a,b Coefficient vectors of length k.
#' @return List with \code{estimate} (the inner product), \code{h},
#'   \code{reproducing_gap}, \code{method}.
#' @references Ghosal & van der Vaart (2017), Fundamentals of
#'   Nonparametric Bayesian Inference, CUP, Example 11.15 and
#'   Definition 11.12, eq. (11.8).
#' @export
Ghosalgpdefrkhs <- function(Sigma, a, b) {
  S <- as.matrix(Sigma)
  a <- as.numeric(a)
  b <- as.numeric(b)
  k <- length(a)
  if (length(b) != k) stop("a and b must have the same length")
  if (!all(dim(S) == c(k, k))) stop("Sigma must be k by k with k = length(a)")
  ip <- as.numeric(t(a) %*% S %*% b)
  h <- as.numeric(S %*% a)
  gaps <- abs(as.numeric(t(a) %*% S) - h)
  .t1_result(estimate = ip, h = h, reproducing_gap = max(gaps),
             method = "Euclidean RKHS (GvdV 2017 Ex 11.15, eq. 11.8)")
}

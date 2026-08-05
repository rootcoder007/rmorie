# SPDX-License-Identifier: AGPL-3.0-or-later
#' Series (eigenexpansion) Gaussian process
#'
#' K(x, y) = sum_k lambda_k phi_k(x) phi_k(y).  Mercer's theorem says
#' every continuous kernel has such an expansion, and the random series
#' W = sum_k a_k(t) Z_k with independent standard normal Z_k has exactly
#' that covariance -- so a series prior and a Gaussian process prior are
#' the same object viewed two ways, which is what makes the RKHS
#' computations of the chapter tractable.  Cosine eigenbasis with
#' lambda_k = k^(-2) here; the partial sums converge.
#'
#' Formula: K(x, y) = sum_k k^(-2) phi_k(x) phi_k(y),
#'   phi_k(t) = sqrt(2) cos(k pi t).
#'
#' @param x,y Evaluation points.
#' @param n_terms Number of terms summed; at least 3 so the convergence
#'   check has three partial sums to compare.
#' @return List with \code{estimate} (the kernel value),
#'   \code{partial_sums}, \code{converging}, \code{method}.
#' @references Ghosal & van der Vaart (2017), Fundamentals of
#'   Nonparametric Bayesian Inference, CUP, Examples 11.4 and 11.16.
#' @export
Ghosalseriesgp <- function(x = 0.3, y = 0.7, n_terms = 60) {
  n_terms <- as.integer(n_terms)
  if (n_terms < 3L) stop("n_terms must be at least 3")
  k <- seq_len(n_terms)
  phi <- function(t, k) sqrt(2) * cos(k * pi * t)
  run <- cumsum(k^(-2) * phi(x, k) * phi(y, k))
  tot <- run[n_terms]
  keep <- c(5L, 20L, n_terms)
  keep <- keep[keep <= n_terms]
  partial <- run[keep]
  m <- base::length(partial)
  conv <- if (m < 3L) NA else
    abs(partial[m] - partial[m - 1]) <
      abs(partial[m - 1] - partial[m - 2]) + 1e-12
  .t1_result(estimate = tot, partial_sums = partial,
             converging = conv,
             method = "eigenexpansion GP kernel (GvdV 2017 Ex 11.16)")
}

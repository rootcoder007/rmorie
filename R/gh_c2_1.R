# SPDX-License-Identifier: AGPL-3.0-or-later
#' Random basis expansion prior
#'
#' f = sum_k z_k phi_k with INDEPENDENT coefficients.  Independence plus a
#' decaying coefficient variance is what makes the series converge almost
#' surely, so the decay rate is not a tuning choice but the thing that
#' decides whether the prior lives on a function space at all.
#'
#' Formula: z_k = Z_k (k+1)^(-decay), Z_k iid N(0, 1);
#'   f(x) = sum_k z_k cos(pi (k+1) x).
#'
#' @param x Evaluation points.
#' @param n_terms Number of basis terms.
#' @param seed Seed for the deterministic draws.
#' @param decay Coefficient decay exponent, positive.
#' @return List with \code{estimate} (mean of f), \code{f},
#'   \code{coefficients}, \code{method}.
#' @references Ghosal & van der Vaart (2017), Fundamentals of
#'   Nonparametric Bayesian Inference, CUP, section 2.1.
#' @export
Ghosalrandombasisexpansion <- function(x, n_terms = 12, seed = 42,
                                       decay = 1.5) {
  xs <- as.numeric(x)
  n_terms <- as.integer(n_terms)
  if (length(xs) == 0L) stop("x must be non-empty")
  if (n_terms < 1L) stop("n_terms must be positive")
  e <- .ghc_rng(seed)
  k <- seq_len(n_terms)
  z <- .ghc_norm(e, n_terms) * k^(-decay)
  f <- as.numeric(outer(xs, k, function(a, b) cos(pi * b * a)) %*% z)
  .t1_result(estimate = mean(f), f = f, coefficients = z,
             method = "random cosine-basis expansion (GvdV 2017 sec. 2.1)")
}

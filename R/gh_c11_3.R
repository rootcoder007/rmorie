# SPDX-License-Identifier: AGPL-3.0-or-later
#' Gaussian contraction-rate equation
#'
#' The contraction rate of a Gaussian process prior is the smallest eps
#' solving phi_{w0}(eps) <= n eps^2.  When the concentration function
#' behaves like eps^(-a) -- a = 2 for Brownian motion, Lemma 11.27 --
#' the solution is eps_n = n^(-1/(2+a)), i.e. n^(-1/4) for BM.  Nothing
#' about the process enters beyond that single exponent, which is why
#' the rate equation is stated once and reused for every prior in the
#' chapter.
#'
#' Formula: eps_n = n^(-1/(2 + a)), the point where eps^(-a) = n eps^2.
#'
#' @param phi_exponent The exponent a in phi(eps) ~ eps^(-a).
#' @param n Sample size.
#' @return List with \code{estimate} (eps_n), \code{rate_exponent},
#'   \code{balance_gap}, \code{method}.
#' @references Ghosal & van der Vaart (2017), Fundamentals of
#'   Nonparametric Bayesian Inference, CUP, Theorem 11.20, eq. (11.12).
#' @export
#' @examples
#' Ghosalgpcrtthm()
Ghosalgpcrtthm <- function(phi_exponent = 2, n = 10000) {
  a <- as.numeric(phi_exponent)
  n <- as.numeric(n)
  if (a <= -2) stop("phi_exponent must exceed -2")
  if (n <= 0) stop("n must be positive")
  eps_n <- n^(-1 / (2 + a))
  gap <- abs(eps_n^(-a) - n * eps_n^2) / (n * eps_n^2)
  .t1_result(estimate = eps_n, rate_exponent = 1 / (2 + a),
             balance_gap = gap,
             method = "Gaussian rate equation (GvdV 2017 Thm 11.20)")
}

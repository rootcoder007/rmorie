# SPDX-License-Identifier: AGPL-3.0-or-later
#' Concentration-function terms
#'
#' phi_\{w0\}(eps) = inf\{ ||h||_H^2 / 2 : ||h - w0|| <= eps \}
#'   - log P(||W|| < eps).
#' The two terms are the whole of Gaussian contraction theory: a
#' decentering (RKHS approximation) term and a small-ball term, and the
#' rate is where they balance.  For a diagonal series process the
#' infimum is attained by keeping the largest coordinates of w0 and
#' zeroing the rest, so the first term is a finite sum; the small-ball
#' probability is estimated on the l2 norm by Monte Carlo.
#'
#' Formula: ||h||_H^2 = sum_\{i kept\} f0_i^2 / lambda_i;
#'   small-ball exponent = -log P(sqrt(sum_i lambda_i Z_i^2) < eps).
#'
#' @param f0_coefs Coefficients of the target w0.
#' @param lambdas Coordinate variances of the process, all positive.
#' @param eps Radius.
#' @param n_sim Monte Carlo draws for the small-ball term.
#' @param seed Seed for the deterministic draws.
#' @return List with \code{estimate} (phi), \code{decentering_norm2},
#'   \code{small_ball_exponent}, \code{method}.
#' @references Ghosal & van der Vaart (2017), Fundamentals of
#'   Nonparametric Bayesian Inference, CUP, eq. (11.11).
#' @export
#' @examples
#' Ghosalrkhsnorm(f0_coefs = c(1, 2, 3, 4, 5, 6, 7, 8), lambdas = c(1, 2, 3, 4, 5, 6, 7,
#' 8), eps = 0.5)
Ghosalrkhsnorm <- function(f0_coefs, lambdas, eps, n_sim = 3000,
                           seed = 42) {
  f0 <- as.numeric(f0_coefs)
  lam <- as.numeric(lambdas)
  if (length(f0) != length(lam))
    stop("f0_coefs and lambdas must have the same length")
  if (length(f0) == 0L) stop("f0_coefs must be non-empty")
  if (any(lam <= 0)) stop("every lambda must be positive")
  if (eps <= 0) stop("eps must be positive")
  ord <- order(-abs(f0))
  h <- numeric(length(f0))
  hn2 <- 0
  for (i in ord) {
    if (sqrt(sum((f0 - h)^2)) <= eps) break
    h[i] <- f0[i]
    hn2 <- hn2 + f0[i]^2 / lam[i]
  }
  e <- .ghc_rng(seed)
  hits <- 0L
  for (it in seq_len(n_sim)) {
    z <- .ghc_norm(e, length(lam))
    if (sqrt(sum(lam * z * z)) < eps) hits <- hits + 1L
  }
  small_ball <- -log(max(hits, 1) / n_sim)
  .t1_result(estimate = 0.5 * hn2 + small_ball,
             decentering_norm2 = hn2,
             small_ball_exponent = small_ball,
             method = "concentration function (GvdV 2017 eq. 11.11)")
}

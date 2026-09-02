# SPDX-License-Identifier: AGPL-3.0-or-later
#' Random-series binary regression
#'
#' P(Y = 1 | x) = Phi(f(x)) with f a finite random series gives the same
#' adaptive rate as the density case.  The fit here is a damped gradient
#' ascent on the logistic MAP objective (a link with matched slope), which
#' is enough to show the posterior mean orders the covariate correctly:
#' the fitted probability crosses one half where the truth does.
#'
#' Formula: f(x) = sum_k beta_k phi_k(x), phi_0 = 1,
#'   phi_k(x) = sqrt(2) cos(k pi x); beta <- beta + step grad - decay beta,
#'   grad_k = sum_i (y_i - p_i) phi_k(x_i).
#'
#' @param n Sample size.
#' @param K Number of basis terms.
#' @param seed Seed for the deterministic draws.
#' @return List with \code{estimate} (deviation of the fit at x = 1/2
#'   from one half), \code{orders_correctly}, \code{p_low_high},
#'   \code{method}.
#' @references Ghosal & van der Vaart (2017), Fundamentals of
#'   Nonparametric Bayesian Inference, CUP, section 10.4.3.
#' @export
#' @examples
#' Ghosalfrsbinreg()
Ghosalfrsbinreg <- function(n = 800, K = 4, seed = 42) {
  n <- as.integer(n); K <- as.integer(K)
  if (n < 1L) stop("n must be positive")
  if (K < 1L) stop("K must be positive")
  e <- .ghc_rng(seed)
  xs <- .ghc_unif(e, n)
  F0 <- 1 / (1 + exp(-4 * (xs - 0.5)))
  ys <- as.numeric(.ghc_unif(e, n) < F0)
  phi <- function(x, k) if (k == 0) rep(1, length(x)) else sqrt(2) * cos(k * pi * x)
  P <- vapply(0:(K - 1), function(k) phi(xs, k), numeric(n))
  beta <- numeric(K)
  for (it in seq_len(80)) {
    p <- 1 / (1 + exp(-as.numeric(P %*% beta)))
    grad <- as.numeric(crossprod(P, ys - p))
    beta <- beta + 0.02 * grad / n * 4 - 0.001 * beta
  }
  fit <- function(x) {
    fx <- sum(beta * vapply(0:(K - 1), function(k) phi(x, k), numeric(1)))
    1 / (1 + exp(-fx))
  }
  .t1_result(estimate = abs(fit(0.5) - 0.5),
             orders_correctly = fit(0.9) > 0.5 && fit(0.1) < 0.5,
             p_low_high = c(fit(0.1), fit(0.9)),
             method = "series binary regression (GvdV 2017 sec. 10.4.3)")
}

# SPDX-License-Identifier: AGPL-3.0-or-later
#' Random-series Poisson regression
#'
#' Y | x ~ Poisson(exp(f(x))) with f a finite random series.  The log
#' link keeps the rate positive without constraining the coefficients,
#' and gradient ascent on the MAP objective recovers the log-rate; the
#' reported error is the mean absolute deviation of the fitted f from the
#' truth f0(x) = 1 + 0.8 cos(pi x) over a uniform grid.
#'
#' Formula: grad_k = sum_i (y_i - exp(min(f(x_i), 5))) phi_k(x_i),
#'   beta <- beta + step grad - decay beta.
#'
#' @param n Sample size.
#' @param K Number of basis terms.
#' @param seed Seed for the deterministic draws.
#' @return List with \code{estimate} (mean absolute error of the fitted
#'   log-rate), \code{beta}, \code{method}.
#' @references Ghosal & van der Vaart (2017), Fundamentals of
#'   Nonparametric Bayesian Inference, CUP, section 10.4.4.
#' @export
Ghosalfrspoireg <- function(n = 800, K = 3, seed = 42) {
  n <- as.integer(n); K <- as.integer(K)
  if (n < 1L) stop("n must be positive")
  if (K < 1L) stop("K must be positive")
  e <- .ghc_rng(seed)
  xs <- .ghc_unif(e, n)
  f0 <- function(x) 1 + 0.8 * cos(pi * x)
  # Knuth's product method, one uniform at a time, so the draw sequence
  # matches the Python arm exactly.
  rpois1 <- function(lam) {
    L <- exp(-lam); k <- 0; p <- 1
    repeat {
      p <- p * .ghc_unif(e, 1L)
      if (p <= L) return(k)
      k <- k + 1
    }
  }
  ys <- vapply(xs, function(x) rpois1(exp(f0(x))), numeric(1))
  phi <- function(x, k) if (k == 0) rep(1, length(x)) else sqrt(2) * cos(k * pi * x)
  P <- vapply(0:(K - 1), function(k) phi(xs, k), numeric(n))
  beta <- numeric(K)
  for (it in seq_len(200)) {
    mu <- exp(pmin(as.numeric(P %*% beta), 5))
    grad <- as.numeric(crossprod(P, ys - mu))
    beta <- beta + 0.002 * grad / n * 10 - 0.0005 * beta
  }
  xg <- (seq_len(20) - 0.5) / 20
  Pg <- vapply(0:(K - 1), function(k) phi(xg, k), numeric(20))
  err <- sum(abs(as.numeric(Pg %*% beta) - f0(xg))) / 20
  .t1_result(estimate = err, beta = beta,
             method = "series Poisson regression (GvdV 2017 sec. 10.4.4)")
}

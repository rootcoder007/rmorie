# SPDX-License-Identifier: AGPL-3.0-or-later
#' Density estimation via a finite random series prior
#'
#' log f = sum_{k <= K} beta_k phi_k - log Z, with the NUMBER of terms K
#' itself random.  That randomness is the entire adaptation mechanism: a
#' fixed K ties the rate to whatever smoothness the basis can express,
#' while a prior on K with fast enough decay lets the posterior settle on
#' the K matching the truth and attain n^(-s/(2s+1)) for every s without
#' knowing s.  The exponential link buys positivity and normalisation, so
#' the coefficients need no constraint.
#'
#' Formula: pi(K) proportional to exp(-c K log K); for each draw
#'   f(g) proportional to exp(sum_j beta_j cos(pi j z(g))), renormalised
#'   by the trapezoid rule, and the reported density is the draw average.
#'
#' @param x Numeric vector of observations; at least 5, with spread.
#' @param grid Evaluation points; 200 equispaced over the range if NULL.
#' @param K Fixed number of terms; drawn from the prior when NULL, which
#'   is the adaptive case.
#' @param s Smoothness at which to report the minimax rate.
#' @param seed Seed for the deterministic draws.
#' @param n_draws Monte Carlo draws over K and the coefficients.
#' @return List with \code{grid}, \code{density}, \code{K_fixed},
#'   \code{K_drawn_mean}, \code{rate}, \code{adaptive},
#'   \code{prior_on_K}, \code{mass}, \code{n}, \code{method}.
#' @references Ghosal & van der Vaart (2017), Fundamentals of
#'   Nonparametric Bayesian Inference, CUP, sections 10.4 and 10.4.1.
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' Ghosalfrsdensity(V)
Ghosalfrsdensity <- function(x, grid = NULL, K = NULL, s = NULL,
                             seed = 0, n_draws = 150) {
  xv <- as.numeric(x)
  nn <- length(xv)
  if (nn < 5) stop(sprintf("need at least 5 observations, got %d.", nn))
  lo <- min(xv); hi <- max(xv)
  if (hi <= lo) stop("the sample has zero spread.")
  g <- if (is.null(grid)) seq(lo, hi, length.out = 200) else as.numeric(grid)
  z <- (g - lo) / (hi - lo)
  zx <- (xv - lo) / (hi - lo)
  e <- .ghc_rng(as.integer(seed))
  kmax <- max(2, ceiling(nn^(1 / 3)))
  ks <- seq_len(kmax)
  pk <- exp(-0.5 * ks * log(ks + 1))
  pk <- pk / sum(pk)
  dens <- numeric(length(g))
  kdraw <- numeric(n_draws)
  for (it in seq_len(n_draws)) {
    kk <- if (is.null(K)) .ghc_choice_p(e, ks, pk) else as.integer(K)
    kdraw[it] <- kk
    j <- seq_len(kk)
    phi_g <- outer(z, j, function(a, b) cos(pi * b * a))
    phi_x <- outer(zx, j, function(a, b) cos(pi * b * a))
    beta <- colMeans(phi_x) * kk + .ghc_norm(e, kk, 0, 0.3)
    psi <- as.numeric(phi_g %*% beta)
    f <- exp(psi - max(psi))
    f <- f / .morie_gh_trapz(g, f)
    dens <- dens + f
  }
  dens <- dens / n_draws
  sv <- if (is.null(s)) 1 else as.numeric(s)
  .t1_result(grid = g, density = dens,
             K_fixed = if (is.null(K)) NULL else as.integer(K),
             K_drawn_mean = mean(kdraw),
             rate = .morie_gh_minimax_rate(nn, sv),
             adaptive = is.null(K),
             prior_on_K = "pi(K) proportional to exp(-c K log K)",
             mass = .morie_gh_trapz(g, dens), n = nn,
             method = "Finite random series (Sec. 10.4.1); the prior on K is what makes it adaptive")
}

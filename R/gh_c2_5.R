# SPDX-License-Identifier: AGPL-3.0-or-later
#' Histogram (binning) density prior
#'
#' f(x) = sum_k (p_k / |B_k|) 1{x in B_k} with the bin probabilities
#' Dirichlet.  It is the simplest density prior that is genuinely
#' nonparametric, and the Dirichlet vector is drawn by normalising
#' independent gammas -- the standard route, and the one the book's
#' Proposition G.2 licenses.
#'
#' Formula: g_k ~ Gamma(alpha, 1), p = g / sum(g),
#'   f(x) = p_{b(x)} K on \[0, 1\] with K equal-width bins.
#'
#' @param x Evaluation points.
#' @param K Number of bins, at least 1.
#' @param alpha Dirichlet concentration, positive.
#' @param seed Seed for the deterministic draws.
#' @return List with \code{estimate} (mean density),
#'   \code{density}, \code{weights}, \code{method}.
#' @references Ghosal & van der Vaart (2017), Fundamentals of
#'   Nonparametric Bayesian Inference, CUP, section 2.3.2.
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' Ghosalhistogramprior(V)
Ghosalhistogramprior <- function(x, K = 8, alpha = 1, seed = 42) {
  xs <- as.numeric(x)
  K <- as.integer(K)
  if (length(xs) == 0L) stop("x must be non-empty")
  if (K < 1L) stop("K must be positive")
  if (alpha <= 0) stop("alpha must be positive")
  e <- .ghc_rng(seed)
  g <- vapply(seq_len(K), function(i) .ghc_gamma1(e, alpha, 1), numeric(1))
  if (sum(g) <= 0) stop("total mass must be positive")
  p <- g / sum(g)
  width <- 1 / K
  idx <- pmin(as.integer(xs * K), K - 1L) + 1L
  dens <- ifelse(xs >= 0 & xs <= 1, p[idx] / width, 0)
  .t1_result(estimate = mean(dens), density = dens, weights = p,
             method = "Dirichlet histogram prior (GvdV 2017 sec. 2.3.2)")
}

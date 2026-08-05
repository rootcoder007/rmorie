# SPDX-License-Identifier: AGPL-3.0-or-later
#' Mixture-of-kernels density prior
#'
#' f = sum_k w_k K(x; theta_k) with Dirichlet weights.  Unlike the
#' histogram the mixture is smooth, and unlike a fixed basis its
#' components move: the locations are themselves random, which is what
#' lets a small number of components track structure anywhere on the
#' domain.
#'
#' Formula: w ~ Dirichlet(1, ..., 1) by gamma normalisation,
#'   theta_k ~ U(0, 1), f(x) = sum_k w_k phi((x - theta_k)/h) / h.
#'
#' @param x Evaluation points.
#' @param K Number of mixture components, at least 1.
#' @param seed Seed for the deterministic draws.
#' @param bandwidth Kernel bandwidth, positive.
#' @return List with \code{estimate} (mean density), \code{density},
#'   \code{weights}, \code{locations}, \code{method}.
#' @references Ghosal & van der Vaart (2017), Fundamentals of
#'   Nonparametric Bayesian Inference, CUP, section 2.3.3.
#' @export
Ghosalmixturebasisprior <- function(x, K = 5, seed = 42,
                                    bandwidth = 0.15) {
  xs <- as.numeric(x)
  K <- as.integer(K)
  if (length(xs) == 0L) stop("x must be non-empty")
  if (K < 1L) stop("K must be positive")
  if (bandwidth <= 0) stop("bandwidth must be positive")
  e <- .ghc_rng(seed)
  g <- vapply(seq_len(K), function(i) .ghc_gamma1(e, 1, 1), numeric(1))
  if (sum(g) <= 0) stop("total mass must be positive")
  w <- g / sum(g)
  th <- .ghc_unif(e, K)
  cc <- 1 / (bandwidth * sqrt(2 * pi))
  dens <- as.numeric(exp(-0.5 * (outer(xs, th, "-") / bandwidth)^2) %*% (w * cc))
  .t1_result(estimate = mean(dens), density = dens, weights = w,
             locations = th,
             method = "Dirichlet kernel mixture (GvdV 2017 sec. 2.3.3)")
}

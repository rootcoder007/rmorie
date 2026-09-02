# SPDX-License-Identifier: AGPL-3.0-or-later
#' Strong approximation for the Dirichlet process
#'
#' sup_t |sqrt(n)(F_n(t) - F0(t)) - B(F0(t))| tends to zero almost
#' surely: the empirical process can be constructed on the SAME
#' probability space as its Brownian bridge limit, so the two stay
#' uniformly close rather than merely agreeing in distribution.  The
#' observable consequence is that the sup statistic sits at the
#' Kolmogorov-Smirnov scale, which is what is computed here.
#'
#' Formula: KS = sqrt(n) sup_i max(|i/n - X_(i)|, |(i-1)/n - X_(i)|)
#'   for the uniform truth.
#'
#' @param n Sample size.
#' @param seed Seed for the deterministic draws.
#' @return List with \code{estimate} (the KS statistic),
#'   \code{typical_ks_range}, \code{method}.
#' @references Ghosal & van der Vaart (2017), Fundamentals of
#'   Nonparametric Bayesian Inference, CUP, section 12.2.1.
#' @export
#' @examples
#' Ghosalstrongapxdp()
Ghosalstrongapxdp <- function(n = 3000, seed = 42) {
  n <- as.integer(n)
  if (n < 1L) stop("n must be positive")
  e <- .ghc_rng(seed)
  d <- sort(.ghc_unif(e, n))
  i <- seq_len(n)
  ks <- sqrt(n) * max(abs(i / n - d), abs((i - 1) / n - d))
  .t1_result(estimate = ks, typical_ks_range = ks < 3,
             method = "strong approximation (GvdV 2017 sec. 12.2.1)")
}

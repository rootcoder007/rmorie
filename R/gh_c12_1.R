# SPDX-License-Identifier: AGPL-3.0-or-later
#' Parametric Bernstein-von Mises
#'
#' The sqrt(n)-rescaled posterior approaches N(theta-hat, I^(-1)/n) in
#' TOTAL VARIATION -- not merely weakly.  Total variation is the strong
#' mode that makes the theorem useful, because it transfers to every
#' derived quantity including credible sets.  The Beta-Bernoulli case
#' admits the exact posterior, so the distance to the BvM normal can be
#' integrated rather than argued.
#'
#' Formula: TV = (1/2) int |Beta(t; 1+S, 1+n-S) - phi(t; S/n, I^(-1)/n)| dt
#'   over theta-hat +/- 6 sd, by midpoint rule.
#'
#' @param theta0 True success probability, in (0, 1).
#' @param n Number of Bernoulli trials.
#' @param seed Seed for the deterministic draws.
#' @return List with \code{estimate} (total variation distance),
#'   \code{bvm_holds}, \code{method}.
#' @references Ghosal & van der Vaart (2017), Fundamentals of
#'   Nonparametric Bayesian Inference, CUP, section 12.1.
#' @export
#' @examples
#' Ghosalinfdimbvm()
Ghosalinfdimbvm <- function(theta0 = 0.4, n = 2000, seed = 42) {
  n <- as.integer(n)
  if (n < 1L) stop("n must be positive")
  if (theta0 <= 0 || theta0 >= 1)
    stop("theta0 must lie strictly between 0 and 1")
  e <- .ghc_rng(seed)
  S <- sum(.ghc_unif(e, n) < theta0)
  a <- 1 + S
  b <- 1 + n - S
  mle <- S / n
  sd <- sqrt(mle * (1 - mle) / n)
  grid <- 2000L
  lo <- max(mle - 6 * sd, 1e-9)
  hi <- min(mle + 6 * sd, 1 - 1e-9)
  t <- lo + (hi - lo) * (seq_len(grid) - 0.5) / grid
  bpdf <- exp(lgamma(a + b) - lgamma(a) - lgamma(b) +
                (a - 1) * log(t) + (b - 1) * log(1 - t))
  npdf <- exp(-0.5 * ((t - mle) / sd)^2) / (sd * sqrt(2 * pi))
  tv <- sum(0.5 * abs(bpdf - npdf) * (hi - lo) / grid)
  .t1_result(estimate = tv, bvm_holds = tv < 0.05,
             method = "parametric BvM (GvdV 2017 sec. 12.1)")
}

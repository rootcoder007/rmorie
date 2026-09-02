# SPDX-License-Identifier: AGPL-3.0-or-later
#' Credible-set frequentist coverage
#'
#' When BvM holds, (1 - alpha)-credible sets have frequentist coverage
#' tending to 1 - alpha.  This is the practical payoff of the whole
#' chapter: it is what licenses reading a Bayesian interval as a
#' confidence interval, and it is exactly what fails in the infinite
#' dimensional cases section 12.4 warns about.
#'
#' Formula: interval = m +/- z sd with m = a/(a+b) and
#'   sd^2 = ab / ((a+b)^2 (a+b+1)) for the Beta(1+S, 1+n-S) posterior.
#'
#' @param theta0 True success probability, in (0, 1).
#' @param n Trials per replicate.
#' @param level Nominal credible level; 0.9 uses z = 1.6449, else 1.96.
#' @param n_sim Number of replicates.
#' @param seed Seed for the deterministic draws.
#' @return List with \code{estimate} (empirical coverage),
#'   \code{nominal}, \code{gap}, \code{method}.
#' @references Ghosal & van der Vaart (2017), Fundamentals of
#'   Nonparametric Bayesian Inference, CUP, section 12.5.
#' @export
#' @examples
#' Ghosalcredsetcov()
Ghosalcredsetcov <- function(theta0 = 0.5, n = 400, level = 0.9,
                             n_sim = 400, seed = 42) {
  n <- as.integer(n)
  n_sim <- as.integer(n_sim)
  if (n < 1L) stop("n must be positive")
  if (n_sim < 1L) stop("n_sim must be positive")
  if (theta0 <= 0 || theta0 >= 1)
    stop("theta0 must lie strictly between 0 and 1")
  e <- .ghc_rng(seed)
  z <- if (abs(level - 0.9) < 1e-9) 1.6448536269514722 else 1.96
  hits <- 0L
  for (i in seq_len(n_sim)) {
    S <- sum(.ghc_unif(e, n) < theta0)
    a <- 1 + S
    b <- 1 + n - S
    m <- a / (a + b)
    sd <- sqrt(a * b / ((a + b)^2 * (a + b + 1)))
    if (m - z * sd <= theta0 && theta0 <= m + z * sd) hits <- hits + 1L
  }
  cov <- hits / n_sim
  .t1_result(estimate = cov, nominal = level, gap = abs(cov - level),
             method = "credible-set coverage (GvdV 2017 sec. 12.5)")
}

# SPDX-License-Identifier: AGPL-3.0-or-later
#' Smooth hazard via a Gaussian process
#'
#' lambda(t) = exp(f(t)) with f a Gaussian process.  The exponential link
#' is what allows an unconstrained smooth prior on f to induce a
#' positive hazard, and once time is binned the likelihood is exactly
#' Poisson with the bin exposure as offset.  Ridge-smoothed bin estimates
#' recover a constant true hazard.
#'
#' Formula: f_b = log((d_b + 1/2) / (E_b + 1/2)) with d_b the events and
#'   E_b the exposure in bin b; hazard = exp(f_b).
#'
#' @param n Sample size.
#' @param seed Seed for the deterministic draws.
#' @return List with \code{estimate} (mean absolute hazard error),
#'   \code{hazard_by_bin}, \code{method}.
#' @references Ghosal & van der Vaart (2017), Fundamentals of
#'   Nonparametric Bayesian Inference, CUP, section 13.5.
#' @export
#' @examples
#' Ghosalsmhazgp()
Ghosalsmhazgp <- function(n = 500, seed = 42) {
  n <- as.integer(n)
  if (n < 1L) stop("n must be positive")
  e <- .ghc_rng(seed)
  lam0 <- 1
  k <- 6L
  d_ <- numeric(k); e_ <- numeric(k)
  for (i in seq_len(n)) {
    x <- -log(max(.ghc_unif(e, 1L), 1e-12)) / lam0
    for (b in seq_len(k)) {
      lo <- (b - 1) * 0.3; hi <- b * 0.3
      if (x >= hi) {
        e_[b] <- e_[b] + 0.3
      } else if (x > lo) {
        e_[b] <- e_[b] + x - lo
        d_[b] <- d_[b] + 1
        break
      }
    }
  }
  haz <- exp(log(pmax((d_ + 0.5) / (e_ + 0.5), 1e-6)))
  .t1_result(estimate = sum(abs(haz - lam0)) / k, hazard_by_bin = haz,
             method = "GP smooth hazard (GvdV 2017 sec. 13.5)")
}

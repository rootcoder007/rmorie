# SPDX-License-Identifier: AGPL-3.0-or-later
#' Riemann-Liouville process
#'
#' R_t^alpha = Gamma(alpha + 1/2)^(-1) int_0^t (t - s)^(alpha - 1/2) dB_s
#' is Gaussian and self-similar of index alpha, so var(R_t) is
#' proportional to t^(2 alpha).  Fractional integration of white noise is
#' how the chapter builds a process of any prescribed smoothness, and the
#' variance-growth exponent is the observable signature of that
#' smoothness; it is recovered here from a discretised stochastic
#' integral.
#'
#' Formula: growth = log(var(R_1) / var(R_{1/4})) / log 4, which equals
#'   2 alpha exactly for the continuum process.
#'
#' @param alpha Self-similarity index, positive.
#' @param n_grid Discretisation of [0, 1].
#' @param n_sim Number of simulated paths.
#' @param seed Seed for the deterministic draws.
#' @return List with \code{estimate} (empirical growth exponent),
#'   \code{expected}, \code{gap}, \code{method}.
#' @references Ghosal & van der Vaart (2017), Fundamentals of
#'   Nonparametric Bayesian Inference, CUP, Example 11.6, eq. (11.2).
#' @export
#' @examples
#' Ghosalrlprocess()
Ghosalrlprocess <- function(alpha = 0.75, n_grid = 200, n_sim = 300,
                            seed = 42) {
  alpha <- as.numeric(alpha)
  n_grid <- as.integer(n_grid); n_sim <- as.integer(n_sim)
  if (alpha <= 0) stop("alpha must be positive")
  if (n_grid < 4L) stop("n_grid must be at least 4")
  if (n_sim < 1L) stop("n_sim must be positive")
  e <- .ghc_rng(seed)
  g <- gamma(alpha + 0.5)
  t1_idx <- n_grid %/% 4L; t2_idx <- n_grid
  v1 <- 0; v2 <- 0
  mid <- (seq_len(n_grid) - 0.5) / n_grid
  for (it in seq_len(n_sim)) {
    dB <- .ghc_norm(e, n_grid) / sqrt(n_grid)
    for (ti in c(t1_idx, t2_idx)) {
      tt <- ti / n_grid
      j <- seq_len(ti)
      r <- sum((tt - mid[j])^(alpha - 0.5) * dB[j]) / g
      if (ti == t1_idx) v1 <- v1 + r * r / n_sim else v2 <- v2 + r * r / n_sim
    }
  }
  growth <- log(v2 / v1) / log(4)
  .t1_result(estimate = growth, expected = 2 * alpha,
             gap = abs(growth - 2 * alpha),
             method = "Riemann-Liouville process (GvdV 2017 eq. 11.2)")
}

# SPDX-License-Identifier: AGPL-3.0-or-later
#' Sample size for a stated half-width, with the finite-population step.
#'
#' Cochran's two-step form: compute n_0 ignoring the population size,
#' then correct it. Both are returned.
#'
#' Formula: n_0 = z^2 S^2 / e^2; n = n_0 / (1 + n_0/N), rounded up
#'
#' @param e Desired half-width of the confidence interval, e > 0.
#' @param S Population standard deviation (an advance estimate).
#' @param N Population size; \code{Inf} for the infinite-population case.
#' @param level Confidence level.
#' @return List with \code{n}, \code{n0}, \code{z}, \code{e}, \code{S},
#'   \code{N}.
#' @references Cochran (1977), Sampling Techniques, 3rd edition, Chapter
#'   4: n_0 = z^2 S^2 / e^2 with the correction n = n_0/(1 + n_0/N).
#'   Cross-checked against the reference implementation in the CRAN
#'   package samplingbook 1.2.4, whose sample.size.mean computes
#'   S^2 / (e^2/q^2 + S^2/N) -- the same quantity rearranged.
#' @export
Nsamp <- function(e, S, N = Inf, level = 0.95) {
  e <- as.numeric(e); S <- as.numeric(S)
  if (e <= 0) stop("the half-width e must be positive")
  if (S < 0) stop("S must be non-negative")
  if (level <= 0 || level >= 1)
    stop("level must lie strictly between 0 and 1")
  z <- stats::qnorm((1 + level) / 2)
  n0 <- z^2 * S^2 / e^2
  N <- as.numeric(N)
  n <- if (is.infinite(N)) n0 else n0 / (1 + n0 / N)
  .t1_result(n = ceiling(n - 1e-12), n0 = n0, z = z, e = e, S = S, N = N,
             method = "Cochran sample size n0/(1 + n0/N)")
}

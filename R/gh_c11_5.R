# SPDX-License-Identifier: AGPL-3.0-or-later
#' GP binary-regression contraction
#'
#' With p(x) = Psi(W_x) and W an s-smooth Gaussian process on the unit
#' cube in d dimensions, the concentration function grows like
#' eps^(-d/s); feeding that exponent into the rate equation of Theorem
#' 11.20 gives eps_n = n^(-s/(2s+d)).  The binary-regression case adds
#' nothing new to the machinery -- that is the point of Theorem 11.22.
#'
#' Formula: eps_n = n^(-s/(2s+d)).
#'
#' @param s Smoothness of the process.
#' @param d Dimension of the covariate.
#' @param ns Vector of sample sizes.
#' @return List with \code{estimate} (rate at the largest n),
#'   \code{rate_by_n}, \code{exponent}, \code{method}.
#' @references Ghosal & van der Vaart (2017), Fundamentals of
#'   Nonparametric Bayesian Inference, CUP, Theorems 11.20 and 11.22.
#' @export
#' @examples
#' Ghosalgpbinregcrt()
Ghosalgpbinregcrt <- function(s = 2, d = 1, ns = c(100, 10000)) {
  s <- as.numeric(s); d <- as.numeric(d); ns <- as.numeric(ns)
  if (s <= 0) stop("s must be positive")
  if (d <= 0) stop("d must be positive")
  if (length(ns) == 0L) stop("ns must be non-empty")
  if (any(ns <= 0)) stop("every n must be positive")
  rates <- ns^(-s / (2 * s + d))
  .t1_result(estimate = rates[length(rates)], rate_by_n = rates,
             exponent = s / (2 * s + d),
             method = "GP binary regression rate (GvdV 2017 Thm 11.22)")
}

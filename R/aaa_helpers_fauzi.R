# SPDX-License-Identifier: AGPL-3.0-or-later

# Shared Fauzi-suite helpers.
#
# Sourced before the rest of the fz*.R files so callers don't depend on R's
# alphabetical load order. Loaded first via the leading underscore in the
# filename and via the explicit Collate: field in DESCRIPTION.

#' @importFrom stats sd quantile
#' @noRd
.morie_silverman_h <- function(x) {
  n <- length(x)
  s <- stats::sd(x)
  iq <- diff(stats::quantile(x, c(.25, .75), names = FALSE)) / 1.34
  sigma <- if (iq > 0) min(s, iq) else s
  if (sigma <= 0) sigma <- 1
  1.06 * sigma * n^(-1 / 5)
}

#' Bandwidth for a DISTRIBUTION-function-type kernel estimator.
#'
#' `4^(1/3) sigma n^(-1/3)` -- a cube root, not the fifth root of
#' `.morie_silverman_h`, and the difference is not cosmetic.
#' Equations (2.3)-(2.4) of Fauzi and Maesono (2023) give
#'
#'   Bias\\[Fhat_h(x)\\] = h^2 f'(x)/2 mu_2(K) + o(h^2)
#'   Var \\[Fhat_h(x)\\] = F(1-F)/n - (2h/n) r_1 f(x) + o(h/n)
#'
#' with `r_1 = int y K(y) W(y) dy`, so the bandwidth enters the
#' variance at order `h/n` and with a NEGATIVE sign -- smoothing
#' REDUCES variance here, where in a density estimator it enters at
#' `1/(nh)` and blows up as `h -> 0`. Minimising the resulting MISE
#' gives `h_opt = (2 r_1 / (n mu_2^2 R(f')))^(1/3)`, which for a
#' Gaussian kernel (`mu_2 = 1`, `r_1 = 1/(2 sqrt(pi))`) against a
#' normal reference (`R(f') = 1/(4 sqrt(pi) sigma^3)`) collapses to
#' `4^(1/3) sigma n^(-1/3)`, about `1.587 sigma n^(-1/3)`.
#'
#' Sec. 5.3.2 says the same in words -- Azzalini (1981) recommended
#' `c n^(-1/3)` for distribution-function estimation -- and the
#' book's own simulations use `h_n = n^(-1/3)`.
#'
#' Everything in the book that converges at the parametric rate with
#' an `O(h^2)` bias and an `O(h/n)` variance term takes this rate:
#' the KDFE, the smoothed KS, CvM, sign and Wilcoxon statistics, the
#' survival estimator, and the mean-residual-life estimators, whose
#' Theorem 4.3 variance is likewise `O(1/n) - O(h/n)`. Only the Ch. 1
#' DENSITY estimators, with `O(1/(nh))` variance, take `n^(-1/5)`.
#'
#' Under the density rule a KDFE oversmooths enough to lose, in mean
#' squared error, to the empirical distribution function it exists to
#' improve on.
#'
#' @importFrom stats sd quantile
#' @noRd
.morie_kdfe_h <- function(x) {
  n <- length(x)
  s <- stats::sd(x)
  iq <- diff(stats::quantile(x, c(.25, .75), names = FALSE)) / 1.349
  sigma <- if (iq > 0) min(s, iq) else s
  if (sigma <= 0) sigma <- 1
  4^(1 / 3) * sigma * n^(-1 / 3)
}

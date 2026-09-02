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

#' Bandwidth for a DISTRIBUTION-function-type kernel estimator
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

#' The book's `R(z) = sqrt(2 pi) z^(z+1/2) / (e^z Gamma(z+1))`, Eq. (1.12)
#'
#' The Stirling defect of the gamma function: `R(z)` increases monotonically
#' to 1 from below (Remark 1.2), which is exactly the fact the book uses to
#' read off the `O(n^-1 h^-1/4)` interior and `O(n^-1 h^-3/4)` boundary rates
#' of `Var\[A_h(x)\]` in (1.11). Computed through `lgamma` so the `z^(z+1/2)`
#' factor cannot overflow at the small bandwidths this suite uses
#' (`z ~ h^(-1/2)`).
#' @noRd
.morie_fauzi_rratio <- function(z) {
  if (any(z <= 0)) stop("R(z) is defined for z > 0.")
  exp(0.5 * log(2 * pi) + (z + 0.5) * log(z) - z - lgamma(z + 1))
}

#' `A_h(v)` of Eq. (1.9): the sample mean of the
#' Gamma(shape = `h^(-1/2)`, scale = `v sqrt(h) + h`) density
#'
#' NOT Chen's gamma kernel, which takes shape `v/h + 1` and scale `h`. The
#' book fixes the SHAPE at `h^(-1/2)` and moves the scale; that single change
#' buys the smaller variance orders of Remark 1.2 and costs the bias its
#' rate, taking it from `O(h)` up to `O(sqrt(h))` in (1.10) -- which the
#' geometric extrapolation (1.14) then buys back.
#' @noRd
.morie_fauzi_agamma <- function(x, v, h) {
  x <- as.numeric(x)
  if (h <= 0) stop("bandwidth must be positive.")
  if (any(x < 0)) stop("gamma kernels need data on [0, infinity).")
  if (any(v < 0)) stop("the evaluation points must lie in [0, infinity).")
  shape <- 1 / sqrt(h)
  vapply(v, function(pt) {
    mean(stats::dgamma(x,
      shape = shape,
      scale = pt * sqrt(h) + h
    ))
  }, numeric(1))
}

#' Exact one-sided Kolmogorov distribution function, Birnbaum-Tingey
#'
#' `P(D_n^+ >= d) = d sum_{j=0}^{floor(n(1-d))} choose(n,j) (d+j/n)^(j-1)
#' (1-d-j/n)^(n-j)`, and this returns `1 -` that. Summed through logarithms so
#' the binomial coefficient cannot overflow, and mirroring
#' `morie.fn._stats_core.ksone` term for term so the Python and R arms agree to
#' the last bit rather than merely to plotting accuracy.
#' @noRd
.morie_fauzi_ksone <- function(d, n) {
  d <- as.numeric(d)
  n <- as.integer(n)
  if (d <= 0) {
    return(0)
  }
  if (d >= 1) {
    return(1)
  }
  limit <- as.integer(n * (1 - d))
  s <- 0
  for (j in 0:limit) {
    a <- d + j / n
    b <- 1 - d - j / n
    if (b <= 0) {
      if (n - j == 0L) b_term <- 0 else next
    } else {
      b_term <- (n - j) * log(b)
    }
    lc <- lchoose(n, j)
    s <- s + exp(lc + (j - 1) * log(a) + b_term) * d
  }
  1 - max(0, min(1, s))
}

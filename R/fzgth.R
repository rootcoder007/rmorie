# SPDX-License-Identifier: AGPL-3.0-or-later

#' The function G(theta), distribution function of the half-sum (X_1+X_2)/2
#'
#' Sec. 5.3: \deqn{G(\theta) = \int F(2\theta+u)f(u)\,du,}{G(theta) = int F(2 theta + u) f(u) du,}
#' the distribution function of `(X_1 + X_2)/2`.
#'
#' It is the population quantity the Mann-Whitney form of Wilcoxon's signed
#' rank statistic estimates: `W = sum_{i<=j} I(X_i + X_j >= 0)` has
#' `E(Wtilde) = n(n+1)/2 {G(theta) + O(h^2)}`. Under `H0` with `F` symmetric,
#' `G(0) = 1/2`.
#'
#' Two equivalent forms appear, `int F(2 theta + u) f(u) du` and
#' `int F(2 theta - u) f(u) du`; they agree because the integrand is
#' symmetrised by `f`. The book prints the first, and the primary source --
#' Maesono, Y., Moriyama, T. and Lu, M. (2018), "Smoothed nonparametric tests
#' and their properties", Annals of the Institute of Statistical Mathematics
#' 70(5):969-982 (arXiv:1610.02145) -- prints both and states they are equal.
#'
#' Integrated on a fixed trapezoid grid; with `cdf` and `density` both standard
#' normal, `G(0)` returns 1/2 to quadrature accuracy.
#'
#' @param theta Location parameter.
#' @param cdf `F`; defaults to the standard normal.
#' @param density `f`; defaults to the standard normal.
#' @param lo,hi Quadrature limits.
#' @param ngrid Number of nodes; fixed, never adapted.
#' @return Named list with ``estimate``, ``theta``, ``method``.
#' @references Fauzi and Maesono (2023), Sec. 5.3; Maesono, Moriyama and Lu (2018), AISM 70:969-982.
#' @examples
#' Hsumcdf(theta = 0)
#' @export
Hsumcdf <- function(theta = 0, cdf = NULL, density = NULL, lo = -10, hi = 10, ngrid = 4001L) {
  if (is.null(cdf)) cdf <- stats::pnorm
  if (is.null(density)) density <- stats::dnorm
  if (!is.function(cdf) || !is.function(density)) stop("cdf and density must be functions.")
  u <- seq(lo, hi, length.out = ngrid)
  fu <- vapply(u, function(t) as.numeric(density(t)), numeric(1))
  trap <- function(y, g) sum(diff(g) * (y[-length(y)] + y[-1]) / 2)
  est <- vapply(as.numeric(theta), function(th) {
    fv <- vapply(u, function(t) as.numeric(cdf(2 * th + t)), numeric(1))
    trap(fv * fu, u)
  }, numeric(1))
  list(estimate = est, theta = as.numeric(theta),
       method = "G(theta), distribution function of (X_1 + X_2)/2")
}

# CANONICAL TEST
# stopifnot(abs(Hsumcdf(theta = 0)$estimate - 0.5) < 1e-9)

#' @rdname Hsumcdf
#' @keywords internal
#' @export
morie_fauzi_g_theta_distribution <- Hsumcdf

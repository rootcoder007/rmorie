# SPDX-License-Identifier: AGPL-3.0-or-later
#' Exponential-link density prior
#'
#' p(x) = exp(psi(x)) / int exp(psi) turns ANY bounded function psi into a
#' positive normalised density.  The two constraints that make density
#' estimation awkward -- positivity and unit mass -- are both discharged
#' by the link, so the prior on psi can be completely unconstrained.
#'
#' Formula: Z = int exp(psi) by the trapezoid rule on the sorted grid;
#'   density = exp(psi) / Z.
#'
#' @param x Grid points; sorted internally, at least two of them.
#' @param psi Log-density shape as a function; sin(3 t) when NULL.
#' @return List with \code{estimate} (density at the middle grid point),
#'   \code{density}, \code{normalizer}, \code{method}.
#' @references Ghosal & van der Vaart (2017), Fundamentals of
#'   Nonparametric Bayesian Inference, CUP, section 2.3.1.
#' @export
Ghosalexplink <- function(x, psi = NULL) {
  xs <- sort(as.numeric(x))
  n <- length(xs)
  if (n < 2L) stop("x must have at least two grid points")
  if (is.null(psi)) psi <- function(t) sin(3 * t)
  ex <- exp(vapply(xs, psi, numeric(1)))
  Z <- sum(0.5 * (ex[-1] + ex[-n]) * diff(xs))
  if (Z <= 0) stop("the normalizing constant is not positive")
  dens <- ex / Z
  .t1_result(estimate = dens[n %/% 2L + 1L], density = dens,
             normalizer = Z,
             method = "exponential link density (GvdV 2017 sec. 2.3.1)")
}

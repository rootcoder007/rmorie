# SPDX-License-Identifier: AGPL-3.0-or-later
#' Increasing-process prior via an exponentiated Gaussian process
#'
#' F(t) = int_0^t exp(W(s)) ds is strictly increasing BY CONSTRUCTION for
#' any Gaussian path W: the monotonicity constraint is absorbed into the
#' parameterisation rather than imposed on the prior, which is what makes
#' the posterior tractable.  Trapezoid integration of the exponentiated
#' draw here.
#'
#' Formula: F(x_i) = F(x_{i-1})
#'   + (exp(W_i) + exp(W_{i-1})) (x_i - x_{i-1}) / 2.
#'
#' @param x Evaluation points; sorted internally.
#' @param length Squared-exponential length scale, positive.
#' @param seed Seed for the deterministic draws.
#' @return List with \code{estimate} (terminal value), \code{F},
#'   \code{increasing}, \code{method}.
#' @references Ghosal & van der Vaart (2017), Fundamentals of
#'   Nonparametric Bayesian Inference, CUP, section 2.2.2.
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' Ghosalgpincreasingprior(V)
Ghosalgpincreasingprior <- function(x, length = 0.5, seed = 42) {
  xs <- sort(as.numeric(x))
  if (base::length(xs) == 0L) stop("x must be non-empty")
  w <- Ghosalgppriordef(xs, length = length, seed = seed)$f
  ex <- exp(w)
  F <- c(0, cumsum(0.5 * (ex[-1] + ex[-base::length(ex)]) * diff(xs)))
  inc <- if (base::length(F) < 2L) TRUE else all(diff(F) >= 0)
  .t1_result(estimate = F[base::length(F)], F = F, increasing = inc,
             method = "exponentiated-GP increasing process (GvdV 2017 sec. 2.2.2)")
}

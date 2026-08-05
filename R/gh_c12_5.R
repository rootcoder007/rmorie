# SPDX-License-Identifier: AGPL-3.0-or-later
#' Efficient influence function for F(t)
#'
#' psi-tilde(x) = 1{x <= t} - F0(t) has mean zero under P0 and variance
#' F0(t)(1 - F0(t)), which IS the semiparametric efficiency bound for
#' estimating F(t).  The influence function is the whole content of the
#' efficiency calculation: once it is identified, the bound is just its
#' second moment.
#'
#' Formula: var = (1/n) sum_i (1{x_i <= t} - F_n(t))^2, which equals
#'   F_n(t)(1 - F_n(t)) identically.
#'
#' @param data Numeric vector of observations, non-empty.
#' @param t Point at which the CDF functional is taken.
#' @return List with \code{estimate} (the variance),
#'   \code{mean_zero_gap}, \code{matches_bernoulli_var},
#'   \code{method}.
#' @references Ghosal & van der Vaart (2017), Fundamentals of
#'   Nonparametric Bayesian Inference, CUP, section 12.3.1.
#' @export
Ghosaleffinflfn <- function(data, t) {
  xs <- as.numeric(data)
  n <- length(xs)
  if (n == 0L) stop("data must be non-empty")
  F_t <- sum(xs <= t) / n
  infl <- as.numeric(xs <= t) - F_t
  v <- sum(infl * infl) / n
  .t1_result(estimate = v, mean_zero_gap = abs(sum(infl) / n),
             matches_bernoulli_var = abs(v - F_t * (1 - F_t)) < 1e-12,
             method = "efficient influence function (GvdV 2017 sec. 12.3.1)")
}

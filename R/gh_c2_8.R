# SPDX-License-Identifier: AGPL-3.0-or-later
#' Nonparametric normal regression with a Gaussian process prior
#'
#' Y_i = f(x_i) + e_i with e normal and f ~ GP(0, k).  Everything is
#' conjugate, so the posterior mean is available in closed form and turns
#' out to be exactly the kernel-ridge smoother -- the Bayesian and the
#' penalised-least-squares answers coincide, with sigma^2 playing the
#' role of the penalty.
#'
#' Formula: f-hat(x*) = k(x*, X) (K + sigma^2 I)^(-1) Y.
#'
#' @param x Design points.
#' @param y Responses, matching \code{x}.
#' @param length Squared-exponential length scale, positive.
#' @param var Kernel variance, positive.
#' @param sigma2 Noise variance, positive.
#' @return List with \code{estimate} (mean fitted value),
#'   \code{fitted}, \code{sse}, \code{method}.
#' @references Ghosal & van der Vaart (2017), Fundamentals of
#'   Nonparametric Bayesian Inference, CUP, section 2.4.
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' Ghosalnpnormalreg(V, V)
Ghosalnpnormalreg <- function(x, y, length = 0.5, var = 1, sigma2 = 0.05) {
  xs <- as.numeric(x); ys <- as.numeric(y)
  n <- base::length(xs)
  if (n == 0L) stop("x must be non-empty")
  if (base::length(ys) != n) stop("x and y must have the same length")
  if (length <= 0) stop("length must be positive")
  if (var <= 0) stop("var must be positive")
  if (sigma2 <= 0) stop("sigma2 must be positive")
  Kx <- var * exp(-0.5 * (outer(xs, xs, "-") / length)^2)
  fhat <- as.numeric(Kx %*% solve(Kx + diag(sigma2, n), ys))
  .t1_result(estimate = mean(fhat), fitted = fhat,
             sse = sum((fhat - ys)^2),
             method = "GP-prior normal regression (GvdV 2017 sec. 2.4)")
}

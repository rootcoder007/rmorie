# SPDX-License-Identifier: AGPL-3.0-or-later
#' Nonparametric Poisson regression with a Gaussian process prior
#'
#' Y | x ~ Poisson(exp f(x)) with f ~ GP.  The log link keeps the
#' intensity positive without constraining f, and as in the binary case
#' the price is the loss of conjugacy, so the posterior mode is found by
#' damped Newton on the log-posterior.
#'
#' Formula: solve (K^(-1) + diag(lambda)) df = (y - lambda) - K^(-1) f
#'   and step by df/2, with lambda = exp(min(f, 30)).
#'
#' @param x Design points.
#' @param y Non-negative counts matching \code{x}.
#' @param length Squared-exponential length scale, positive.
#' @param var Kernel variance, positive.
#' @return List with \code{estimate} (mean intensity),
#'   \code{intensity}, \code{f}, \code{method}.
#' @references Ghosal & van der Vaart (2017), Fundamentals of
#'   Nonparametric Bayesian Inference, CUP, section 2.6.
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' Ghosalnppoissonreg(V, V)
Ghosalnppoissonreg <- function(x, y, length = 0.7, var = 1) {
  xs <- as.numeric(x); ys <- as.numeric(y)
  n <- base::length(xs)
  if (n == 0L) stop("x must be non-empty")
  if (base::length(ys) != n) stop("x and y must have the same length")
  if (any(ys < 0)) stop("counts must be non-negative")
  if (length <= 0) stop("length must be positive")
  if (var <= 0) stop("var must be positive")
  K <- var * exp(-0.5 * (outer(xs, xs, "-") / length)^2) + diag(1e-8, n)
  Ki <- .ghc_pinv(K)
  f <- log(pmax(ys, 0.5))
  for (it in seq_len(60)) {
    lam <- exp(pmin(f, 30))
    A <- Ki + diag(lam, n)
    b <- (ys - lam) - as.numeric(Ki %*% f)
    step <- as.numeric(tryCatch(solve(A, b),
                                error = function(e) .ghc_pinv(A) %*% b))
    f <- f + 0.5 * step
    if (max(abs(step)) < 1e-8) break
  }
  lam <- exp(f)
  .t1_result(estimate = mean(lam), intensity = lam, f = f,
             method = "log-link GP Poisson regression MAP (GvdV 2017 sec. 2.6)")
}

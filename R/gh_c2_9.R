# SPDX-License-Identifier: AGPL-3.0-or-later
#' Nonparametric binary regression with a Gaussian process prior
#'
#' P(Y = 1 | x) = Phi(f(x)) with f ~ GP.  The probit link destroys
#' conjugacy, so the posterior is approximated at its mode: damped Newton
#' on the log-posterior, which is the Laplace route the chapter's
#' computation sections use throughout.
#'
#' Formula: with s_i = phi(f_i)/Phi(f_i) for y_i = 1 and
#'   -phi(f_i)/(1 - Phi(f_i)) otherwise, and W_i = s_i (s_i + f_i),
#'   solve (K^(-1) + W) df = s - K^(-1) f and step by df/2.
#'
#' @param x Design points.
#' @param y Binary responses matching \code{x}.
#' @param length Squared-exponential length scale, positive.
#' @param var Kernel variance, positive.
#' @return List with \code{estimate} (mean fitted probability),
#'   \code{prob}, \code{f}, \code{method}.
#' @references Ghosal & van der Vaart (2017), Fundamentals of
#'   Nonparametric Bayesian Inference, CUP, section 2.5.
#' @export
Ghosalnpbinaryreg <- function(x, y, length = 0.7, var = 2) {
  xs <- as.numeric(x); ys <- as.numeric(y)
  n <- base::length(xs)
  if (n == 0L) stop("x must be non-empty")
  if (base::length(ys) != n) stop("x and y must have the same length")
  if (length <= 0) stop("length must be positive")
  if (var <= 0) stop("var must be positive")
  K <- var * exp(-0.5 * (outer(xs, xs, "-") / length)^2) + diag(1e-8, n)
  Ki <- .ghc_pinv(K)
  f <- numeric(n)
  for (it in seq_len(50)) {
    phi <- exp(-0.5 * f * f) / sqrt(2 * pi)
    Phi <- pmin(pmax(pnorm(f), 1e-10), 1 - 1e-10)
    s <- ifelse(ys > 0.5, phi / Phi, -phi / (1 - Phi))
    W <- s * (s + f)
    A <- Ki + diag(W, n)
    b <- s - as.numeric(Ki %*% f)
    step <- tryCatch(solve(A, b), error = function(e) .ghc_pinv(A) %*% b)
    step <- as.numeric(step)
    f <- f + 0.5 * step
    if (max(abs(step)) < 1e-8) break
  }
  p <- pnorm(f)
  .t1_result(estimate = mean(p), prob = p, f = f,
             method = "probit-GP binary regression, Laplace MAP (GvdV 2017 sec. 2.5)")
}

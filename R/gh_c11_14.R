# SPDX-License-Identifier: AGPL-3.0-or-later
#' Laplace approximation for GP posteriors
#'
#' pi(f | data) is approximated by N(f-hat, (K^(-1) + W)^(-1)) with
#' W = diag(-d^2 log p(y | f)) evaluated at the mode.  For a non-Gaussian
#' likelihood the GP posterior is not available in closed form at all, so
#' some Gaussian surrogate is unavoidable; Laplace is the cheapest, and
#' its covariance is the one place the likelihood curvature enters.
#' Logistic classification here, with W = diag(p(1 - p)).
#'
#' Formula: f <- f + 0.3 (K (y - p) - f) to the mode;
#'   Var(f_1) = ((K^(-1) + W)^(-1))_\{11\}.
#'
#' @param x Covariate values; a small separable design when NULL.
#' @param y Binary responses matching \code{x}.
#' @param length Squared-exponential length scale, positive.
#' @param seed Unused; kept for call compatibility.
#' @return List with \code{estimate} (fitted probability at the last
#'   point), \code{mode_probs}, \code{laplace_var_site0},
#'   \code{separates}, \code{method}.
#' @references Ghosal & van der Vaart (2017), Fundamentals of
#'   Nonparametric Bayesian Inference, CUP, section 11.7.5.
#' @export
#' @examples
#' Ghosalgplaplace()
Ghosalgplaplace <- function(x = NULL, y = NULL, length = 0.5, seed = 42) {
  if (is.null(x)) {
    x <- c(0.1, 0.25, 0.4, 0.6, 0.75, 0.9)
    y <- c(0, 0, 0, 1, 1, 1)
  }
  xs <- as.numeric(x)
  ys <- as.numeric(y)
  n <- base::length(xs)
  if (n == 0L) stop("x must be non-empty")
  if (base::length(ys) != n) stop("x and y must have the same length")
  if (length <= 0) stop("length must be positive")
  K <- exp(-0.5 * (outer(xs, xs, "-") / length)^2) + diag(1e-8, n)
  f <- numeric(n)
  for (it in seq_len(100)) {
    p <- 1 / (1 + exp(-f))
    f <- f + 0.3 * (as.numeric(K %*% (ys - p)) - f)
  }
  p <- 1 / (1 + exp(-f))
  W <- p * (1 - p)
  B <- solve(K) + diag(W, n)
  e0 <- c(1, numeric(n - 1))
  var0 <- solve(B, e0)[1]
  .t1_result(estimate = p[n], mode_probs = p,
             laplace_var_site0 = var0,
             separates = p[n] > 0.5 && 0.5 > p[1],
             method = "GP Laplace approximation (GvdV 2017 sec. 11.7.5)")
}

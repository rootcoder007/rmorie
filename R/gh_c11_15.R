# SPDX-License-Identifier: AGPL-3.0-or-later
#' Expectation propagation for Gaussian process classification
#'
#' q(f) = N(mu, Sigma) with Sigma = (K^(-1) + sum_i Lambda_i)^(-1):
#' each likelihood factor is replaced by a Gaussian site, and the sites
#' are refined to a fixed point.  Unlike Laplace, which linearises once
#' at the mode, EP matches moments site by site, so its site precisions
#' carry information from the whole factor rather than only its
#' curvature at a single point.  The simplified version here takes the
#' site precisions from the curvature at the current mean and damps the
#' mean update.
#'
#' Formula: Lambda_i = max(p_i (1 - p_i), 1e-4);
#'   mu <- 0.7 mu + 0.3 (mu + K (y - p));
#'   Sigma_{11} = ((K^(-1) + diag(Lambda))^(-1))_{11}.
#'
#' @param x Covariate values; a small separable design when NULL.
#' @param y Binary responses matching \code{x}.
#' @param length Squared-exponential length scale, positive.
#' @return List with \code{estimate} (fitted probability at the last
#'   point), \code{site_precisions}, \code{ep_var_site0},
#'   \code{separates}, \code{method}.
#' @references Ghosal & van der Vaart (2017), Fundamentals of
#'   Nonparametric Bayesian Inference, CUP, section 11.7.4.
#' @export
#' @examples
#' Ghosalepgp()
Ghosalepgp <- function(x = NULL, y = NULL, length = 0.5) {
  if (is.null(x)) {
    x <- c(0.1, 0.3, 0.7, 0.9)
    y <- c(0, 0, 1, 1)
  }
  xs <- as.numeric(x); ys <- as.numeric(y)
  n <- base::length(xs)
  if (n == 0L) stop("x must be non-empty")
  if (base::length(ys) != n) stop("x and y must have the same length")
  if (length <= 0) stop("length must be positive")
  K <- exp(-0.5 * (outer(xs, xs, "-") / length)^2) + diag(1e-8, n)
  mu <- numeric(n)
  Lam <- rep(0.25, n)
  for (it in seq_len(60)) {
    p <- 1 / (1 + exp(-mu))
    Lam <- pmax(p * (1 - p), 1e-4)
    mu <- 0.7 * mu + 0.3 * (mu + as.numeric(K %*% (ys - p)))
  }
  B <- solve(K) + diag(Lam, n)
  var0 <- solve(B, c(1, numeric(n - 1)))[1]
  p <- 1 / (1 + exp(-mu))
  .t1_result(estimate = p[n], site_precisions = Lam,
             ep_var_site0 = var0,
             separates = p[n] > 0.5 && 0.5 > p[1],
             method = "EP for GP classification (GvdV 2017 sec. 11.7.4)")
}

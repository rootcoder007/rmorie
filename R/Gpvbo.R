# SPDX-License-Identifier: AGPL-3.0-or-later
#' Bayesian optimisation: expected improvement under a GP posterior
#'
#' The stub carried the label "Wang-Frazier (2017)".  Nothing by that
#' author pair in that year matches an acquisition function of this
#' description in Crossref; the attribution is recorded as UNVERIFIED.
#' The acquisition implemented is the standard one the stub's formula
#' describes.  Expected improvement vanishes exactly where the posterior
#' variance is zero, which is what stops the search re-evaluating a
#' point it already knows.
#'
#' Formula: EI(x) = (f_min - mu(x)) Phi(z) + sigma(x) phi(z),
#'   z = (f_min - mu(x)) / sigma(x); EI = 0 when sigma(x) = 0.
#'
#' @param X Evaluated inputs.
#' @param y Observed objective values (minimisation).
#' @param X_grid Candidate points.
#' @param lengthscale,variance,noise GP hyperparameters.
#' @param xi Exploration offset.
#' @return List with \code{estimate}, \code{acquisition}, \code{mean},
#'   \code{sd}, \code{next_index}, \code{next_point}, \code{f_min},
#'   \code{n}, \code{method}.
#' @references Jones, Schonlau and Welch (1998), Efficient global
#'   optimization of expensive black-box functions, Journal of Global
#'   Optimization 13(4):455-492, eq. (15).
#'   \doi{10.1023/A:1008306431147}
#' @export
#' @examples
#' Gpvbo(X = c(1, 2, 3, 4, 5, 6, 7, 8), y = c(1, 2, 3, 4, 5, 6, 7, 8), X_grid = c(1, 2, 3, 4, 5, 6, 7, 8))
Gpvbo <- function(X, y, X_grid, lengthscale = 1, variance = 1, noise = 1e-6, xi = 0) {
  A <- .s03mat(X)
  n <- nrow(A)
  if (n == 0L) stop("gp_variational_bayes_opt: X is empty")
  yv <- .s03vec(y)
  if (length(yv) != n) stop("gp_variational_bayes_opt: X and y have different lengths")
  G <- .s03mat(X_grid)
  if (nrow(G) == 0L) stop("gp_variational_bayes_opt: the candidate grid is empty")
  if (ncol(G) != ncol(A)) stop("gp_variational_bayes_opt: grid and X have different dimensions")
  ell <- as.numeric(lengthscale); var <- as.numeric(variance); s2 <- as.numeric(noise)
  if (ell <= 0 || var <= 0) stop("gp_variational_bayes_opt: lengthscale and variance must be positive")
  if (s2 < 0) stop("gp_variational_bayes_opt: noise must be non-negative")
  kf <- function(P, Q) {
    o <- matrix(0, nrow(P), nrow(Q))
    for (i in seq_len(nrow(P))) for (j in seq_len(nrow(Q)))
      o[i, j] <- var * exp(-0.5 * sum((P[i, ] - Q[j, ])^2) / (ell * ell))
    o
  }
  K <- kf(A, A) + diag(s2, n)
  al <- .s03cholsolve(K, yv)
  Ks <- kf(G, A)
  fmin <- min(yv)
  mu <- numeric(nrow(G)); sdv <- numeric(nrow(G)); ei <- numeric(nrow(G))
  for (j in seq_len(nrow(G))) {
    m <- sum(Ks[j, ] * al)
    v <- .s03cholsolve(K, Ks[j, ])
    s <- sqrt(max(var - sum(Ks[j, ] * v), 0))
    mu[j] <- m; sdv[j] <- s
    if (s <= 0) {
      ei[j] <- 0
    } else {
      imp <- fmin - m - as.numeric(xi)
      z <- imp / s
      ei[j] <- imp * .s03pnorm(z) + s * exp(-0.5 * z * z) / sqrt(2 * pi)
    }
  }
  best <- which.max(ei)
  .t1_result(estimate = ei[best], acquisition = ei, mean = mu, sd = sdv,
             next_index = best, next_point = G[best, ], f_min = fmin, n = n,
             method = "expected improvement, Jones, Schonlau & Welch (1998) eq. (15); stub attribution 'Wang-Frazier (2017)' UNVERIFIED")
}

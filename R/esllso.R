# SPDX-License-Identifier: AGPL-3.0-or-later
#' Lasso by cyclic coordinate descent
#'
#' The L1 penalty is not differentiable at zero, and that is the point:
#' the subgradient condition lets a coefficient sit exactly at zero over a
#' whole range of correlations, so the fit selects as it shrinks. Cyclic
#' coordinate descent exploits the fact that each one-dimensional
#' subproblem has the closed-form soft-threshold solution, so no
#' quadratic program is needed.
#'
#' Columns with no variation are left unpenalised: shrinking an intercept
#' towards zero is a statement about the origin of the response scale, not
#' about model complexity.
#'
#' Formula: \code{min_beta 0.5 ||y - X beta||^2 + lambda ||beta||_1},
#' cycled as \eqn{beta_j <- S(x_j primer + ||x_j||^2 beta_j, lambda) /
# prime ||x_j||^2} with \code{S} the soft-threshold -- Tibshirani (1996);
#' Hastie, Tibshirani and Friedman, The Elements of Statistical Learning,
#' section 3.4.2. The penalty is on the same scale as n times glmnet's
#'
#' @param X Design matrix, n by p.
#' @param y Response of length n.
#' @param lambda_ Penalty, non-negative.
#' @param max_iter Maximum sweeps.
#' @param tol Convergence tolerance on the largest coefficient move.
#' @return List with \code{estimate}, \code{beta}, \code{n_nonzero},
#'   \code{active_set}, \code{objective}, \code{iterations},
#'   \code{converged}, \code{lambda}, \code{n}, \code{p}.
#' @references Tibshirani, R. (1996). Journal of the Royal Statistical
#'   Society Series B 58(1):267-288. \doi{10.1111/j.2517-6161.1996.tb02080.x}.
#' @export
#' @examples
#' Esllso(X = c(1, 2, 3, 4, 5, 6, 7, 8), y = c(1, 2, 3, 4, 5, 6, 7, 8), lambda_ = 5L)
Esllso <- function(X, y, lambda_, max_iter = 10000, tol = 1e-12) {
  X <- as.matrix(X)
  y <- as.numeric(y)
  lam <- as.numeric(lambda_)
  n <- nrow(X)
  p <- ncol(X)
  if (length(y) != n)
    stop(sprintf("X has %d rows but y has %d entries.", n, length(y)))
  if (lam < 0)
    stop(sprintf("the lasso penalty must be non-negative; got %g.", lam))
  colsq <- colSums(X^2)
  if (any(colsq == 0))
    stop("an all-zero column cannot be penalised meaningfully.")
  const <- vapply(seq_len(p), function(j) diff(range(X[, j])) == 0, TRUE)
  beta <- numeric(p)
  r <- y
  converged <- FALSE
  it <- 0L
  for (it in seq_len(as.integer(max_iter))) {
    delta <- 0
    for (j in seq_len(p)) {
      old <- beta[j]
      rho <- sum(X[, j] * r) + colsq[j] * old
      pen <- if (const[j]) 0 else lam
      new <- sign(rho) * max(abs(rho) - pen, 0) / colsq[j]
      if (new != old) {
        r <- r + X[, j] * (old - new)
        beta[j] <- new
        delta <- max(delta, abs(new - old))
      }
    }
    if (delta < tol) { converged <- TRUE
    break }
  }
  active <- which(beta != 0) - 1L
  .t1_result(estimate = beta[1], beta = beta, n_nonzero = length(active),
             active_set = active,
             objective = 0.5 * sum(r^2) + lam * sum(abs(beta[!const])),
             iterations = it, converged = converged, lambda = lam,
             n = n, p = p,
             method = paste("lasso coordinate descent; constant columns",
                            "unpenalised; lambda is n x glmnet's"))
}

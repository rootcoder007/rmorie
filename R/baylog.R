# SPDX-License-Identifier: AGPL-3.0-or-later
#' Posterior mode and normal approximation for logistic regression.
#'
#' The N(0, prior_sd^2) prior keeps the Hessian invertible under
#' separation. The approximation is centred at the MODE and scaled by the
#' inverse Hessian. An intercept column is prepended; do not include one.
#'
#' Formula: Newton step beta <- beta + (X'WX + I/s^2)^-1
#'   (X'(y - p) - beta/s^2), W = diag(p_i(1 - p_i));
#'   Var(beta) ~= (X'WX + I/s^2)^-1
#'
#' @param X Predictors, WITHOUT an intercept column.
#' @param y Binary responses in \{0, 1\}.
#' @param prior_sd Standard deviation of the coefficient prior.
#' @param iters Maximum Newton steps.
#' @param tol Convergence tolerance on the maximum coefficient change.
#' @return List with \code{estimate}, \code{se}, \code{log_posterior},
#'   \code{iterations}, \code{converged}, \code{n}, \code{p}.
#' @references Gelman, Carlin, Stern, Dunson, Vehtari & Rubin (2013),
#'   Bayesian Data Analysis, 3rd edition, Section 4.1 and Chapter 16.
#'   Fetched as the full text of the book from the author's own copy.
#' @export
#' @examples
#' set.seed(1)
#' X <- cbind(1, matrix(rnorm(40), 20, 2))
#' y <- rbinom(20, 1, 0.5)
#' Bayeslogit(X, y)
Bayeslogit <- function(X, y, prior_sd = 10, iters = 50, tol = 1e-12) {
  X <- as.matrix(X); y <- .t1_vec(y); n <- nrow(X)
  if (length(y) != n) stop("X and y must have the same number of rows")
  if (any(!(y %in% c(0, 1)))) stop("y must be binary 0/1")
  s <- as.numeric(prior_sd)
  if (s <= 0) stop("prior_sd must be positive")
  Z <- .t1_cbind1(X); p <- ncol(Z)
  if (n < p) stop("more coefficients than observations")
  b <- rep(0, p); inv_s2 <- 1 / s^2; conv <- 0; it <- 0L
  for (it in seq_len(as.integer(iters))) {
    eta <- as.numeric(Z %*% b)
    mu <- 1 / (1 + exp(-pmin(500, pmax(-500, eta))))
    g <- as.numeric(t(Z) %*% (y - mu)) - b * inv_s2
    H <- t(Z) %*% (Z * (mu * (1 - mu))) + diag(inv_s2, p)
    step <- as.numeric(solve(H, g))
    b <- b + step
    if (max(abs(step)) < tol) { conv <- 1; break }
  }
  eta <- as.numeric(Z %*% b)
  mu <- 1 / (1 + exp(-pmin(500, pmax(-500, eta))))
  H <- t(Z) %*% (Z * (mu * (1 - mu))) + diag(inv_s2, p)
  V <- solve(H)
  ll <- sum(y * eta - log1p(exp(pmin(500, eta))))
  lp <- ll - 0.5 * inv_s2 * sum(b^2)
  .t1_result(estimate = b, se = sqrt(diag(V)), log_posterior = lp,
             iterations = as.numeric(it), converged = conv,
             n = as.numeric(n), p = as.numeric(p),
             method = "Logistic regression posterior mode, BDA3 Section 4.1")
}

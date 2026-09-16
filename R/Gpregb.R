# SPDX-License-Identifier: AGPL-3.0-or-later
#' Gaussian process regression with the hyperparameters integrated out
#'
#' Conditional on the lengthscale and noise the predictive moments and
#' the marginal likelihood are closed form; the hyperparameter average
#' is taken by DETERMINISTIC quadrature over a grid weighted by that
#' marginal likelihood, not by slice sampling, so both language arms
#' land on the same numbers.  A one-point grid collapses to the plain
#' point-estimate GP, which is the identity the tests check.
#'
#' Formula: mu* = k*'(K + s2 I)^\{-1\} y;
#'   var* = k** - k*'(K + s2 I)^\{-1\} k*;
#'   log p(y) = -0.5 y'(K + s2 I)^\{-1\} y - 0.5 log|K + s2 I| - (n/2) log 2 pi.
#'
#' @param X Input matrix, one row per training point.
#' @param y Training responses.
#' @param kernel Unused placeholder kept for the interface.
#' @param X_test Test inputs; the training inputs by default.
#' @param lengthscales Grid of positive lengthscales.
#' @param noises Grid of positive noise variances.
#' @param variance Signal variance.
#' @return List with \code{estimate}, \code{mean}, \code{variance},
#'   \code{weights}, \code{loglik}, \code{n}, \code{method}.
#' @references Rasmussen and Williams (2006), Gaussian Processes for
#'   Machine Learning, MIT Press, eqs. (2.23)-(2.26) and (5.8); Murray
#'   and Adams (2010), Slice sampling covariance hyperparameters of
#'   latent Gaussian models, NIPS 23, arXiv:1006.0868.
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' Gpregb(V, V)
Gpregb <- function(X, y, kernel = NULL, X_test = NULL, lengthscales = NULL,
                   noises = NULL, variance = 1) {
  A <- .s03mat(X)
  yv <- .s03vec(y)
  n <- nrow(A)
  if (n == 0L) stop("gp_regression_bayes: X is empty")
  if (length(yv) != n) stop("gp_regression_bayes: X and y have different lengths")
  Xs <- if (is.null(X_test)) A else .s03mat(X_test)
  ells <- if (is.null(lengthscales)) c(0.5, 1, 2) else .s03vec(lengthscales)
  s2s <- if (is.null(noises)) c(0.01, 0.1) else .s03vec(noises)
  var <- as.numeric(variance)
  if (var <= 0) stop("gp_regression_bayes: variance must be positive")
  if (any(c(ells, s2s) <= 0)) stop("gp_regression_bayes: hyperparameters must be positive")
  kf <- function(P, Q, ell) {
    out <- matrix(0, nrow(P), nrow(Q))
    for (i in seq_len(nrow(P))) for (j in seq_len(nrow(Q)))
      out[i, j] <- var * exp(-0.5 * sum((P[i, ] - Q[j, ])^2) / (ell * ell))
    out
  }
  mus <- list()
  sds <- list()
  lls <- numeric(0)
  for (e in ells) for (s in s2s) {
    K <- kf(A, A, e) + diag(s, n)
    alpha <- .s03cholsolve(K, yv)
    L <- .s03chol(K)
    ll <- -0.5 * sum(yv * alpha) - sum(log(diag(L))) - 0.5 * n * log(2 * pi)
    Ks <- kf(Xs, A, e)
    mu <- as.numeric(Ks %*% alpha)
    sd <- numeric(nrow(Xs))
    for (j in seq_len(nrow(Xs))) {
      v <- .s03cholsolve(K, Ks[j, ])
      sd[j] <- max(var - sum(Ks[j, ] * v), 0)
    }
    mus[[length(mus) + 1L]] <- mu
    sds[[length(sds) + 1L]] <- sd
    lls <- c(lls, ll)
  }
  w <- exp(lls - max(lls))
  w <- w / sum(w)
  m <- nrow(Xs)
  mean <- numeric(m)
  varp <- numeric(m)
  for (j in seq_len(m)) {
    mean[j] <- sum(vapply(seq_along(w), function(g) w[g] * mus[[g]][j], 0))
    varp[j] <- sum(vapply(seq_along(w), function(g) w[g] * (sds[[g]][j] + mus[[g]][j]^2), 0)) - mean[j]^2
  }
  .t1_result(estimate = mean[1], mean = mean, variance = varp, weights = w,
             loglik = lls, n = n,
             method = "GP posterior (R&W eqs. 2.23-2.26) averaged over a marginal-likelihood-weighted hyperparameter grid; Murray & Adams (2010)")
}

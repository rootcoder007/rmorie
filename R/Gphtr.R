# SPDX-License-Identifier: AGPL-3.0-or-later
#' Heteroscedastic Gaussian process regression
#'
#' Two processes: one for the mean, one for the LOG noise variance, so
#' the noise level is itself a smooth function of the input and cannot
#' go negative.  The alternation is the deterministic analogue of the
#' paper's sampler: fit the mean GP under the current noise, form the
#' log squared residuals, smooth them with the second GP, exponentiate,
#' refit.  Zero iterations leaves the plain homoscedastic GP exactly.
#'
#' Formula: y(x) = f(x) + eps(x), f ~ GP, log var(eps(x)) ~ GP.
#'
#' @param X Training inputs.
#' @param y Training responses.
#' @param X_test Test inputs; the training inputs by default.
#' @param lengthscale,variance Mean-process hyperparameters.
#' @param noise0 Starting noise variance.
#' @param noise_lengthscale Lengthscale of the log-variance process.
#' @param iters Alternations; zero gives the homoscedastic GP.
#' @param floor Lower bound on a squared residual before its logarithm.
#' @return List with \code{estimate}, \code{mean}, \code{variance},
#'   \code{noise}, \code{noise_test}, \code{loglik}, \code{n},
#'   \code{method}.
#' @references Goldberg, Williams and Bishop (1998), Regression with
#'   input-dependent noise: a Gaussian process treatment, NIPS 10.
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' Gphtr(V, V)
Gphtr <- function(X, y, X_test = NULL, lengthscale = 1, variance = 1,
                  noise0 = 0.1, noise_lengthscale = 1, iters = 3, floor = 1e-6) {
  A <- .s03mat(X)
  n <- nrow(A)
  if (n == 0L) stop("gp_heteroscedastic: X is empty")
  yv <- .s03vec(y)
  if (length(yv) != n) stop("gp_heteroscedastic: X and y have different lengths")
  ell <- as.numeric(lengthscale)
  var <- as.numeric(variance)
  if (ell <= 0 || var <= 0) stop("gp_heteroscedastic: lengthscale and variance must be positive")
  s0 <- as.numeric(noise0)
  if (s0 <= 0) stop("gp_heteroscedastic: noise0 must be positive")
  if (as.integer(iters) < 0L) stop("gp_heteroscedastic: iters must be non-negative")
  Xs <- if (is.null(X_test)) A else .s03mat(X_test)
  kf <- function(P, Q, e, v) {
    o <- matrix(0, nrow(P), nrow(Q))
    for (i in seq_len(nrow(P))) for (j in seq_len(nrow(Q)))
      o[i, j] <- v * exp(-0.5 * sum((P[i, ] - Q[j, ])^2) / (e * e))
    o
  }
  K <- kf(A, A, ell, var)
  Ks <- kf(Xs, A, ell, var)
  Kn <- kf(A, A, as.numeric(noise_lengthscale), 1)
  Kns <- kf(Xs, A, as.numeric(noise_lengthscale), 1)
  noise <- rep(s0, n)
  for (k in seq_len(as.integer(iters))) {
    M <- K + diag(noise, n)
    al <- .s03cholsolve(M, yv)
    fit <- as.numeric(K %*% al)
    z <- log(pmax((yv - fit)^2, as.numeric(floor)))
    zbar <- mean(z)
    an <- .s03cholsolve(Kn + diag(0.25, n), z - zbar)
    noise <- exp(zbar + as.numeric(Kn %*% an))
  }
  M <- K + diag(noise, n)
  al <- .s03cholsolve(M, yv)
  mu <- as.numeric(Ks %*% al)
  sd <- numeric(nrow(Xs))
  for (j in seq_len(nrow(Xs))) {
    v <- .s03cholsolve(M, Ks[j, ])
    sd[j] <- max(var - sum(Ks[j, ] * v), 0)
  }
  if (as.integer(iters) > 0L) {
    zb <- mean(log(pmax(noise, as.numeric(floor))))
    an2 <- .s03cholsolve(Kn + diag(0.25, n), log(noise) - zb)
    noise_test <- exp(zb + as.numeric(Kns %*% an2))
  } else {
    noise_test <- rep(s0, nrow(Xs))
  }
  L <- .s03chol(M)
  ll <- -0.5 * sum(yv * al) - sum(log(diag(L))) - 0.5 * n * log(2 * pi)
  .t1_result(estimate = mu[1], mean = mu, variance = sd, noise = noise,
             noise_test = noise_test, loglik = ll, n = n,
             method = "alternating mean GP and log-variance GP, Goldberg, Williams & Bishop (1998)")
}

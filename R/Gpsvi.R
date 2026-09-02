# SPDX-License-Identifier: AGPL-3.0-or-later
#' Sparse variational GP regression
#'
#' For a Gaussian likelihood the optimal variational distribution is
#' available in closed form, so the stochastic gradient loop of the
#' paper is unnecessary and the collapsed bound can be evaluated
#' exactly.  When the inducing inputs ARE the training inputs the trace
#' term is zero and both the bound and the predictive mean coincide with
#' full GP regression -- that identity is the anchor, and it is the one
#' thing a sparse approximation must satisfy.
#'
#' Formula: Q = K_nm K_mm^\{-1\} K_mn;
#'   L = log N(y; 0, Q + s2 I) - tr(K_nn - Q) / (2 s2).
#'
#' @param X Training inputs.
#' @param y Training responses.
#' @param X_test Test inputs; the training inputs by default.
#' @param inducing Inducing inputs; the training inputs by default.
#' @param batch_size Unused placeholder kept for the interface.
#' @param lengthscale,variance,noise GP hyperparameters.
#' @param jitter Ridge added to K_mm.
#' @return List with \code{estimate}, \code{mean}, \code{variance},
#'   \code{elbo}, \code{gaussian_term}, \code{trace_term}, \code{n},
#'   \code{method}.
#' @references Hensman, Fusi and Lawrence (2013), Gaussian processes for
#'   big data, UAI 2013, arXiv:1309.6835; Titsias (2009), AISTATS 2009,
#'   PMLR 5:567-574.
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' Gpsvi(V, V)
Gpsvi <- function(X, y, X_test = NULL, inducing = NULL, batch_size = NULL,
                  lengthscale = 1, variance = 1, noise = 0.1, jitter = 1e-9) {
  A <- .s03mat(X); yv <- .s03vec(y)
  n <- nrow(A)
  if (n == 0L) stop("gp_stochastic_vi: X is empty")
  if (length(yv) != n) stop("gp_stochastic_vi: X and y have different lengths")
  Z <- if (is.null(inducing)) A else .s03mat(inducing)
  m <- nrow(Z)
  if (m == 0L) stop("gp_stochastic_vi: no inducing inputs")
  if (ncol(Z) != ncol(A)) stop("gp_stochastic_vi: inducing inputs have the wrong dimension")
  Xs <- if (is.null(X_test)) A else .s03mat(X_test)
  ell <- as.numeric(lengthscale); var <- as.numeric(variance); s2 <- as.numeric(noise)
  if (ell <= 0 || var <= 0 || s2 <= 0) stop("gp_stochastic_vi: lengthscale, variance and noise must be positive")
  kf <- function(P, Q) {
    out <- matrix(0, nrow(P), nrow(Q))
    for (i in seq_len(nrow(P))) for (j in seq_len(nrow(Q)))
      out[i, j] <- var * exp(-0.5 * sum((P[i, ] - Q[j, ])^2) / (ell * ell))
    out
  }
  Kmm <- kf(Z, Z) + diag(as.numeric(jitter), m)
  Knm <- kf(A, Z)
  Kinv <- matrix(0, n, m)
  for (i in seq_len(n)) Kinv[i, ] <- .s03cholsolve(Kmm, Knm[i, ])
  Q <- Knm %*% t(Kinv)
  trace <- sum(var - diag(Q))
  S <- Q + diag(s2, n)
  a <- .s03cholsolve(S, yv)
  L <- .s03chol(S)
  gauss <- -0.5 * sum(yv * a) - sum(log(diag(L))) - 0.5 * n * log(2 * pi)
  elbo <- gauss - trace / (2 * s2)
  Ksm <- kf(Xs, Z)
  mu <- numeric(nrow(Xs)); sd <- numeric(nrow(Xs))
  for (j in seq_len(nrow(Xs))) {
    row <- as.numeric(Kinv %*% Ksm[j, ])
    mu[j] <- sum(row * a)
    z <- .s03cholsolve(S, row)
    sd[j] <- max(var - sum(row * z), 0)
  }
  .t1_result(estimate = mu[1], mean = mu, variance = sd, elbo = elbo,
             gaussian_term = gauss, trace_term = trace, n = n,
             method = "collapsed sparse bound of Titsias (2009) as used by Hensman, Fusi & Lawrence (2013)")
}

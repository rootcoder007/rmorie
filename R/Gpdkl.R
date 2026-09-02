# SPDX-License-Identifier: AGPL-3.0-or-later
#' Deep kernel learning
#'
#' A base kernel applied to the output of a neural network rather than
#' to the raw inputs.  The network is a fixed one-hidden-layer tanh map
#' whose weights are supplied by the caller, so there is no training
#' loop and both language arms follow the same deterministic map; an
#' identity map recovers the plain RBF GP exactly, which is the identity
#' the tests check.
#'
#' Formula: k(x, x') -> k(g(x, w), g(x', w)).
#'
#' @param X Training inputs.
#' @param y Training responses.
#' @param X_test Test inputs; the training inputs by default.
#' @param nn List with W1, b1, W2, b2 and optionally tanh; NULL means the
#'   identity map.
#' @param lengthscale,variance,noise GP hyperparameters.
#' @return List with \code{estimate}, \code{mean}, \code{variance},
#'   \code{loglik}, \code{features}, \code{n}, \code{method}.
#' @references Wilson, Hu, Salakhutdinov and Xing (2016), Deep kernel
#'   learning, AISTATS 2016, PMLR 51:370-378, eq. (1). arXiv:1511.02222
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' Gpdkl(V, V)
Gpdkl <- function(X, y, X_test = NULL, nn = NULL, lengthscale = 1,
                  variance = 1, noise = 0.01) {
  A <- .s03mat(X); yv <- .s03vec(y)
  n <- nrow(A)
  if (n == 0L) stop("deep_kernel_gp: X is empty")
  if (length(yv) != n) stop("deep_kernel_gp: X and y have different lengths")
  d <- ncol(A)
  Xs <- if (is.null(X_test)) A else .s03mat(X_test)
  ell <- as.numeric(lengthscale); var <- as.numeric(variance); s2 <- as.numeric(noise)
  if (ell <= 0 || var <= 0) stop("deep_kernel_gp: lengthscale and variance must be positive")
  if (s2 < 0) stop("deep_kernel_gp: noise must be non-negative")
  if (is.null(nn)) {
    GA <- A; GS <- Xs
  } else {
    W1 <- .s03mat(nn$W1); W2 <- .s03mat(nn$W2)
    b1 <- if (is.null(nn$b1)) rep(0, ncol(W1)) else .s03vec(nn$b1)
    b2 <- if (is.null(nn$b2)) rep(0, ncol(W2)) else .s03vec(nn$b2)
    act <- if (is.null(nn$tanh)) TRUE else isTRUE(nn$tanh)
    if (nrow(W1) != d) stop("deep_kernel_gp: W1 must have one row per input feature")
    if (nrow(W2) != ncol(W1)) stop("deep_kernel_gp: W2 must have one row per hidden unit")
    ap <- function(P) {
      out <- matrix(0, nrow(P), ncol(W2))
      for (i in seq_len(nrow(P))) {
        h <- as.numeric(P[i, ] %*% W1) + b1
        if (act) h <- tanh(h)
        out[i, ] <- as.numeric(h %*% W2) + b2
      }
      out
    }
    GA <- ap(A); GS <- ap(Xs)
  }
  kf <- function(P, Q) {
    out <- matrix(0, nrow(P), nrow(Q))
    for (i in seq_len(nrow(P))) for (j in seq_len(nrow(Q)))
      out[i, j] <- var * exp(-0.5 * sum((P[i, ] - Q[j, ])^2) / (ell * ell))
    out
  }
  K <- kf(GA, GA) + diag(s2, n)
  alpha <- .s03cholsolve(K, yv)
  Ks <- kf(GS, GA)
  mu <- as.numeric(Ks %*% alpha)
  sd <- numeric(nrow(GS))
  for (j in seq_len(nrow(GS))) {
    z <- .s03cholsolve(K, Ks[j, ])
    sd[j] <- max(var - sum(Ks[j, ] * z), 0)
  }
  L <- .s03chol(K)
  ll <- -0.5 * sum(yv * alpha) - sum(log(diag(L))) - 0.5 * n * log(2 * pi)
  .t1_result(estimate = mu[1], mean = mu, variance = sd, loglik = ll,
             features = ncol(GA), n = n,
             method = "k(g(x), g(x')) with a fixed tanh feature map, Wilson et al. (2016) eq. (1)")
}

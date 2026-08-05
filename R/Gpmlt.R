# SPDX-License-Identifier: AGPL-3.0-or-later
#' Multi-task Gaussian process
#'
#' The covariance between the values of two tasks factorises into an
#' inter-task matrix times an input kernel, so the joint covariance is a
#' Kronecker product.  A diagonal task covariance means the tasks share
#' nothing and the model collapses to independent single-task GPs -- the
#' tests check that collapse exactly, because it is the only way to be
#' sure the Kronecker indexing is the right way round.
#'
#' Formula: cov(f_l(x), f_m(x')) = K^f_{lm} k^x(x, x').
#'
#' @param X Inputs shared by every task, one row per point.
#' @param y_tasks Matrix of observations, one row per task.
#' @param X_test Test inputs; the training inputs by default.
#' @param task_cov Inter-task covariance; the identity by default.
#' @param lengthscale,variance,noise GP hyperparameters.
#' @return List with \code{estimate}, \code{mean}, \code{loglik},
#'   \code{tasks}, \code{n}, \code{method}.
#' @references Bonilla, Chai and Williams (2008), Multi-task Gaussian
#'   process prediction, NIPS 20, eqs. (1)-(2).
#' @export
Gpmlt <- function(X, y_tasks, X_test = NULL, task_cov = NULL, lengthscale = 1,
                  variance = 1, noise = 0.01) {
  A <- .s03mat(X)
  n <- nrow(A)
  if (n == 0L) stop("gp_multitask: X is empty")
  Y <- .s03mat(y_tasks)
  T <- nrow(Y)
  if (T == 0L) stop("gp_multitask: no tasks supplied")
  if (ncol(Y) != n) stop("gp_multitask: every task needs one observation per input")
  Kf <- if (is.null(task_cov)) diag(1, T) else .s03mat(task_cov)
  if (nrow(Kf) != T || ncol(Kf) != T) stop("gp_multitask: task_cov must be T x T")
  ell <- as.numeric(lengthscale); var <- as.numeric(variance); s2 <- as.numeric(noise)
  if (ell <= 0 || var <= 0) stop("gp_multitask: lengthscale and variance must be positive")
  if (s2 < 0) stop("gp_multitask: noise must be non-negative")
  Xs <- if (is.null(X_test)) A else .s03mat(X_test)
  kf <- function(P, Q) {
    out <- matrix(0, nrow(P), nrow(Q))
    for (i in seq_len(nrow(P))) for (j in seq_len(nrow(Q)))
      out[i, j] <- var * exp(-0.5 * sum((P[i, ] - Q[j, ])^2) / (ell * ell))
    out
  }
  Kx <- kf(A, A)
  K <- kronecker(Kf, Kx) + diag(s2, T * n)
  yv <- as.numeric(t(Y))
  alpha <- .s03cholsolve(K, yv)
  Ksx <- kf(Xs, A)
  mean <- matrix(0, T, nrow(Xs))
  for (a in seq_len(T)) for (j in seq_len(nrow(Xs))) {
    s <- 0
    for (b in seq_len(T)) s <- s + Kf[a, b] * sum(Ksx[j, ] * alpha[((b - 1L) * n + 1L):(b * n)])
    mean[a, j] <- s
  }
  L <- .s03chol(K)
  ll <- -0.5 * sum(yv * alpha) - sum(log(diag(L))) - 0.5 * (T * n) * log(2 * pi)
  .t1_result(estimate = mean[1, 1], mean = mean, loglik = ll, tasks = T, n = n,
             method = "K = K^f (x) K^x, Bonilla, Chai & Williams (2008) eqs. (1)-(2)")
}

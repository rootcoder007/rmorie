# SPDX-License-Identifier: AGPL-3.0-or-later
#' Functional linear regression
#'
#' E(Y | X) = int X(t) beta(t) dt with beta given a series prior.  Once X
#' is expanded on the same basis the model collapses to ridge regression
#' on the coefficient scores, which is exactly why a series prior is the
#' natural one here: the infinite-dimensional problem becomes a finite
#' penalised one with no approximation.  Solved by Gauss-Seidel, the
#' normal equations being diagonally dominant after the unit ridge.
#'
#' Formula: beta_k <- sum_i (y_i - sum_{j != k} beta_j s_ij) s_ik
#'   / (sum_i s_ik^2 + 1).
#'
#' @param n Sample size.
#' @param K Number of basis scores.
#' @param seed Seed for the deterministic draws.
#' @return List with \code{estimate} (max absolute coefficient error),
#'   \code{beta_hat}, \code{method}.
#' @references Ghosal & van der Vaart (2017), Fundamentals of
#'   Nonparametric Bayesian Inference, CUP, section 10.4.5.
#' @export
Ghosalfuncreg <- function(n = 300, K = 4, seed = 42) {
  n <- as.integer(n); K <- as.integer(K)
  if (n < 1L) stop("n must be positive")
  if (K < 1L) stop("K must be positive")
  e <- .ghc_rng(seed)
  beta0 <- c(1, -0.5, 0.25, 0)
  if (K != length(beta0)) stop("K must equal 4 for the built-in truth")
  scores <- matrix(0, n, K)
  ys <- numeric(n)
  for (i in seq_len(n)) {
    s <- .ghc_norm(e, K)
    scores[i, ] <- s
    ys[i] <- sum(beta0 * s) + 0.2 * .ghc_norm(e, 1L)
  }
  bhat <- numeric(K)
  den <- colSums(scores^2) + 1
  for (it in seq_len(300)) {
    for (k in seq_len(K)) {
      resid <- ys - as.numeric(scores[, -k, drop = FALSE] %*% bhat[-k])
      bhat[k] <- sum(resid * scores[, k]) / den[k]
    }
  }
  .t1_result(estimate = max(abs(bhat - beta0)), beta_hat = bhat,
             method = "functional regression (GvdV 2017 sec. 10.4.5)")
}

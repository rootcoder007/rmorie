# SPDX-License-Identifier: AGPL-3.0-or-later
#' Finite-random-series regression
#'
#' Y_i = f(x_i) + e_i with f = sum_\{k <= K\} beta_k phi_k.  Choosing K by
#' the conjugate evidence rather than fixing it delivers the rate
#' n^(-2s/(2s+1)) without knowing s.  The design is the equispaced
#' cosine basis, so the ridge fit is a per-coefficient shrinkage of the
#' empirical Fourier coefficient and no matrix has to be formed.
#'
#' Formula: beta_k-hat = (n/(n+1)) mean_i y_i phi_k(x_i);
#'   evidence(K) = -n log(RSS/n)/2 - K log(n)/2.
#'
#' @param n Sample size.
#' @param seed Seed for the deterministic draws.
#' @return List with \code{estimate} (integrated squared error against
#'   the truth), \code{K_hat}, \code{method}.
#' @references Ghosal & van der Vaart (2017), Fundamentals of
#'   Nonparametric Bayesian Inference, CUP, section 10.4.2.
#' @export
#' @examples
#' Ghosalfrsreg()
Ghosalfrsreg <- function(n = 600, seed = 42) {
  n <- as.integer(n)
  if (n < 2L) stop("n must be at least 2")
  e <- .ghc_rng(seed)
  xs <- (seq_len(n) - 0.5) / n
  f0 <- sin(2 * pi * xs)
  ys <- f0 + 0.4 * .ghc_norm(e, n)
  phi <- function(x, k) sqrt(2) * cos((k + 1) * pi * x)
  best_ev <- NULL
  K_hat <- NA_integer_
  coef <- numeric(0)
  for (K in 1:8) {
    cf <- vapply(0:(K - 1), function(k)
      sum(ys * phi(xs, k)) / n * n / (n + 1), numeric(1))
    P <- outer(xs, 0:(K - 1), phi)
    rss <- sum((ys - as.numeric(P %*% cf))^2)
    ev <- -0.5 * n * log(rss / n) - 0.5 * K * log(n)
    if (is.null(best_ev) || ev > best_ev) { best_ev <- ev
    K_hat <- K
    coef <- cf }
  }
  P <- outer(xs, 0:(K_hat - 1), phi)
  risk <- sum((as.numeric(P %*% coef) - f0)^2) / n
  .t1_result(estimate = risk, K_hat = K_hat,
             method = "finite random series regression (GvdV 2017 sec. 10.4.2)")
}

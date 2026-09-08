# SPDX-License-Identifier: AGPL-3.0-or-later
#' Random-series prior with random dimension
#'
#' K ~ pi_n and beta_k | K iid N(0, sigma^2).  Putting a prior on the
#' number of retained basis coefficients makes the posterior on K
#' concentrate near the effective dimension of the truth, and that is
#' what lets the contraction rate adapt to an unknown smoothness.  The
#' evidence for each K is conjugate, so the whole posterior on K is exact.
#'
#' Formula: pi(K | y) propto exp(log m_K(y) - lam K log n); the reported
#'   estimate is the posterior mean sum_K K pi(K | y).
#'
#' @param K_true Number of nonzero coefficients in the truth.
#' @param n Precision (sample size) of each coefficient.
#' @param lam Complexity-penalty scale.
#' @param K_max Largest dimension considered.
#' @param seed Seed for the deterministic draw.
#' @return List with \code{estimate} (posterior mean of K),
#'   \code{K_posterior}, \code{mode_K}, \code{method}.
#' @references Ghosal & van der Vaart (2017), Fundamentals of
#'   Nonparametric Bayesian Inference, CUP, section 10.4.
#' @export
#' @examples
#' Ghosalrndseriespr()
Ghosalrndseriespr <- function(K_true = 4, n = 1000, lam = 0.5,
                              K_max = 15, seed = 42) {
  if (K_max < 1) stop("K_max must be at least 1")
  if (n <= 1) stop("n must exceed 1")
  e <- .ghc_rng(seed)
  y <- ifelse(seq_len(K_max) <= K_true, 0.9, 0) + .ghc_norm(e, K_max) / sqrt(n)
  logs <- vapply(0:K_max, function(K) .ghc_log_evidence_K(y, n, K) -
                   lam * K * log(n), numeric(1))
  w <- exp(logs - max(logs))
  post <- w / sum(w)
  .t1_result(estimate = sum((0:K_max) * post),
             K_posterior = post,
             mode_K = which.max(post) - 1,
             method = "random series prior (GvdV 2017 sec. 10.4)")
}

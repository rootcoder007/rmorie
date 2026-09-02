# SPDX-License-Identifier: AGPL-3.0-or-later
#' Adaptive hierarchical model prior
#'
#' Pi = sum_k pi_k Pi_k with pi_k proportional to exp(-lam k log n): the
#' posterior over the model index k concentrates, so the contraction rate
#' adapts to the truth without the truth being known.  The evidence of
#' each sub-model is available in closed form here (conjugate normal
#' means), so the model posterior is exact rather than sampled.
#'
#' Formula: log m_K(y) = sum_k \[-log(2 pi s_k^2)/2 - y_k^2/(2 s_k^2)\],
#'   s_k^2 = 1/n + tau2 1{k < K};  pi_K propto m_K(y) exp(-lam K log n).
#'
#' @param y Numeric vector of observed coordinates; simulated when NULL.
#' @param n Precision (sample size) of each coordinate.
#' @param K_true Number of nonzero coordinates in the simulated truth.
#' @param lam Complexity-penalty scale of the model prior.
#' @param K_max Largest model dimension considered.
#' @param seed Seed for the deterministic draw of \code{y}.
#' @return List with \code{estimate} (posterior mode of K),
#'   \code{model_posterior}, \code{K_true}, \code{method}.
#' @references Ghosal & van der Vaart (2017), Fundamentals of
#'   Nonparametric Bayesian Inference, CUP, section 10.1.
#' @export
#' @examples
#' Ghosaladaptthm()
Ghosaladaptthm <- function(y = NULL, n = 200, K_true = 3, lam = 1,
                           K_max = 12, seed = 42) {
  if (K_max < 1) stop("K_max must be at least 1")
  if (n <= 0) stop("n must be positive")
  e <- .ghc_rng(seed)
  if (is.null(y)) {
    z <- .ghc_norm(e, K_max)
    y <- ifelse(seq_len(K_max) <= K_true, 1, 0) + z / sqrt(n)
  } else {
    y <- as.numeric(y)
    if (length(y) == 0L) stop("y must be non-empty")
  }
  logs <- vapply(0:K_max, function(K) .ghc_log_evidence_K(y, n, K) -
                   lam * K * log(n), numeric(1))
  w <- exp(logs - max(logs))
  post <- w / sum(w)
  .t1_result(estimate = which.max(post) - 1,
             model_posterior = post,
             K_true = K_true,
             method = "adaptive model prior (GvdV 2017 sec. 10.1)")
}

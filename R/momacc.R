# SPDX-License-Identifier: AGPL-3.0-or-later
#' Moments accountant for DP-SGD
#'
#' Formula: alpha(lam) <= T q^2 lam(lam+1)/((1-q) sigma^2); delta = min_lam exp(alpha(lam) - lam eps)
#'
#' @param sigma Noise multiplier (noise sd over clipping norm).
#' @param sample_rate Lot sampling probability q in (0, 1).
#' @param steps Number of SGD steps composed.
#' @param delta Target delta.
#' @param max_order Largest integer moment order searched.

#' @param sigma See Usage.
#' @param sample_rate See Usage.
#' @param steps See Usage.
#' @param delta See Usage.
#' @param max_order See Usage.
#' @return List with ``epsilon``, ``order``, ``logmgf``, ``delta``, ``sigma``, ``sample_rate``, ``steps``.
#' @references Abadi, Chu, Goodfellow, McMahan, Mironov, Talwar and Zhang (2016), Deep Learning with Differential Privacy, CCS'16, arXiv:1607.00133. Lemma 3 for the sampled-Gaussian log moment, Theorem 2 for composability and the tail bound. Verified against the paper.
#' @export
Dpacct <- function(sigma, sample_rate, steps, delta = 1e-05, max_order = 64) {
  sigma <- as.numeric(sigma)
  q <- as.numeric(sample_rate)
  steps <- as.integer(steps)
  delta <- as.numeric(delta)
  if (sigma <= 0 || q <= 0 || q >= 1 || steps < 1)
    stop("need sigma > 0, 0 < sample_rate < 1, steps >= 1")
  lam <- 2:as.integer(max_order)
  a <- steps * q * q * lam * (lam + 1) / ((1 - q) * sigma * sigma)
  eps <- (a + log(1 / delta)) / lam
  k <- which.min(eps)
  .t1_result(epsilon = eps[k], order = lam[k], logmgf = a[k], delta = delta,
             sigma = sigma, sample_rate = q, steps = steps,
             method = "Moments accountant (sampled Gaussian, DP-SGD)")
}

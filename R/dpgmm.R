# SPDX-License-Identifier: AGPL-3.0-or-later
#' Dirichlet-process Gaussian mixture, truncated stick-breaking
#'
#' Sethuraman (1994), Statistica Sinica 4(2), 639-650, for pi_k = V_k
#' prod_(j<k)(1 - V_j) with V_k ~ Beta(1, alpha); Blei and Jordan (2006),
#' Variational inference for Dirichlet process mixtures, Bayesian Analysis
#' 1(1), 121-143, for the truncated stick-breaking approximation at a
#' fixed K.  Neither was retrievable here as a full text; both are quoted
#' in their standard published form, and the construction is reproduced in
#' Teh et al. (2006), eqs. (5)-(6), which WAS fetched.
#'
#' Determinism: prior weights from the exact Beta quantile at
#' low-discrepancy points, components fitted by EM -- a deterministic
#' fixed point, not a Gibbs sampler.
#'
#' @param y the data.
#' @param alpha DP concentration.
#' @param prior_mu,prior_sigma the base measure.
#' @param truncation number of components K.
#' @param max_iter,tol EM controls.
#' @return list: estimate, weights, mu, sigma, loglik, prior_pi, n, method.
#' @keywords internal
#' @examples
#' Dpgmm(c(0, 0.2, 5, 5.3, 5.1), 1, 0, 1, 3)$weights
#' @export
Dpgmm <- function(y, alpha = 1, prior_mu = 0, prior_sigma = 1,
                  truncation = 5, max_iter = 200, tol = 1e-13) {
  v <- .s03vec(y)
  n <- length(v)
  K <- as.integer(truncation)
  prior <- Stickw(alpha, K)$pi
  tot <- 0
  for (x in prior) tot <- tot + x
  prior <- if (tot > 0) prior / tot else rep(1 / K, K)
  lo <- min(v)
  hi <- max(v)
  mu <- numeric(K)
  for (i in seq_len(K)) mu[i] <- lo + (hi - lo) * (i - 1 + 0.5) / K
  sd_ <- rep(max((hi - lo) / K, 1e-6), K)
  w <- prior
  ll <- -Inf
  for (it in seq_len(as.integer(max_iter))) {
    R <- matrix(0, n, K)
    newll <- 0
    for (i in seq_len(n)) {
      lp <- numeric(K)
      for (cc in seq_len(K)) {
        z <- (v[i] - mu[cc]) / sd_[cc]
        lp[cc] <- log(if (w[cc] > 1e-300) w[cc] else 1e-300) - 0.5 * z * z -
          log(sd_[cc]) - 0.5 * log(2 * pi)
      }
      m <- .s03logsumexp(lp)
      newll <- newll + m
      for (cc in seq_len(K)) R[i, cc] <- exp(lp[cc] - m)
    }
    for (cc in seq_len(K)) {
      nk <- 0
      for (i in seq_len(n)) nk <- nk + R[i, cc]
      eff <- nk + as.numeric(alpha) * prior[cc]
      w[cc] <- eff
      s <- 0
      for (i in seq_len(n)) s <- s + R[i, cc] * v[i]
      mu[cc] <- if (eff > 0) (s + as.numeric(alpha) * prior[cc] * as.numeric(prior_mu)) / eff else as.numeric(prior_mu)
      q <- 0
      for (i in seq_len(n)) q <- q + R[i, cc] * (v[i] - mu[cc])^2
      q <- q + as.numeric(alpha) * prior[cc] * as.numeric(prior_sigma)^2
      sd_[cc] <- if (eff > 0) sqrt(q / eff) else as.numeric(prior_sigma)
      if (sd_[cc] < 1e-8) sd_[cc] <- 1e-8
    }
    wt <- 0
    for (cc in seq_len(K)) wt <- wt + w[cc]
    w <- w / wt
    if (abs(newll - ll) < tol) { ll <- newll
    break }
    ll <- newll
  }
  active <- 0L
  for (x in w) if (x > 1 / (10 * K)) active <- active + 1L
  list(estimate = as.numeric(active), weights = w, mu = mu, sigma = sd_,
       loglik = ll, prior_pi = prior, n = n,
       method = "Truncated stick-breaking DP mixture fitted by EM (Sethuraman 1994; Blei and Jordan 2006)")
}

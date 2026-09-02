# SPDX-License-Identifier: AGPL-3.0-or-later
#' Hierarchical Dirichlet process Gaussian mixture
#'
#' Teh, Jordan, Beal and Blei (2006), JASA 101(476), 1566-1581 (FETCHED),
#' eqs. (2) and (19), applied to a Gaussian likelihood: every group has
#' its own mixing proportions but the COMPONENT LOCATIONS are shared,
#' which is the entire reason for the hierarchy (their section 3).
#'
#' Determinism: locations fitted by EM with the HDP weights entering as
#' pseudo-counts; the stick-breaking prior from the exact Beta quantile at
#' low-discrepancy points.  No draws.
#'
#' @param y the data.
#' @param groups group label per observation.
#' @param gamma,alpha concentrations.
#' @param truncation number of components.
#' @param max_iter,tol EM controls.
#' @return list: estimate, loglik, mu, sigma, pi, beta, n, method.
#' @keywords internal
#' @examples
#' Hdpgmm(c(0, 0.2, 5, 5.3), c("a", "a", "b", "b"), 1, 1, 2)$mu
#' @export
Hdpgmm <- function(y, groups = NULL, gamma = 1, alpha = 1, truncation = 4,
                   max_iter = 200, tol = 1e-13) {
  v <- .s03vec(y)
  n <- length(v)
  g <- as.character(if (!is.null(groups)) groups else rep(0, n))
  ids <- character(0)
  for (cc in g) if (!(cc %in% ids)) ids <- c(ids, cc)
  J <- length(ids)
  gi <- match(g, ids)
  K <- as.integer(truncation)
  beta <- Stickw(gamma, K)$pi
  tot <- 0
  for (x in beta) tot <- tot + x
  beta <- if (tot > 0) beta / tot else rep(1 / K, K)
  lo <- min(v)
  hi <- max(v)
  mu <- numeric(K)
  for (t in seq_len(K)) mu[t] <- lo + (hi - lo) * (t - 1 + 0.5) / K
  sd_ <- rep(max((hi - lo) / K, 1e-6), K)
  pi_ <- matrix(rep(beta, each = J), J, K)
  ll <- -Inf
  for (it in seq_len(as.integer(max_iter))) {
    R <- matrix(0, n, K)
    newll <- 0
    for (i in seq_len(n)) {
      lp <- numeric(K)
      for (t in seq_len(K)) {
        w <- pi_[gi[i], t]
        z <- (v[i] - mu[t]) / sd_[t]
        lp[t] <- log(if (w > 1e-300) w else 1e-300) - 0.5 * z * z -
          log(sd_[t]) - 0.5 * log(2 * pi)
      }
      m <- .s03logsumexp(lp)
      newll <- newll + m
      for (t in seq_len(K)) R[i, t] <- exp(lp[t] - m)
    }
    for (j in seq_len(J)) {
      nj <- 0
      row <- numeric(K)
      for (i in seq_len(n)) if (gi[i] == j) {
        for (t in seq_len(K)) row[t] <- row[t] + R[i, t]
        nj <- nj + 1
      }
      pi_[j, ] <- (as.numeric(alpha) * beta + row) / (as.numeric(alpha) + nj)
    }
    for (t in seq_len(K)) {
      nk <- 0
      s <- 0
      for (i in seq_len(n)) { nk <- nk + R[i, t]
      s <- s + R[i, t] * v[i] }
      eff <- nk + as.numeric(gamma) * beta[t]
      if (nk > 1e-12) mu[t] <- s / nk
      q <- 0
      for (i in seq_len(n)) q <- q + R[i, t] * (v[i] - mu[t])^2
      if (eff > 0) sd_[t] <- sqrt(q / eff)
      if (sd_[t] < 1e-8) sd_[t] <- 1e-8
    }
    if (abs(newll - ll) < tol) { ll <- newll
    break }
    ll <- newll
  }
  list(estimate = ll, loglik = ll, mu = mu, sigma = sd_, pi = pi_,
       beta = beta, n = n,
       method = "HDP mixture with shared component locations (Teh et al. 2006, eqs. 2 and 19)")
}

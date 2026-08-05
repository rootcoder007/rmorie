# SPDX-License-Identifier: AGPL-3.0-or-later

#' CRP Gibbs sampler with auxiliary parameters
#'
#' Formula: P(z_i | z_{-i}) proportional to n_k^{-i} f(y_i | theta_k),
#' and to (alpha/m) f(y_i | theta_aux) for each of m auxiliary draws
#'
#' Neal's algorithm 8: the cluster parameters are kept explicit rather
#' than integrated out, and m fresh draws from the base measure stand in
#' for the infinitely many empty tables.  That makes the sampler valid
#' for non-conjugate base measures, unlike the collapsed algorithm 3;
#' here the conjugate normal case is used so the two can be compared.
#'
#' @param y Observations.
#' @param alpha Concentration, strictly positive.
#' @param n_iter Number of sweeps.
#' @param m Number of auxiliary components.
#' @param mu0,tau2 Base measure N(mu0, tau2).
#' @param sigma2 Known within-cluster variance.
#' @param seed Seed of the deterministic stream.
#' @return List with \code{estimate} (number of clusters), \code{z},
#'   \code{counts}, \code{theta}, \code{n_clusters}, \code{loglik},
#'   \code{n}, \code{method}.
#' @references Neal (2000), J. Comput. Graph. Statist. 9(2):249-265,
#'   algorithm 8.
#' @export
Crpgib <- function(y, alpha = 1, n_iter = 50, m = 3, mu0 = 0, tau2 = 10,
                   sigma2 = 1, seed = 42) {
  y <- .s03vec(y)
  n <- length(y)
  if (n == 0L) stop("empty input: y has no observations")
  if (!(alpha > 0)) stop("alpha must be strictly positive")
  m <- as.integer(m)
  if (m < 1L) stop("m must be at least 1")
  if (!(tau2 > 0 && sigma2 > 0))
    stop("tau2 and sigma2 must be strictly positive")
  e <- .ghc_rng(seed)
  z <- rep(1L, n)
  counts <- c(n)
  theta <- c(sum(y) / n)
  for (it in seq_len(as.integer(n_iter))) {
    for (i in seq_len(n)) {
      k <- z[i]
      counts[k] <- counts[k] - 1L
      K <- length(counts)
      aux <- numeric(m)
      for (j in seq_len(m)) aux[j] <- .ghc_norm(e, 1L, mu0, sqrt(tau2))
      if (counts[k] == 0L) aux[1] <- theta[k]
      w <- numeric(K + m)
      for (c in seq_len(K))
        w[c] <- if (counts[c] > 0L)
          counts[c] * exp(.crp_norm_logpdf(y[i], theta[c], sigma2)) else 0
      for (j in seq_len(m))
        w[K + j] <- alpha / m * exp(.crp_norm_logpdf(y[i], aux[j], sigma2))
      tot <- 0
      for (v in w) tot <- tot + v
      u <- .ghc_unif(e, 1L) * tot
      acc <- 0
      pick <- K + m
      for (c in seq_len(K + m)) {
        acc <- acc + w[c]
        if (u <= acc) { pick <- c; break }
      }
      if (pick > K) {
        theta <- c(theta, aux[pick - K])
        counts <- c(counts, 0L)
        pick <- length(counts)
      }
      z[i] <- pick
      counts[pick] <- counts[pick] + 1L
    }
    keep <- which(counts > 0L)
    remap <- integer(length(counts))
    remap[keep] <- seq_along(keep)
    counts <- counts[keep]
    theta <- theta[keep]
    z <- remap[z]
    for (c in seq_along(counts)) {
      s <- 0
      for (i in seq_len(n)) if (z[i] == c) s <- s + y[i]
      prec <- 1 / tau2 + counts[c] / sigma2
      mpost <- (mu0 / tau2 + s / sigma2) / prec
      theta[c] <- .ghc_norm(e, 1L, mpost, sqrt(1 / prec))
    }
  }
  ll <- 0
  for (i in seq_len(n)) ll <- ll + .crp_norm_logpdf(y[i], theta[z[i]], sigma2)
  .t1_result(estimate = length(counts), z = z - 1L, counts = counts,
             theta = theta, n_clusters = length(counts), loglik = ll, n = n,
             method = "CRP Gibbs sampler, Neal algorithm 8")
}

# SPDX-License-Identifier: AGPL-3.0-or-later

#' .crp_norm_logpdf
#'
#' A step of the crpcol implementation. Called by \code{.crp_collapsed_sweep}, \code{Crpcol}, \code{Crpgib} and 1 others in the module.
#' See the file header for the source the module follows.
#' it follows.
#'
#' @param x Numeric; combined arithmetically in the body.
#' @param mu Numeric; combined arithmetically in the body.
#' @param var Numeric; combined arithmetically in the body.
#' @return A numeric value.
#' @export
.crp_norm_logpdf <- function(x, mu, var) {
  -0.5 * (log(2 * pi * var) + (x - mu)^2 / var)
}

# Neal's algorithm 3 for a conjugate normal DP mixture.  theta is
# integrated out, so a customer joins table k with weight
# n_k f(y_i | y_k) and opens a new one with weight alpha f(y_i).  The
# sweep order and the uniform draws are fixed, so the two language arms
# visit identical states.
#' Neal\'s algorithm 3 for a conjugate normal DP mixture.  theta is
#'
#' integrated out, so a customer joins table k with weight n_k f(y_i |
#' y_k) and opens a new one with weight alpha f(y_i).  The sweep order
#' and the uniform draws are fixed, so the two language arms visit
#' identical states.
#'
#' @param y A vector; its length is taken and its elements indexed.
#' @param alpha Numeric; combined arithmetically in the body.
#' @param n_iter A count; the body uses it as \code{seq_len(...)}.
#' @param mu0 Numeric; combined arithmetically in the body.
#' @param tau2 Numeric; combined arithmetically in the body.
#' @param sigma2 Numeric; combined arithmetically in the body.
#' @param seed Passed to \code{.ghc_rng}.
#' @return A list with \code{z}, \code{counts}, \code{sums}.
#' @export
.crp_collapsed_sweep <- function(y, alpha, n_iter, mu0, tau2, sigma2, seed) {
  n <- length(y)
  z <- rep(1L, n)
  counts <- c(n)
  sums <- c(sum(y))
  e <- .ghc_rng(seed)
  for (it in seq_len(n_iter)) {
    for (i in seq_len(n)) {
      k <- z[i]
      counts[k] <- counts[k] - 1L
      sums[k] <- sums[k] - y[i]
      K <- length(counts)
      w <- numeric(K + 1L)
      for (c in seq_len(K)) {
        if (counts[c] == 0L) { w[c] <- 0; next }
        prec <- 1 / tau2 + counts[c] / sigma2
        m <- (mu0 / tau2 + sums[c] / sigma2) / prec
        w[c] <- counts[c] * exp(.crp_norm_logpdf(y[i], m, sigma2 + 1 / prec))
      }
      w[K + 1L] <- alpha * exp(.crp_norm_logpdf(y[i], mu0, sigma2 + tau2))
      tot <- 0
      for (v in w) tot <- tot + v
      u <- .ghc_unif(e, 1L) * tot
      acc <- 0
      pick <- K + 1L
      for (c in seq_len(K + 1L)) {
        acc <- acc + w[c]
        if (u <= acc) { pick <- c; break }
      }
      if (pick == K + 1L) { counts <- c(counts, 0L); sums <- c(sums, 0) }
      z[i] <- pick
      counts[pick] <- counts[pick] + 1L
      sums[pick] <- sums[pick] + y[i]
    }
    keep <- which(counts > 0L)
    remap <- integer(length(counts))
    remap[keep] <- seq_along(keep)
    counts <- counts[keep]
    sums <- sums[keep]
    z <- remap[z]
  }
  list(z = z, counts = counts, sums = sums)
}

#' Collapsed Gibbs sampler for a CRP mixture
#'
#' Formula: P(z_i | z_{-i}, y) with theta marginalized
#'
#' P(z_i = k | .) proportional to n_k^{-i} f(y_i | y_{-i,k}) and to
#' alpha f(y_i) for a new table, with the normal-normal predictive
#' density in closed form.  Marginalising theta is what collapses the
#' sampler: no cluster parameters are ever stored.
#'
#' @param y Observations.
#' @param alpha Concentration, strictly positive.
#' @param n_iter Number of full sweeps.
#' @param mu0,tau2 Base measure N(mu0, tau2) for the cluster means.
#' @param sigma2 Known within-cluster variance.
#' @param seed Seed of the deterministic stream.
#' @return List with \code{estimate} (number of clusters), \code{z},
#'   \code{counts}, \code{cluster_mean}, \code{n_clusters},
#'   \code{loglik}, \code{n}, \code{method}.
#' @references MacEachern (1994), Commun. Statist. B 23(3):727-741;
#'   Neal (2000), J. Comput. Graph. Statist. 9(2):249-265, algorithm 3.
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' Crpcol(V)
Crpcol <- function(y, alpha = 1, n_iter = 50, mu0 = 0, tau2 = 10,
                   sigma2 = 1, seed = 42) {
  y <- .s03vec(y)
  n <- length(y)
  if (n == 0L) stop("empty input: y has no observations")
  if (!(alpha > 0)) stop("alpha must be strictly positive")
  if (!(tau2 > 0 && sigma2 > 0))
    stop("tau2 and sigma2 must be strictly positive")
  n_iter <- as.integer(n_iter)
  if (n_iter < 1L) stop("n_iter must be at least 1")
  s <- .crp_collapsed_sweep(y, alpha, n_iter, mu0, tau2, sigma2, seed)
  K <- length(s$counts)
  means <- numeric(K)
  for (c in seq_len(K)) {
    prec <- 1 / tau2 + s$counts[c] / sigma2
    means[c] <- (mu0 / tau2 + s$sums[c] / sigma2) / prec
  }
  ll <- 0
  for (i in seq_len(n)) ll <- ll + .crp_norm_logpdf(y[i], means[s$z[i]], sigma2)
  .t1_result(estimate = K, z = s$z - 1L, counts = s$counts,
             cluster_mean = means, n_clusters = K, loglik = ll, n = n,
             method = "collapsed Gibbs sampler for a CRP mixture")
}

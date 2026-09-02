# SPDX-License-Identifier: AGPL-3.0-or-later

#' Dirichlet process mixture model
#'
#' Formula: G ~ DP(alpha, G_0); theta_i | G ~ G; y_i ~ f(theta_i)
#'
#' Fitted by the collapsed Gibbs sampler with a normal base measure and
#' a known within-cluster variance.  Because theta is integrated out,
#' the single-cluster case reduces exactly to the conjugate normal
#' posterior mean (mu0/tau2 + sum y/sigma2) / (1/tau2 + n/sigma2), the
#' degenerate check the fit has to reproduce.
#'
#' @param y Observations.
#' @param alpha Concentration, strictly positive.
#' @param base_distribution (mu0, tau2) of the normal base measure, or
#'   NULL for (0, 10).
#' @param n_iter Number of sweeps.
#' @param sigma2 Known within-cluster variance.
#' @param seed Seed of the deterministic stream.
#' @return List with \code{estimate} (posterior predictive mean),
#'   \code{n_clusters}, \code{z}, \code{counts}, \code{cluster_mean},
#'   \code{weights}, \code{w_new}, \code{loglik}, \code{n},
#'   \code{method}.
#' @references Antoniak (1974), Ann. Statist. 2(6):1152-1174;
#'   Escobar & West (1995), JASA 90(430):577-588; Neal (2000),
#'   J. Comput. Graph. Statist. 9(2):249-265.
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' Dpmem(V)
Dpmem <- function(y, alpha = 1, base_distribution = NULL, n_iter = 50,
                  sigma2 = 1, seed = 42) {
  y <- .s03vec(y)
  n <- length(y)
  if (n == 0L) stop("empty input: y has no observations")
  if (!(alpha > 0)) stop("alpha must be strictly positive")
  if (is.null(base_distribution)) {
    mu0 <- 0; tau2 <- 10
  } else {
    b <- .s03vec(base_distribution)
    if (length(b) != 2L) stop("base_distribution must be (mu0, tau2)")
    mu0 <- b[1]; tau2 <- b[2]
  }
  if (!(tau2 > 0 && sigma2 > 0))
    stop("tau2 and sigma2 must be strictly positive")
  s <- .crp_collapsed_sweep(y, alpha, as.integer(n_iter), mu0, tau2,
                            sigma2, seed)
  K <- length(s$counts)
  means <- numeric(K); w <- numeric(K)
  for (c in seq_len(K)) {
    prec <- 1 / tau2 + s$counts[c] / sigma2
    means[c] <- (mu0 / tau2 + s$sums[c] / sigma2) / prec
    w[c] <- s$counts[c] / (n + alpha)
  }
  w_new <- alpha / (n + alpha)
  pred <- 0
  for (c in seq_len(K)) pred <- pred + w[c] * means[c]
  pred <- pred + w_new * mu0
  ll <- 0
  for (i in seq_len(n)) ll <- ll + .crp_norm_logpdf(y[i], means[s$z[i]], sigma2)
  .t1_result(estimate = pred, n_clusters = K, z = s$z - 1L,
             counts = s$counts, cluster_mean = means, weights = w,
             w_new = w_new, loglik = ll, n = n,
             method = "Dirichlet process mixture, collapsed Gibbs")
}

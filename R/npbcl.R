# SPDX-License-Identifier: AGPL-3.0-or-later
#' Posterior-mode partition of a Dirichlet-process normal mixture
#'
#' Quintana's predictive view is that a DP mixture is a rule for
#' deciding, one observation at a time, whether the next value belongs
#' with something already seen or starts a new group. Reading the Polya
#' urn as an objective rather than as a sampler gives a partition
#' directly, without a Markov chain. Observation \code{i} joins the
#' cluster maximizing \code{log n_c + log N(y_i | mu_c_post, s2_c_pred)}
#' over existing clusters, against
#' \code{log alpha + log N(y_i | m0, tau0^2 + sigma^2)} for a new one,
#' where with base prior \code{N(m0, tau0^2)} and known within-cluster
#' variance \code{sigma^2} a cluster holding \code{n_c} values summing
#' to \code{S_c} has posterior precision \code{1/tau0^2 + n_c/sigma^2}
#' and the predictive adds \code{sigma^2}.
#'
#' Determinism: no sampling. The allocation depends on the order of
#' \code{y}, which is a property of the greedy rule; \code{log_score}
#' lets a caller compare orderings.
#'
#' @param y Observations.
#' @param alpha DP concentration, positive.
#' @param sigma Within-cluster standard deviation, positive.
#' @param m0 Base-measure mean; the sample mean if \code{NULL}.
#' @param tau0 Base-measure standard deviation, positive.
#' @return List with \code{labels}, \code{estimate}, \code{n_clusters},
#'   \code{sizes}, \code{means}, \code{log_score}, \code{alpha},
#'   \code{n}.
#' @references Quintana, F. A. (2006). A predictive view of Bayesian
#'   clustering. Journal of Statistical Planning and Inference, 136(8),
#'   2407-2429. doi:10.1016/j.jspi.2004.09.015
#' @export
Npbcl <- function(y, alpha = 1, sigma = 1, m0 = NULL, tau0 = 10) {
  v <- as.numeric(y)
  n <- length(v)
  if (n == 0L) stop("Npbcl: y is empty")
  a <- as.numeric(alpha)
  if (a <= 0) stop("Npbcl: alpha must be positive")
  s <- as.numeric(sigma)
  if (s <= 0) stop("Npbcl: sigma must be positive")
  t0 <- as.numeric(tau0)
  if (t0 <= 0) stop("Npbcl: tau0 must be positive")
  mu0 <- if (is.null(m0)) mean(v) else as.numeric(m0)
  s2 <- s * s
  p0 <- 1 / (t0 * t0)
  lnorm <- function(x, mu, vv) -0.5 * (log(2 * pi * vv) + (x - mu)^2 / vv)
  labels <- integer(n)
  counts <- numeric(0)
  sums <- numeric(0)
  total <- 0
  for (i in seq_len(n)) {
    best <- NA_real_; bestk <- -1L
    if (length(counts)) {
      for (cc in seq_along(counts)) {
        prec <- p0 + counts[cc] / s2
        mc <- (mu0 * p0 + sums[cc] / s2) / prec
        sc <- 1 / prec + s2
        sco <- log(counts[cc]) + lnorm(v[i], mc, sc)
        if (is.na(best) || sco > best) { best <- sco; bestk <- cc }
      }
    }
    newsco <- log(a) + lnorm(v[i], mu0, t0 * t0 + s2)
    if (is.na(best) || newsco > best) {
      counts <- c(counts, 1); sums <- c(sums, v[i])
      labels[i] <- length(counts) - 1L
      total <- total + newsco
    } else {
      labels[i] <- bestk - 1L
      counts[bestk] <- counts[bestk] + 1
      sums[bestk] <- sums[bestk] + v[i]
      total <- total + best
    }
  }
  prec <- p0 + counts / s2
  means <- (mu0 * p0 + sums / s2) / prec
  .t1_result(labels = labels, estimate = length(counts),
             n_clusters = length(counts), sizes = counts, means = means,
             log_score = total, alpha = a, n = n,
             method = "DP-mixture MAP partition (Quintana 2006)")
}

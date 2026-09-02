# SPDX-License-Identifier: AGPL-3.0-or-later
#' Three-level (nested) random-effects meta-analysis by maximum likelihood
#'
#' Effect i in cluster j has Var(y_j) = diag(v_ij + tau2_2) + tau2_3 * 1 1', so
#' the covariance is exchangeable within a cluster.  The likelihood uses the
#' Sherman-Morrison inverse and log|V_j| = sum log(v + tau2_2) +
#' log(1 + tau2_3 sum 1/(v + tau2_2)); mu is the GLS estimate at the current
#' components and the two components are maximised by nested golden-section
#' search with a fixed iteration count.  Source consulted: Cheung (2014),
#' Psychological Methods 19(2), 211-229, equations (5)-(8).  Verified against
#' metafor::rma.mv.
#'
#' @param yi,vi effect sizes and their sampling variances.
#' @param cluster level-3 grouping label per effect.
#' @param level confidence level.
#' @param upper optional upper bound of the variance-component search.
#' @return list: estimate, se, ci_lower, ci_upper, tau2_level2, tau2_level3,
#'   loglik, i2_level2, i2_level3, n_clusters, n, method.
#' @keywords internal
#' @examples
#' matrl(c(0.1, 0.3, -0.2, 0.45), c(0.02, 0.05, 0.03, 0.08), c(1, 1, 2, 2))$tau2_level3
#' @export
matrl <- function(yi, vi, cluster, level = 0.95, upper = NULL) {
  y <- as.numeric(yi)
  v <- as.numeric(vi)
  cl <- cluster
  n <- length(y)
  gls <- function(t2, t3) {
    keys <- unique(cl)
    logdet <- 0
    q1 <- 0
    cross <- 0
    qy <- 0
    for (key in keys) {
      idx <- which(cl == key)
      d <- v[idx] + t2
      yy <- y[idx]
      di <- 1 / d
      s1 <- sum(di)
      sy <- sum(di * yy)
      syy <- sum(di * yy * yy)
      denom <- 1 + t3 * s1
      logdet <- logdet + sum(log(d)) + log(denom)
      q1 <- q1 + s1 - t3 * s1 * s1 / denom
      cross <- cross + sy - t3 * s1 * sy / denom
      qy <- qy + syy - t3 * sy * sy / denom
    }
    mu <- cross / q1
    rss <- qy - 2 * mu * cross + mu * mu * q1
    list(mu = mu, var = 1 / q1, m2ll = logdet + rss + n * log(2 * pi))
  }
  hi <- if (is.null(upper)) 10 * (sum((y - mean(y))^2) / n) + 1e-8 else as.numeric(upper)
  inner <- function(t3) k02gold(function(t2) gls(t2, t3)$m2ll, 0, hi, 60L)
  outer <- function(t3) gls(inner(t3), t3)$m2ll
  t3 <- k02gold(outer, 0, hi, 60L)
  t2 <- inner(t3)
  f <- gls(t2, t3)
  se <- sqrt(f$var)
  crit <- k02z(0.5 + 0.5 * level)
  tot <- t2 + t3 + mean(v)
  list(estimate = f$mu, se = se, ci_lower = f$mu - crit * se,
       ci_upper = f$mu + crit * se, tau2_level2 = t2, tau2_level3 = t3,
       loglik = -0.5 * f$m2ll, i2_level2 = 100 * t2 / tot,
       i2_level3 = 100 * t3 / tot, n_clusters = length(unique(cl)), n = n,
       method = "Three-level random-effects meta-analysis, ML (Cheung 2014)")
}

# CANONICAL TEST
# r <- matrl(c(0.10,0.30,-0.20,0.45,0.05,0.22,0.31,-0.05),
#            c(0.02,0.05,0.03,0.08,0.01,0.04,0.02,0.06), c(1,1,2,2,3,3,4,4))
# stopifnot(r$n_clusters == 4L, r$tau2_level2 >= 0, r$tau2_level3 >= 0)

#' @rdname matrl
#' @keywords internal
#' @export
morie_matrl <- matrl

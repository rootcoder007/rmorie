# SPDX-License-Identifier: AGPL-3.0-or-later
#' Hierarchical pooling: no pooling, complete pooling, partial pooling
#'
#' Lindley and Smith (1972), "Bayes estimates for the linear model", Journal of
#' the Royal Statistical Society Series B 34(1), 1-18,
#' doi:10.1111/j.2517-6161.1972.tb00885.x (citation verified against Crossref;
#' the pages are 1-18), and Gelman et al., Bayesian Data Analysis, 3rd edition,
#' Chapter 5, which the module specification names.  BDA3 itself was not
#' retrievable here, so the estimator is written in the standard published form
#' of the exchangeable normal-normal hierarchy.
#'
#' For groups with n_g observations, within-group variance sigma^2 and
#' between-group variance tau^2, the group mean has sampling variance
#' sigma_g^2 = sigma^2 / n_g and the posterior mean of the group effect is the
#' precision-weighted compromise
#' lambda_g = tau^2 / (tau^2 + sigma_g^2) = 1 / (1 + sigma_g^2/tau^2) and
#' theta_g = lambda_g ybar_g + (1 - lambda_g) mu, with mu the
#' precision-weighted grand mean.
#'
#' The two limits are the whole point and are checked as anchors: tau^2 to
#' infinity gives lambda_g = 1 and theta_g = ybar_g, no pooling; tau^2 to zero
#' gives lambda_g = 0 and theta_g = mu for every group, complete pooling.
#' Partial pooling is everything between, and the shrinkage is not uniform: a
#' group with few observations has a large sigma_g^2 and is pulled hard toward
#' mu while a large group barely moves.  Applying one shrinkage factor to every
#' group is wrong whenever the design is unbalanced, and the unbalanced fixture
#' is in the tests for that reason.
#'
#' tau^2 is estimated by the one-way method of moments,
#' max(0, (MS_between - MS_within) / n_tilde).  The truncation at zero is real:
#' a negative variance estimate means the data show less between-group spread
#' than sampling alone would produce, and the honest answer there is complete
#' pooling, not a negative variance.
#'
#' @param y the observations.
#' @param group group label per observation.
#' @param sigma2 optional within-group variance; the pooled within-group mean
#'   square by default.
#' @param tau2 optional between-group variance; the one-way method-of-moments
#'   estimate by default.  Pass a value to force a pooling regime.
#' @return list: theta, estimate, lambda_g, theta_nopool, theta_pool, mu,
#'   sigma2, tau2, n_g, grand_mean, G, n, method.
#' @keywords internal
#' @examples
#' Bayhier(c(1, 2, 5, 6, 9, 10), c("a", "a", "b", "b", "c", "c"))$theta
#' @export
Bayhier <- function(y, group, sigma2 = NULL, tau2 = NULL) {
  yy <- .s03vec(y)
  n <- length(yy)
  if (n == 0L) stop("hierarchical_pooling: y is empty")
  gl <- as.character(group)
  if (length(gl) != n) stop("hierarchical_pooling: y and group have different lengths")
  labs <- sort(unique(gl))
  G <- length(labs)
  if (G < 2L) stop("hierarchical_pooling: need at least two groups")
  ng <- integer(G)
  sg <- numeric(G)
  for (i in seq_len(n)) {
    j <- match(gl[i], labs)
    ng[j] <- ng[j] + 1L
    sg[j] <- sg[j] + yy[i]
  }
  if (any(ng == 0L)) stop("hierarchical_pooling: a group has no observations")
  ybar <- sg / ng
  ssw <- 0
  for (i in seq_len(n)) {
    d <- yy[i] - ybar[match(gl[i], labs)]
    ssw <- ssw + d * d
  }
  if (is.null(sigma2)) {
    if (n - G <= 0L) stop("hierarchical_pooling: no residual degrees of freedom for sigma2")
    s2 <- ssw / (n - G)
  } else {
    s2 <- as.numeric(sigma2)
    if (s2 < 0) stop("hierarchical_pooling: sigma2 must be non-negative")
  }
  grand <- 0
  for (v in yy) grand <- grand + v
  grand <- grand / n
  if (is.null(tau2)) {
    ssb <- 0
    for (j in seq_len(G)) {
      d <- ybar[j] - grand
      ssb <- ssb + ng[j] * d * d
    }
    msb <- ssb / (G - 1)
    sq <- 0
    for (j in seq_len(G)) sq <- sq + ng[j] * ng[j]
    ntil <- (n - sq / n) / (G - 1)
    t2 <- if (ntil > 0) (msb - s2) / ntil else 0
    if (t2 < 0) t2 <- 0
  } else {
    t2 <- as.numeric(tau2)
    if (t2 < 0) stop("hierarchical_pooling: tau2 must be non-negative")
  }
  lam <- numeric(G)
  for (j in seq_len(G)) {
    sgj <- s2 / ng[j]
    lam[j] <- if ((t2 + sgj) > 0) t2 / (t2 + sgj) else 0
  }
  num <- 0
  den <- 0
  for (j in seq_len(G)) {
    dd <- t2 + s2 / ng[j]
    w <- if (dd > 0) 1 / dd else 0
    num <- num + w * ybar[j]
    den <- den + w
  }
  mu <- if (den > 0) num / den else grand
  theta <- lam * ybar + (1 - lam) * mu
  list(theta = theta, estimate = theta[1], lambda_g = lam, theta_nopool = ybar,
       theta_pool = mu, mu = mu, sigma2 = s2, tau2 = t2, n_g = ng,
       grand_mean = grand, G = G, n = n,
       method = "normal-normal partial pooling, lambda_g = tau2/(tau2 + sigma2/n_g); Lindley and Smith (1972)")
}

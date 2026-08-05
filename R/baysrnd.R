# SPDX-License-Identifier: AGPL-3.0-or-later
#' Random-effects shrinkage -- the same method as \code{Bayhier}
#'
#' The specification here is "random intercepts u_g ~ N(0, tau^2)", citing
#' Lindley and Smith (1972).  That is the exchangeable normal-normal hierarchy,
#' and its posterior mean is exactly the partial-pooling estimator Bayhier
#' computes: the random intercept is u_g = theta_g - mu = lambda_g
#' (ybar_g - mu) with lambda_g = tau^2/(tau^2 + sigma^2/n_g).  "Random-effects
#' shrinkage" and "hierarchical partial pooling" are two names for one
#' estimator.
#'
#' There is therefore exactly one implementation.  This function calls Bayhier
#' and reports the random effects u_g themselves rather than the pooled group
#' means; writing the arithmetic a second time would agree with the first at
#' 1e-9 forever and be indistinguishable from correct work while doubling the
#' surface under a second name.  Recorded in ledger/wave2/DUPMAP.tsv as
#' baysrnd -> bayhier.
#'
#' @param y the observations.
#' @param X accepted for interface compatibility; taken as the grouping vector
#'   when group is omitted, matching the original argument order.
#' @param group group label per observation.
#' @param sigma2,tau2 optional variance components; estimated when omitted.
#' @return list: u_g, estimate, theta, lambda_g, theta_nopool, mu, sigma2,
#'   tau2, n_g, G, n, method.
#' @keywords internal
#' @examples
#' Baysrnd(c(1, 2, 5, 6, 9, 10), c("a", "a", "b", "b", "c", "c"))$u_g
#' @export
Baysrnd <- function(y, X = NULL, group = NULL, sigma2 = NULL, tau2 = NULL) {
  g <- if (!is.null(group)) group else X
  if (is.null(g)) stop("shrinkage_random: a grouping vector is required")
  r <- Bayhier(y, g, sigma2 = sigma2, tau2 = tau2)
  u <- r$theta - r$mu
  list(u_g = u, estimate = u[1], theta = r$theta, lambda_g = r$lambda_g,
       theta_nopool = r$theta_nopool, mu = r$mu, sigma2 = r$sigma2, tau2 = r$tau2,
       n_g = r$n_g, G = r$G, n = r$n,
       method = "u_g = lambda_g (ybar_g - mu); shared implementation with morie.fn.bayhier")
}

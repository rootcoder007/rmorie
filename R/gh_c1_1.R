# SPDX-License-Identifier: AGPL-3.0-or-later
#' Bayes's rule with a dominated likelihood
#'
#' pi(theta | X) is proportional to p_theta(X) pi(theta).  In infinite
#' dimensions this is not automatic: it needs the family to be dominated
#' by a common sigma-finite measure, which is what section 1.3 establishes
#' before anything else in the book can proceed.  On a finite grid the
#' rule is a plain normalisation, computed in logs.
#'
#' Formula: pi_i proportional to exp(log p(X | theta_i) + log pi(theta_i)).
#'
#' @param x Grid of parameter values.
#' @param log_lik Log-likelihood as a function of theta; a single N(theta, 1)
#'   observation at 1 when NULL.
#' @param log_prior Log-prior as a function of theta; standard normal when
#'   NULL.
#' @return List with \code{estimate} (posterior mean), \code{posterior},
#'   \code{grid}, \code{method}.
#' @references Ghosal & van der Vaart (2017), Fundamentals of
#'   Nonparametric Bayesian Inference, CUP, section 1.3.
#' @export
Ghosalbayesruleinfinite <- function(x, log_lik = NULL, log_prior = NULL) {
  th <- as.numeric(x)
  if (length(th) == 0L) stop("x must be non-empty")
  if (is.null(log_lik)) log_lik <- function(t) -0.5 * (1 - t)^2
  if (is.null(log_prior)) log_prior <- function(t) -0.5 * t * t
  lw <- vapply(th, log_lik, numeric(1)) + vapply(th, log_prior, numeric(1))
  w <- exp(lw - max(lw))
  post <- w / sum(w)
  .t1_result(estimate = sum(th * post), posterior = post, grid = th,
             method = "Bayes rule on a grid (GvdV 2017 sec. 1.3)")
}

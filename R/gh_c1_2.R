# SPDX-License-Identifier: AGPL-3.0-or-later
#' Posterior via Radon-Nikodym under domination
#'
#' When every P_theta has a density with respect to a common mu the
#' posterior exists and is the normalised density-weighted prior.  The
#' normalising constant -- the marginal likelihood -- is returned
#' alongside because a zero marginal is precisely the signal that the
#' domination assumption has failed, which is the failure mode section
#' 1.3.1 is guarding against.
#'
#' Formula: log m = log(mean_i exp(l_i)) with
#'   l_i = log p(X | theta_i) + log pi(theta_i).
#'
#' @param x Grid of parameter values.
#' @param log_lik Log-likelihood as a function of theta; a single
#'   N(theta, 1) observation at 1 when NULL.
#' @param log_prior Log-prior as a function of theta; standard normal
#'   when NULL.
#' @return List with \code{estimate}, \code{log_marginal},
#'   \code{posterior}, \code{method}.
#' @references Ghosal & van der Vaart (2017), Fundamentals of
#'   Nonparametric Bayesian Inference, CUP, section 1.3.1.
#' @export
Ghosalabsolutecontinuity <- function(x, log_lik = NULL, log_prior = NULL) {
  th <- as.numeric(x)
  if (length(th) == 0L) stop("x must be non-empty")
  if (is.null(log_lik)) log_lik <- function(t) -0.5 * (1 - t)^2
  if (is.null(log_prior)) log_prior <- function(t) -0.5 * t * t
  lw <- vapply(th, log_lik, numeric(1)) + vapply(th, log_prior, numeric(1))
  mx <- max(lw)
  w <- exp(lw - mx)
  tot <- sum(w)
  post <- w / tot
  .t1_result(estimate = sum(th * post),
             log_marginal = log(tot / length(th)) + mx,
             posterior = post,
             method = "dominated posterior, Radon-Nikodym (GvdV 2017 sec. 1.3.1)")
}

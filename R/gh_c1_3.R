# SPDX-License-Identifier: AGPL-3.0-or-later
#' Sequential prior-to-posterior updating
#'
#' dPi_n/dPi(theta) = p_theta(X^n) / int p_eta(X^n) dPi(eta).  Batch
#' updating and one-observation-at-a-time updating must give the same
#' posterior -- coherence is what makes the Bayesian recipe well defined
#' at all -- and the two are computed separately here so the agreement is
#' measured rather than assumed.
#'
#' Formula: batch log weight = sum_j log p(d_j | theta) + log pi(theta);
#'   sequential accumulates the same terms one at a time.
#'
#' @param x Grid of parameter values.
#' @param data Observations; c(0.8, 1.2, 1) when NULL.
#' @param log_lik_one Log-likelihood of one observation, f(theta, d).
#' @param log_prior Log-prior as a function of theta.
#' @return List with \code{estimate}, \code{posterior},
#'   \code{sequential_batch_gap}, \code{method}.
#' @references Ghosal & van der Vaart (2017), Fundamentals of
#'   Nonparametric Bayesian Inference, CUP, section 1.3.
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' Ghosalpriorposteriorupdate(V)
Ghosalpriorposteriorupdate <- function(x, data = NULL, log_lik_one = NULL,
                                       log_prior = NULL) {
  th <- as.numeric(x)
  if (length(th) == 0L) stop("x must be non-empty")
  if (is.null(data)) data <- c(0.8, 1.2, 1)
  if (length(data) == 0L) stop("data must be non-empty")
  if (is.null(log_lik_one)) log_lik_one <- function(t, d) -0.5 * (d - t)^2
  if (is.null(log_prior)) log_prior <- function(t) -0.5 * t * t
  lw <- vapply(th, function(t)
    sum(vapply(data, function(d) log_lik_one(t, d), numeric(1))), numeric(1)) +
    vapply(th, log_prior, numeric(1))
  w <- exp(lw - max(lw))
  post <- w / sum(w)
  logp <- vapply(th, log_prior, numeric(1))
  for (d in data)
    logp <- logp + vapply(th, function(t) log_lik_one(t, d), numeric(1))
  w2 <- exp(logp - max(logp))
  seqp <- w2 / sum(w2)
  .t1_result(estimate = sum(th * post), posterior = post,
             sequential_batch_gap = max(abs(post - seqp)),
             method = "prior-to-posterior updating (GvdV 2017 sec. 1.3)")
}

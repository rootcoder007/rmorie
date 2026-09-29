# SPDX-License-Identifier: AGPL-3.0-or-later
#' Differentially private release of a posterior sample
#'
#' Wang, Fienberg and Smola (2015), Privacy for free: posterior sampling
#' and stochastic gradient Monte Carlo, ICML 37, 2493-2502, and
#' Dimitrakakis, Nelson, Mitrokotsa and Rubinstein (2014), Robust and
#' private Bayesian inference, ALT 291-305: releasing a SINGLE posterior
#' draw is already differentially private when the log-likelihood is
#' bounded -- if sup |log p(x|theta) - log p(x'|theta)| <= B then one draw
#' is 2B-DP, and tempering the likelihood by 1/(2B/epsilon) buys any
#' target epsilon.  Neither was retrievable here as a full text; the bound
#' and the tempering are quoted in their standard published form.  The
#' privacy is not bought with added noise: it comes from the posterior's
#' own randomness.  The Laplace mechanism (Dwork et al. 2006) scale is
#' returned for comparison.
#'
#' The function releases ONE draw, chosen on the Philox stream, from the
#' supplied sample of the tempered posterior (it used to release the
#' posterior mean, which carries no guarantee). Identical to the Python arm
#' \code{morie.fn.bayesm.dp_bayesian_mechanism}.
#'
#' @param y the data (only its length is used, for the Laplace comparison).
#' @param posterior_sample draws from the tempered posterior (required).
#' @param epsilon the privacy budget.
#' @param B the log-likelihood-ratio bound.
#' @param sensitivity L1 sensitivity for the Laplace comparison.
#' @param seed Philox seed choosing the released draw.
#' @return list: estimate, released, draw_index (zero-based), posterior_mean,
#'   posterior_sd, temperature, eps_free, laplace_scale, n, method.
#' @keywords internal
#' @examples
#' Dpbayes(c(1, 2, 3), c(1.9, 2.0, 2.1), 1, 1)$temperature
#' @export
Dpbayes <- function(y, posterior_sample = NULL, epsilon = 1, B = 1,
                    sensitivity = NULL, seed = 0) {
  n <- length(y)
  if (is.null(posterior_sample)) {
    stop("posterior_sample is required: the mechanism releases one draw from the tempered posterior")
  }
  post <- as.numeric(posterior_sample)
  S <- length(post)
  if (S == 0) stop("posterior_sample is empty")
  temp <- if (epsilon > 0) 2 * B / epsilon else Inf
  j <- min(floor(.morie_random_uniform(1, seed = seed, stream = 0) * S), S - 1)
  sens <- if (is.null(sensitivity)) (if (n) 1 / n else NaN) else sensitivity
  list(estimate = post[j + 1], released = post[j + 1], draw_index = j,
       posterior_mean = sum(post) / S, posterior_sd = if (S > 1) stats::sd(post) else 0,
       temperature = temp, eps_free = 2 * B,
       laplace_scale = if (epsilon > 0) sens / epsilon else Inf, n = n,
       method = "One draw from the posterior tempered by 2B/epsilon is epsilon-DP")
}

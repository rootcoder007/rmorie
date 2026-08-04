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
#' @param y the data.
#' @param posterior_sample posterior draws of the quantity to release.
#' @param epsilon the privacy budget.
#' @param B the log-likelihood-ratio bound.
#' @param sensitivity L1 sensitivity for the Laplace comparison.
#' @return list: estimate, released, posterior_sd, tempered_sd, temperature,
#'   eps_free, laplace_scale, n, method.
#' @keywords internal
#' @examples
#' Dpbayes(c(1, 2, 3), c(1.9, 2.0, 2.1), 1, 1)$temperature
#' @export
Dpbayes <- function(y, posterior_sample = NULL, epsilon = 1, B = 1,
                    sensitivity = NULL) {
  v <- .s03vec(y); n <- length(v)
  e <- as.numeric(epsilon); b <- as.numeric(B)
  temp <- if (e > 0) (2 * b) / e else Inf
  post <- if (!is.null(posterior_sample)) .s03vec(posterior_sample) else v
  m <- .s03mean(post)
  sd_ <- if (length(post) > 1L) .s03sd(post, 1L) else 0
  sens <- if (!is.null(sensitivity)) as.numeric(sensitivity) else if (n) 1 / n else NaN
  list(estimate = m, released = m, posterior_sd = sd_,
       tempered_sd = if (!is.nan(sd_)) sd_ * sqrt(temp) else NaN,
       temperature = temp, eps_free = 2 * b,
       laplace_scale = if (e > 0) sens / e else Inf, n = n,
       method = "One posterior draw is 2B-DP; tempering by 2B/epsilon reaches any epsilon (Dimitrakakis et al. 2014; Wang et al. 2015)")
}

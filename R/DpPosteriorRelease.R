#' Differentially private release of one posterior draw
#'
#' If the log-likelihood ratio is bounded by \eqn{B}, one draw from the
#' posterior is \eqn{2B}-differentially private, and tempering the likelihood
#' by \eqn{\epsilon / (2B)} reaches any budget \eqn{\epsilon}. The function
#' reports the temperature \eqn{2B/\epsilon} and releases one draw chosen
#' uniformly (Philox stream) from the supplied sample of the tempered
#' posterior. Identical to the Python arm
#' \code{morie.fn.bayesm.dp_bayesian_mechanism}.
#'
#' @param y The data (only its length is used, for the Laplace comparison).
#' @param posterior_sample Draws from the tempered posterior.
#' @param epsilon Privacy budget.
#' @param B Bound on the log-likelihood ratio.
#' @param sensitivity L1 sensitivity for the Laplace comparison; default 1/n.
#' @param seed Philox seed choosing the released draw.
#' @return A list with \code{estimate} and \code{released} (the draw),
#'   \code{draw_index} (zero-based), \code{posterior_mean},
#'   \code{posterior_sd}, \code{temperature}, \code{eps_free},
#'   \code{laplace_scale}, \code{n} and \code{method}.
#' @references Dimitrakakis, C., Nelson, B., Mitrokotsa, A. and Rubinstein, B.
#'   (2014). Robust and private Bayesian inference. Algorithmic Learning
#'   Theory, 291-305.
#'
#'   Wang, Y.-X., Fienberg, S. E. and Smola, A. (2015). Privacy for free:
#'   posterior sampling and stochastic gradient Monte Carlo. ICML 37,
#'   2493-2502.
#' @examples
#' dp_bayesian_mechanism(c(0.2, 0.4), c(1, 2, 3, 4), epsilon = 0.5, seed = 3)$temperature
#' @export
dp_bayesian_mechanism <- function(y, posterior_sample = NULL, epsilon = 1, B = 1,
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

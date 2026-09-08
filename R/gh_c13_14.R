# SPDX-License-Identifier: AGPL-3.0-or-later
#' Cox posterior via the partial likelihood
#'
#' pi(beta | data) is proportional to pi(beta) times the partial
#' likelihood.  Using the partial likelihood as if it were a likelihood
#' is not an approximation to be apologised for: section 13.6.1 shows the
#' resulting posterior has the right asymptotics, because the partial
#' likelihood already carries all the information about beta that the
#' unknown baseline leaves available.
#'
#' Formula: log pi(b | data) = -b^2/(2 prior_sd^2)
#'   + sum_i \[b z_i - log sum_\{j in R_i\} exp(b z_j)\], on a grid.
#'
#' @param beta0 True log hazard ratio.
#' @param n Sample size.
#' @param prior_sd Normal prior standard deviation, positive.
#' @param seed Seed for the deterministic draws.
#' @return List with \code{estimate} (posterior mean), \code{error},
#'   \code{method}.
#' @references Ghosal & van der Vaart (2017), Fundamentals of
#'   Nonparametric Bayesian Inference, CUP, section 13.6.1.
#' @export
#' @examples
#' Ghosalcoxpost()
Ghosalcoxpost <- function(beta0 = 0.6, n = 400, prior_sd = 2, seed = 42) {
  n <- as.integer(n)
  if (n < 1L) stop("n must be positive")
  if (prior_sd <= 0) stop("prior_sd must be positive")
  e <- .ghc_rng(seed)
  zs <- as.numeric((seq_len(n) - 1L) %% 2L == 0L)
  times <- -log(pmax(.ghc_unif(e, n), 1e-12)) / exp(beta0 * zs)
  ord <- order(times)
  log_pl <- function(b) {
    ez <- exp(b * zs)
    risk <- sum(ez) - c(0, cumsum(ez[ord])[-n])
    -0.5 * (b / prior_sd)^2 + sum(b * zs[ord] - log(pmax(risk, 1e-300)))
  }
  grid <- beta0 - 1.5 + 3 * (seq_len(61) - 1) / 60
  ws <- vapply(grid, log_pl, numeric(1))
  ws <- exp(ws - max(ws))
  post_mean <- sum(grid * ws) / sum(ws)
  .t1_result(estimate = post_mean, error = abs(post_mean - beta0),
             method = "Cox posterior (GvdV 2017 sec. 13.6.1)")
}

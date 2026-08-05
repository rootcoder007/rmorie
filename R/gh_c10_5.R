# SPDX-License-Identifier: AGPL-3.0-or-later
#' Spike-and-slab white-noise adaptation
#'
#' theta_jk ~ pi N(0, tau^2) + (1 - pi) delta_0.  Because the slab is
#' conjugate the posterior inclusion probability of each coordinate is
#' available exactly, and it recovers a sparse truth: coordinates whose
#' observed value is large relative to the noise level 1/sqrt(n) get
#' inclusion probabilities near one, the rest near zero.
#'
#' Formula: p_k = 1 / (1 + exp(l_0 - l_1)) with
#'   l_1 = log(pi) + log phi(y_k; 0, 1/n + tau2),
#'   l_0 = log(1 - pi) + log phi(y_k; 0, 1/n).
#'
#' @param y Numeric vector of observed coordinates; simulated when NULL.
#' @param n Precision (sample size) of each coordinate.
#' @param pi_incl Prior inclusion probability, in (0, 1).
#' @param tau2 Slab variance.
#' @param seed Seed for the deterministic draw of \code{y}.
#' @return List with \code{estimate} (first inclusion probability),
#'   \code{inclusion_probs}, \code{method}.
#' @references Ghosal & van der Vaart (2017), Fundamentals of
#'   Nonparametric Bayesian Inference, CUP, section 10.3.2.
#' @export
Ghosalwnadapt <- function(y = NULL, n = 400, pi_incl = 0.2, tau2 = 1,
                          seed = 42) {
  if (pi_incl <= 0 || pi_incl >= 1)
    stop("pi_incl must lie strictly between 0 and 1")
  if (tau2 <= 0) stop("tau2 must be positive")
  if (n <= 0) stop("n must be positive")
  e <- .ghc_rng(seed)
  if (is.null(y)) {
    truth <- c(1.2, -0.9, 0, 0, 0, 0, 0, 0)
    y <- truth + .ghc_norm(e, length(truth)) / sqrt(n)
  } else {
    y <- as.numeric(y)
    if (length(y) == 0L) stop("y must be non-empty")
  }
  v <- 1 / n
  l1 <- -0.5 * log(2 * pi * (v + tau2)) - 0.5 * y * y / (v + tau2) +
    log(pi_incl)
  l0 <- -0.5 * log(2 * pi * v) - 0.5 * y * y / v + log(1 - pi_incl)
  incl <- 1 / (1 + exp(l0 - l1))
  .t1_result(estimate = incl[1],
             inclusion_probs = incl,
             method = "spike-slab adaptation (GvdV 2017 sec. 10.3.2)")
}

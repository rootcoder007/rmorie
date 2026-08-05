# SPDX-License-Identifier: AGPL-3.0-or-later
#' White-noise full Bernstein-von Mises
#'
#' In the white-noise model dY = theta dt + dW/sqrt(n) with a nearly flat
#' Gaussian prior the posterior is N(Y, I/n) EXACTLY, not asymptotically.
#' The white-noise model is the one place where the BvM statement can be
#' checked as an identity rather than a limit, which is why the chapter
#' uses it to isolate what does and does not survive in infinite
#' dimensions.
#'
#' Formula: posterior mean = Y prior_var / (prior_var + 1/n);
#'   posterior variance = 1 / (1/prior_var + n).
#'
#' @param y Observed coordinates.
#' @param n Precision (sample size).
#' @param prior_var Prior variance per coordinate; large means flat.
#' @return List with \code{estimate} (max deviation of the posterior
#'   mean from Y), \code{mean_matches_Y}, \code{var_matches_In},
#'   \code{method}.
#' @references Ghosal & van der Vaart (2017), Fundamentals of
#'   Nonparametric Bayesian Inference, CUP, section 12.4.1.
#' @export
Ghosalwnfullbvm <- function(y = c(0.5, -0.2, 0.1), n = 400,
                            prior_var = 1e6) {
  ys <- as.numeric(y)
  n <- as.numeric(n)
  if (length(ys) == 0L) stop("y must be non-empty")
  if (n <= 0) stop("n must be positive")
  if (prior_var <= 0) stop("prior_var must be positive")
  means <- prior_var / (prior_var + 1 / n) * ys
  vars_ <- rep(1 / (1 / prior_var + n), length(ys))
  gap <- max(abs(means - ys))
  var_gap <- max(abs(vars_ - 1 / n))
  .t1_result(estimate = gap, mean_matches_Y = gap < 1e-6,
             var_matches_In = var_gap < 1e-8,
             method = "white-noise full BvM (GvdV 2017 sec. 12.4.1)")
}

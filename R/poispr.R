# SPDX-License-Identifier: AGPL-3.0-or-later
#' Conjugate Poisson analysis with its negative-binomial predictive.
#'
#' The predictive is NEGATIVE BINOMIAL, not Poisson, so its variance
#' strictly exceeds its mean by the factor \code{overdispersion}.
#'
#' Formula: theta | y ~ Gamma(alpha + sum y_i, beta + sum e_i);
#'   ytilde mean alpha'/beta', variance (alpha'/beta')(1 + 1/beta')
#'
#' @param y Observed counts, non-negative.
#' @param alpha Prior shape, > 0.
#' @param beta Prior rate, > 0.
#' @param exposure Optional exposure per observation (default all 1).
#' @return List with \code{alpha_post}, \code{beta_post},
#'   \code{rate_mean}, \code{rate_var}, \code{pred_mean},
#'   \code{pred_var}, \code{overdispersion}, \code{n}.
#' @references Gelman, Carlin, Stern, Dunson, Vehtari & Rubin (2013),
#'   Bayesian Data Analysis, 3rd edition, Sections 2.6 and 2.7. Fetched as
#'   the full text of the book from the author's own copy.
#' @export
Poispred <- function(y, alpha, beta, exposure = NULL) {
  y <- .t1_vec(y); n <- length(y)
  if (n < 1L) stop("at least one observation is required")
  if (any(y < 0)) stop("counts must be non-negative")
  a <- as.numeric(alpha); b <- as.numeric(beta)
  if (a <= 0 || b <= 0)
    stop("the Gamma prior needs alpha > 0 and beta > 0")
  e <- if (is.null(exposure)) rep(1, n) else .t1_vec(exposure)
  if (length(e) != n) stop("exposure must have one entry per observation")
  if (any(e <= 0)) stop("exposures must be positive")
  ap <- a + sum(y); bp <- b + sum(e)
  pm <- ap / bp
  pv <- pm * (1 + 1 / bp)
  .t1_result(alpha_post = ap, beta_post = bp, rate_mean = pm,
             rate_var = ap / bp^2, pred_mean = pm, pred_var = pv,
             overdispersion = pv / pm, n = as.numeric(n),
             method = "Gamma-Poisson posterior and negative-binomial predictive")
}

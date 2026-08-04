# SPDX-License-Identifier: AGPL-3.0-or-later
#' Hierarchical normal model: shrinkage of group means towards mu.
#'
#' tau is taken as GIVEN; the log marginal posterior of tau, up to an
#' additive constant and under a flat prior, is returned so a caller can
#' profile or grid over it.
#'
#' Formula: mu_hat and V_mu from (5.20); theta_hat_j and V_j from (5.17);
#'   log p(tau|y) from (5.21) with a flat prior
#'
#' @param y Group means.
#' @param sigma Known within-group standard errors, strictly positive.
#' @param tau Between-group standard deviation, tau >= 0.
#' @return List with \code{mu_hat}, \code{V_mu}, \code{theta_hat},
#'   \code{V_theta}, \code{shrinkage}, \code{log_post_tau}, \code{tau},
#'   \code{J}.
#' @references Gelman, Carlin, Stern, Dunson, Vehtari & Rubin (2013),
#'   Bayesian Data Analysis, 3rd edition, Section 5.4, equations (5.17),
#'   (5.20) and (5.21). Fetched as the full text of the book from the
#'   author's own copy. A flat prior on tau is used and the additive
#'   constant dropped, so the value is comparable across tau but is not an
#'   absolute density.
#' @export
Hiermodel <- function(y, sigma, tau) {
  y <- .t1_vec(y); s <- .t1_vec(sigma); J <- length(y)
  if (length(s) != J) stop("y and sigma must have the same length")
  if (J < 2L) stop("a hierarchical model needs at least two groups")
  if (any(s <= 0))
    stop("the within-group standard errors must be positive")
  tau <- as.numeric(tau)
  if (tau < 0) stop("tau must be non-negative")
  w <- 1 / (s^2 + tau^2)
  Vmu <- 1 / sum(w)
  mu <- sum(w * y) * Vmu
  if (tau == 0) {
    th <- rep(mu, J); Vt <- rep(0, J); shr <- rep(1, J)
  } else {
    prec <- 1 / s^2 + 1 / tau^2
    Vt <- 1 / prec
    th <- (y / s^2 + mu / tau^2) / prec
    shr <- 1 - (1 / s^2) / prec
  }
  lp <- 0.5 * log(Vmu) - 0.5 * sum(log(s^2 + tau^2) + (y - mu)^2 * w)
  .t1_result(mu_hat = mu, V_mu = Vmu, theta_hat = th, V_theta = Vt,
             shrinkage = shr, log_post_tau = lp, tau = tau,
             J = as.numeric(J),
             method = "Hierarchical normal model, BDA3 (5.17)/(5.20)/(5.21)")
}

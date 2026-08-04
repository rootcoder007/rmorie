# SPDX-License-Identifier: AGPL-3.0-or-later
#' Conjugate normal-normal random-effects posterior (empirical Bayes)
#'
#' With y_i ~ N(theta_i, v_i), theta_i ~ N(mu, tau^2) and a flat prior on mu,
#' mu | y ~ N(sum w* y / sum w*, 1/sum w*) with w* = 1/(v + tau^2), and each
#' theta_i | y is the shrinkage estimate (y_i/v_i + mu/tau^2)/(1/v_i + 1/tau^2)
#' with variance 1/(1/v_i + 1/tau^2).  tau^2 is plugged in at DerSimonian-Laird.
#' Source consulted: Higgins, Thompson and Spiegelhalter (2009), JRSS A 172(1),
#' 137-159, section 2.
#'
#' @param yi,vi study effects and their within-study variances.
#' @param tau2 optional between-study variance; DerSimonian-Laird if absent.
#' @param level credible-interval level.
#' @return list: estimate, se, ci_lower, ci_upper, tau2, theta_mean, theta_sd,
#'   shrinkage, n, method.
#' @keywords internal
#' @examples
#' mabay(c(0.1, 0.3, -0.2, 0.45), c(0.02, 0.05, 0.03, 0.08))$estimate
#' @export
mabay <- function(yi, vi, tau2 = NULL, level = 0.95) {
  y <- as.numeric(yi); v <- as.numeric(vi)
  t2 <- if (is.null(tau2)) k02dl(y, v)$tau2 else as.numeric(tau2)
  ws <- 1 / (v + t2); sws <- sum(ws)
  mu <- sum(ws * y) / sws
  se <- sqrt(1 / sws)
  crit <- k02z(0.5 + 0.5 * level)
  if (t2 > 0) {
    prec <- 1 / v + 1 / t2
    th <- (y / v + mu / t2) / prec
    sd <- sqrt(1 / prec)
  } else {
    th <- rep(mu, length(y)); sd <- rep(0, length(y))
  }
  list(estimate = mu, se = se, ci_lower = mu - crit * se,
       ci_upper = mu + crit * se, tau2 = t2, theta_mean = th, theta_sd = sd,
       shrinkage = 1 - sd^2 / v, n = length(y),
       method = "Conjugate normal-normal random-effects posterior (Higgins, Thompson & Spiegelhalter 2009, sec. 2)")
}

# CANONICAL TEST
# r <- mabay(c(0.10,0.30,-0.20,0.45,0.05,0.22), c(0.02,0.05,0.03,0.08,0.01,0.04))
# stopifnot(abs(r$estimate - 0.0920094772579361) < 1e-13)

#' @rdname mabay
#' @keywords internal
#' @export
morie_mabay <- mabay

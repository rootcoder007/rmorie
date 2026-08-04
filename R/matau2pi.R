# SPDX-License-Identifier: AGPL-3.0-or-later
#' Prediction interval for a new study from tau^2
#'
#' mu +/- t_{k-2} sqrt(tau^2 + SE(mu)^2).  The k - 2 degrees of freedom are the
#' paper's own recommendation; metafor::predict uses k - 1 with the
#' Knapp-Hartung standard error, returned here as pi_lower_km1/pi_upper_km1.
#' Source consulted: Higgins, Thompson and Spiegelhalter (2009), JRSS A 172(1),
#' 137-159, section 3.1.
#'
#' @param yi,vi study effects and their within-study variances.
#' @param level interval level.
#' @return list: estimate, se, tau2, spread, pi_lower, pi_upper,
#'   pi_lower_km1, pi_upper_km1, df, n, method.
#' @keywords internal
#' @examples
#' matau2pi(c(0.1, 0.3, -0.2, 0.45), c(0.02, 0.05, 0.03, 0.08))$pi_lower
#' @export
matau2pi <- function(yi, vi, level = 0.95) {
  y <- as.numeric(yi); v <- as.numeric(vi); k <- length(y)
  d <- k02dl(y, v)
  se <- sqrt(d$var)
  spread <- sqrt(d$tau2 + d$var)
  c2 <- k02tq(0.5 + 0.5 * level, k - 2)
  c1 <- k02tq(0.5 + 0.5 * level, k - 1)
  list(estimate = d$mu, se = se, tau2 = d$tau2, spread = spread,
       pi_lower = d$mu - c2 * spread, pi_upper = d$mu + c2 * spread,
       pi_lower_km1 = d$mu - c1 * spread, pi_upper_km1 = d$mu + c1 * spread,
       df = as.integer(k - 2), n = k,
       method = "Prediction interval for a new study (Higgins, Thompson & Spiegelhalter 2009, sec. 3.1)")
}

# CANONICAL TEST
# r <- matau2pi(c(0.10,0.30,-0.20,0.45,0.05,0.22), c(0.02,0.05,0.03,0.08,0.01,0.04))
# stopifnot(r$pi_upper > r$pi_upper_km1)

#' @rdname matau2pi
#' @keywords internal
#' @export
morie_matau2pi <- matau2pi

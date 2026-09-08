# SPDX-License-Identifier: AGPL-3.0-or-later
#' Huber tuning constant for a target normal efficiency
#'
#' ARE(k) = (2 Phi(k) - 1)^2 / (2 Phi(k) - 1 - 2 k phi(k) + 2 k^2 (1 - Phi(k))),
#' increasing in k, inverted by bisection with a fixed step count.  Source
#' consulted: Huber (1964), Annals of Mathematical Statistics 35(1), 73-101.
#' Recovers the classical k = 1.345 at 95 per cent efficiency.
#'
#' @param efficiency target asymptotic relative efficiency at the normal.
#' @param lower,upper bisection bracket.
#' @param iters bisection steps.
#' @return list: estimate, efficiency, achieved, asymptotic_variance,
#'   breakdown_hint, n, method.
#' @keywords internal
#' @examples
#' opthr(0.95)$estimate
#' @export
opthr <- function(efficiency = 0.95, lower = 1e-6, upper = 20, iters = 200L) {
  are <- function(k) {
    if (k <= 0) return(0)
    phi <- stats::pnorm(k)
    den <- stats::dnorm(k)
    a <- 2 * phi - 1
    a * a / (a - 2 * k * den + 2 * k * k * (1 - phi))
  }
  lo <- as.numeric(lower)
  hi <- as.numeric(upper)
  for (i in seq_len(as.integer(iters))) {
    mid <- 0.5 * (lo + hi)
    if (are(mid) < efficiency) lo <- mid else hi <- mid
  }
  k <- 0.5 * (lo + hi)
  ach <- are(k)
  list(estimate = k, efficiency = as.numeric(efficiency), achieved = ach,
       asymptotic_variance = 1 / ach, breakdown_hint = 0.5, n = 0L,
       method = "Huber tuning constant for a target normal efficiency (Huber 1964)")
}

# CANONICAL TEST
# stopifnot(abs(opthr(0.95)$estimate - 1.345) < 5e-4)

#' @rdname opthr
#' @keywords internal
#' @export
morie_opthr <- opthr

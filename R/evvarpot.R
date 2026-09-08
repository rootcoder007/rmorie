# SPDX-License-Identifier: AGPL-3.0-or-later

#' Value-at-risk from a fitted GPD tail
#'
#' Formula: VaR_p = u + (sigma/xi) (((1-p)/zeta_u)^(-xi) - 1)
#'
#' Inverting the tail estimate
#' P(X > x) = zeta_u (1 + xi (x-u)/sigma)^(-1/xi).  The xi = 0 limit is
#' u + sigma log(zeta_u/(1-p)), used when |xi| is below 1e-12 so the
#' formula stays continuous.
#'
#' @param u Threshold.
#' @param sigma GPD scale, strictly positive.
#' @param xi GPD shape.
#' @param zeta_u Exceedance rate P(X > u), in (0, 1].
#' @param p VaR level in (0, 1); must exceed 1 - zeta_u.
#' @return List with \code{VaR}, \code{estimate}, \code{tail_prob},
#'   \code{p}, \code{method}.
#' @references McNeil & Frey (2000), J. Empirical Finance 7(3-4):271-300.
#' @export
#' @examples
#' Evvarpot(u = 1, sigma = 0.5, xi = 0.1, zeta_u = 0.1, p = 0.99)
Evvarpot <- function(u, sigma, xi, zeta_u, p) {
  u <- as.numeric(u)
  sigma <- as.numeric(sigma)
  xi <- as.numeric(xi)
  zeta_u <- as.numeric(zeta_u)
  p <- as.numeric(p)
  if (!(sigma > 0)) stop("sigma must be strictly positive")
  if (!(zeta_u > 0 && zeta_u <= 1)) stop("zeta_u must lie in (0, 1]")
  if (!(p > 0 && p < 1)) stop("p must lie strictly in (0, 1)")
  if (1 - p > zeta_u)
    stop("p is below the threshold exceedance rate; the GPD tail says nothing there")
  r <- (1 - p) / zeta_u
  var <- if (abs(xi) < 1e-12) u + sigma * log(1 / r) else
    u + (sigma / xi) * (r^(-xi) - 1)
  z <- 1 + xi * (var - u) / sigma
  tail <- if (abs(xi) < 1e-12) zeta_u * exp(-(var - u) / sigma) else
    if (z > 0) zeta_u * z^(-1 / xi) else 0
  .t1_result(VaR = var, estimate = var, tail_prob = tail, p = p,
             method = "GPD value-at-risk above a POT threshold")
}

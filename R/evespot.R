# SPDX-License-Identifier: AGPL-3.0-or-later

#' Expected shortfall of a GPD tail above a threshold
#'
#' Formula: ES_p = (VaR_p + sigma - xi u) / (1 - xi)
#'
#' E\[X | X > VaR\] for the generalised Pareto tail.  The mean excess of a
#' GPD is finite only for xi < 1, so xi >= 1 is refused rather than
#' returned as a number.  At xi = 0 the expression collapses to
#' VaR + sigma, the memoryless exponential mean excess.
#'
#' @param u Threshold the GPD was fitted above.
#' @param sigma GPD scale, strictly positive.
#' @param xi GPD shape.
#' @param VaR Value-at-risk at the level of interest.
#' @return List with \code{ES}, \code{estimate}, \code{ratio},
#'   \code{xi}, \code{method}.
#' @references McNeil & Frey (2000), J. Empirical Finance 7(3-4):271-300.
#' @export
#' @examples
#' Evespot(u = 1, sigma = 0.5, xi = 0.1, VaR = 2)
Evespot <- function(u, sigma, xi, VaR) {
  u <- as.numeric(u); sigma <- as.numeric(sigma)
  xi <- as.numeric(xi); VaR <- as.numeric(VaR)
  if (!(sigma > 0)) stop("sigma must be strictly positive")
  if (xi >= 1) stop("expected shortfall is infinite for xi >= 1")
  es <- (VaR + sigma - xi * u) / (1 - xi)
  .t1_result(ES = es, estimate = es,
             ratio = if (VaR != 0) es / VaR else NaN, xi = xi,
             method = "GPD expected shortfall above a POT threshold")
}

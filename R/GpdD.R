# SPDX-License-Identifier: AGPL-3.0-or-later
#' Generalized Pareto distribution
#'
#' The distribution of exceedances over a high threshold.  The xi -> 0
#' limit is the exponential, the mean is sigma/(1 - xi) for xi < 1 and
#' infinite otherwise, and the quantile function inverts in closed form;
#' all three are what the tests check.  The wave2 audit pointed this
#' module at \code{dtgpd} as a duplicate; dtgpd is itself still an
#' unimplemented placeholder, so there is nothing to delegate to.
#'
#' Formula: F(x) = 1 - (1 + xi x/sigma)^(-1/xi), or 1 - exp(-x/sigma)
#'   when xi = 0.
#'
#' @param sigma Positive scale.
#' @param xi Shape.
#' @param x Points at which the CDF and density are evaluated.
#' @param p Probabilities at which quantiles are returned.
#' @return List with \code{estimate} (mean), \code{cdf}, \code{pdf},
#'   \code{quantile}, \code{mean}, \code{variance},
#'   \code{upper_endpoint}, \code{sigma}, \code{xi}, \code{n},
#'   \code{method}.
#' @references Pickands (1975), Statistical inference using extreme
#'   order statistics, Annals of Statistics 3(1):119-131.
#'   \doi{10.1214/aos/1176343003}
#' @export
#' @examples
#' GpdD(sigma = 0.5, xi = 5L)
GpdD <- function(sigma, xi, x = NULL, p = NULL) {
  s <- as.numeric(sigma)
  k <- as.numeric(xi)
  if (s <= 0) stop("gpd_distribution: sigma must be positive")
  xs <- if (is.null(x)) c(0.5, 1, 2, 4) else .s03vec(x)
  ps <- if (is.null(p)) c(0.5, 0.9, 0.95, 0.99) else .s03vec(p)
  if (any(ps <= 0 | ps >= 1)) stop("gpd_distribution: probabilities must lie in (0, 1)")
  eps <- 1e-12
  cdf1 <- function(v) {
    if (v <= 0) return(0)
    if (abs(k) < eps) return(1 - exp(-v / s))
    z <- 1 + k * v / s
    if (z <= 0) return(1)
    1 - z^(-1 / k)
  }
  pdf1 <- function(v) {
    if (v < 0) return(0)
    if (abs(k) < eps) return(exp(-v / s) / s)
    z <- 1 + k * v / s
    if (z <= 0) return(0)
    z^(-1 / k - 1) / s
  }
  q1 <- function(pp) if (abs(k) < eps) -s * log(1 - pp) else s * ((1 - pp)^(-k) - 1) / k
  upper <- if (k >= 0) Inf else -s / k
  mean <- if (k < 1) s / (1 - k) else Inf
  var <- if (k < 0.5) s * s / ((1 - k)^2 * (1 - 2 * k)) else Inf
  .t1_result(estimate = mean, cdf = vapply(xs, cdf1, 0), pdf = vapply(xs, pdf1, 0),
             quantile = vapply(ps, q1, 0), mean = mean, variance = var,
             upper_endpoint = upper, sigma = s, xi = k, n = length(xs),
             method = "F(x) = 1 - (1 + xi x/sigma)^(-1/xi), Pickands (1975)")
}

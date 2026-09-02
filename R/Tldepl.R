# SPDX-License-Identifier: AGPL-3.0-or-later
#' Lower tail dependence coefficient
#'
#' lambda_L = lim_\{u->0+\} C(u,u) / u.  The lower tail of (X, Y) is the
#' upper tail of (-X, -Y), so the estimator is Coles' chi(u) applied to
#' the reflected sample; no second estimator is written.  For a radially
#' symmetric dependence structure the two coefficients coincide, which is
#' the anchor used in the tests.
#'
#' @param y Numeric vector, first margin.
#' @param copula Numeric vector, second margin (equal length).
#' @param theta Threshold in (0, 1), default 0.95.
#' @return List with \code{estimate}, \code{u}, \code{n}, \code{method}.
#' @references Joe, H. (1997). Multivariate Models and Multivariate
#'   Dependence Concepts. Chapman and Hall, section 2.1.10.
#' @examples
#' Tldepl(1:20, 1:20, 0.5)
#' @export
Tldepl <- function(y, copula, theta = 0.95) {
  r <- ChiDep(-.s03vec(y), -.s03vec(copula), theta)
  r$method <- "lower tail dependence via reflected empirical chi(u) [Joe 1997]"
  r
}

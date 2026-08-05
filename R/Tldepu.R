# SPDX-License-Identifier: AGPL-3.0-or-later
#' Upper tail dependence coefficient
#'
#' lambda_U = lim_{u->1-} (1 - 2u + C(u,u)) / (1 - u), estimated by Coles'
#' chi(u) diagnostic, which converges to exactly this limit.  A thin alias
#' for \code{ChiDep}: the estimator already exists and is not duplicated
#' here.
#'
#' @param y Numeric vector, first margin.
#' @param copula Numeric vector, second margin (equal length).
#' @param theta Threshold in (0, 1), default 0.95.
#' @return List with \code{estimate}, \code{u}, \code{n}, \code{method}.
#' @references Joe, H. (1997). Multivariate Models and Multivariate
#'   Dependence Concepts. Chapman and Hall, section 2.1.10.
#'   Coles, S. (2001). An Introduction to Statistical Modeling of Extreme
#'   Values. Springer, section 8.4.
#' @examples
#' Tldepu(1:20, 1:20, 0.5)
#' @export
Tldepu <- function(y, copula, theta = 0.95) {
  r <- ChiDep(y, copula, theta)
  r$method <- "upper tail dependence via empirical chi(u) [Joe 1997; Coles 2001]"
  r
}

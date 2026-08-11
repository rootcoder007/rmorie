# SPDX-License-Identifier: AGPL-3.0-or-later

#' Manski no-assumption bounds on the ATE
#'
#' \code{Manski} and \code{morie_bnd_manski} document the SAME method:
#' the worst-case decomposition of each counterfactual mean over the
#' outcome support, differenced for the ATE. Rather than carry a second
#' implementation -- which would agree with the first at 1e-9 forever
#' while doubling the surface -- this function forwards to
#' \code{\link{morie_bnd_manski}} with the argument layout of its stub.
#' The ATE interval always has width exactly \eqn{y_{max} - y_{min}}
#' and therefore always contains zero.
#'
#' @param y Observed outcome.
#' @param D Treatment indicator, coded 0/1.
#' @param y_min Lower endpoint of the outcome support.
#' @param y_max Upper endpoint of the outcome support.
#' @return Whatever \code{\link{morie_bnd_manski}} returns for the ATE
#'   case: \code{ate_lower}, \code{ate_upper}, \code{ate_width},
#'   \code{y1_bounds}, \code{y0_bounds}, \code{p_treated},
#'   \code{contains_zero}, \code{n}.
#' @references Manski, C. F. (1990), Nonparametric Bounds on Treatment
#'   Effects, American Economic Review Papers and Proceedings
#'   80(2):319-323. Molinari, F. (2021), Microeconometrics with Partial
#'   Identification, Handbook of Econometrics 7A, eq. (2.11) and p. 18
#'   (arXiv:2004.11751).
#' @export
Manski <- function(y, D, y_min, y_max) {
  morie_bnd_manski(y, NULL, c(y_min, y_max), treatment = D)
}

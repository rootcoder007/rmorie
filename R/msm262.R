# SPDX-License-Identifier: AGPL-3.0-or-later
#' Basis expansion of the coefficient function (MVSML eq. 14.2)
#'
#' \code{beta(t) = sum_{l=1}^{L1} beta_l phi_l(t)}, the device that makes
#' eq. (14.1) estimable: an infinite-dimensional unknown function is
#' replaced by \code{L1} scalars, after which (14.1) collapses to the
#' ordinary linear model (14.3).
#'
#' This is the named R arm of module \code{msm262}. The expansion itself
#' is implemented once, in \code{Basexp} of \code{gp_mvsml_ch9_14.R}; this
#' entry point only exports it under the module's own name.
#'
#' @param t Numeric grid of evaluation points.
#' @param beta_coef Numeric basis coefficients \code{beta_1..beta_L1}.
#' @param kind Basis family, "fourier" or "poly".
#' @param period Optional Fourier period; the grid span by default.
#' @return List with beta_t, t, n_basis.
#' @references Montesinos Lopez, Montesinos Lopez and Crossa (2022),
#'   Multivariate Statistical Machine Learning Methods for Genomic
#'   Prediction, Springer, eq. (14.2) p.579,
#'   \doi{10.1007/978-3-030-89010-0}.
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' Msm262(V, V)
Msm262 <- function(t, beta_coef, kind = "fourier", period = NULL) {
  Basexp(t, beta_coef, kind, period)
}

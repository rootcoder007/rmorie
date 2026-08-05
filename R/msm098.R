# SPDX-License-Identifier: AGPL-3.0-or-later
#' Ordinal predictor with environment and genetic effects.
#'
#' Formula: L = X_E beta_E + Z_L g + eps (eq. 7.5), environment effects and
#' genetic effects without the line-by-environment interaction. With \code{L_g}
#' the Cholesky factor of G, the genetic block enters as Z_L L_g, the design
#' used in Table 7.6.
#'
#' @param n Number of rows to take from each block.
#' @param X_E Environment design block.
#' @param Z_L Line incidence matrix.
#' @param L_g Cholesky factor of G, so that G = L_g L_g'.
#' @return List with estimate (the total column count), design, widths, method.
#' @references Montesinos Lopez, Montesinos Lopez & Crossa (2022),
#'   Multivariate Statistical Machine Learning Methods for Genomic Prediction,
#'   Springer, eq. (7.5) p.220 and Table 7.6 p.233.
#'   DOI 10.1007/978-3-030-89010-0.
#' @export
Msm098 <- function(n, X_E, Z_L, L_g = NULL) {
  f <- .gpordlatent(as.integer(n), X_E = X_E, Z_L = Z_L, L_g = L_g)
  list(estimate = as.numeric(f$n_columns), design = f$design, widths = f$widths,
       method = "ordinal environment + genetic predictor (MVSML 2022 eq. 7.5)")
}

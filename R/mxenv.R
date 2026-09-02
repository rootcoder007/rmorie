# SPDX-License-Identifier: AGPL-3.0-or-later
#' Multi-environment genomic model with a genotype-by-environment term
#'
#' Formula: Y = 1 mu + X_E beta_E + Z_L b_1 + Z_EL b_2 + eps,  b_1 ~ N(0, sigma2_g G),  b_2 ~ N(0, Sigma_E (x) G)
#'
#' @param y Response vector of length n.
#' @param X_E Environment design matrix; None or empty uses an intercept only.
#' @param Z_L Design matrix of lines.
#' @param Z_EL Design matrix of the line-by-environment interaction.
#' @param G Genomic relationship matrix.
#' @param sigma2_g Genomic variance component.
#' @param Sigma_E Genetic covariance between environments.
#' @param sigma2_e Residual variance component.
#'
#' @return List with ``beta``, ``b_lines``, ``b_gxe``, ``n``.
#' @references Montesinos Lopez, Montesinos Lopez and Crossa (2022), Multivariate Statistical Machine Learning Methods for Genomic Prediction, Springer, doi:10.1007/978-3-030-89010-0.  Chapter 5, Eq. (5.4) p. 150.  The two random terms are stacked into one Z = \[Z_L  Z_EL\] with a block-diagonal Sigma and solved as Eq. (5.1).  Delegates to the chapter routine in morie.fn._gp_core, which was verified against this book in the earlier tranches of this shelf recorded in ledger/SHELF_LEDGER.txt; the page and equation number above are that routine's own, re-read against the chapter PDF here.
#' @export
Gxeblup <- function(y, X_E, Z_L, Z_EL, G, sigma2_g, Sigma_E, sigma2_e = 1) {
  out <- morie_gxe_blup(y, X_E, Z_L, Z_EL, G, as.numeric(sigma2_g),
                        Sigma_E, as.numeric(sigma2_e))
  .t1_result(beta = out$beta, b_lines = out$b_lines, b_gxe = out$b_gxe,
             n = length(.t1_vec(y)),
             method = "Multi-environment GxE model, MVSML Eq. (5.4)")
}

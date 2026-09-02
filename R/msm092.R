# SPDX-License-Identifier: AGPL-3.0-or-later
#' Ordinal latent predictor with environment, markers and interaction
#'
#' Formula: L = X_E beta_E + X beta + X_EM beta_EM + eps (eq. 7.3), the ordinal
#' latent variable with a flat prior on the environment effects and a
#' BRR/BayesA/BayesB/BayesC/BL prior on the marker and marker-by-environment
#' effects. Blocks are stacked in the order printed in Table 7.6.
#'
#' @param n Number of rows to take from each block.
#' @param X_E Environment design block.
#' @param X Marker design block.
#' @param X_EM Marker-by-environment design block.
#' @return List with estimate (the total column count), design, widths, method.
#' @references Montesinos Lopez, Montesinos Lopez & Crossa (2022),
#'   Multivariate Statistical Machine Learning Methods for Genomic Prediction,
#'   Springer, eq. (7.3) p.219. DOI 10.1007/978-3-030-89010-0.
#' @export
Msm092 <- function(n, X_E = NULL, X = NULL, X_EM = NULL) {
  f <- .gpordlatent(as.integer(n), X_E = X_E, X = X, X_EM = X_EM)
  list(estimate = as.numeric(f$n_columns), design = f$design, widths = f$widths,
       method = "ordinal latent predictor (MVSML 2022 eq. 7.3)")
}

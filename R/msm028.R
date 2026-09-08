# SPDX-License-Identifier: AGPL-3.0-or-later
#' Multi-trait model with extra fixed effects
#'
#' Formula: Y = (1_IJ (x) I_nT) mu + X beta + Z b + eps (eq. 5.5a), the
#' multi-trait model of eq. (5.5) extended with a fixed-effects term X beta.
#'
#' @param Y Lines-by-traits response matrix (J x n_T).
#' @param Z Random-effects incidence matrix over lines.
#' @param G Genomic relationship matrix.
#' @param Sigma_T Genetic covariance between traits.
#' @param R_T Residual covariance between traits.
#' @param X Extra fixed-effects design in the stacked ordering.
#' @return List with estimate, mu, beta, b, method.
#' @references Montesinos Lopez, Montesinos Lopez & Crossa (2022),
#'   Multivariate Statistical Machine Learning Methods for Genomic Prediction,
#'   Springer, eq. (5.5a) p.153. DOI 10.1007/978-3-030-89010-0.
#' @export
Msm028 <- function(Y, Z, G, Sigma_T, R_T, X = NULL) {
  f <- .gpmultitrait(Y, Z, G, Sigma_T, R_T, X = X)
  list(estimate = f$mu[1L], mu = f$mu, beta = f$beta, b = f$b,
       method = "multi-trait LMM with fixed effects (MVSML 2022 eq. 5.5a)")
}

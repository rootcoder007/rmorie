# SPDX-License-Identifier: AGPL-3.0-or-later
#' Multi-trait genomic linear mixed model.
#'
#' Formula: stacking the n_T traits of each line,
#' Y = (1 (x) I_nT) mu + Z b + eps with b ~ N(0, G (x) Sigma_T) and
#' eps ~ N(0, I_J (x) R_nT) (eq. 5.5). Sigma_T is the genetic covariance
#' between traits. When Sigma_T and R are diagonal the fit is equivalent to
#' fitting a univariate GBLUP per trait.
#'
#' @param Y Lines-by-traits response matrix (J x n_T).
#' @param Z Random-effects incidence matrix over lines.
#' @param G Genomic relationship matrix.
#' @param Sigma_T Genetic covariance between traits.
#' @param R_T Residual covariance between traits.
#' @return List with estimate, mu, b, b_by_line, method.
#' @references Montesinos Lopez, Montesinos Lopez & Crossa (2022),
#'   Multivariate Statistical Machine Learning Methods for Genomic Prediction,
#'   Springer, eq. (5.5) p.153. DOI 10.1007/978-3-030-89010-0.
#' @export
Msm026 <- function(Y, Z, G, Sigma_T, R_T) {
  f <- .gpmultitrait(Y, Z, G, Sigma_T, R_T)
  list(estimate = f$mu[1L], mu = f$mu, b = f$b, b_by_line = f$b_by_line,
       method = "multi-trait genomic LMM (MVSML 2022 eq. 5.5)")
}

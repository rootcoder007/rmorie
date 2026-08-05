# SPDX-License-Identifier: AGPL-3.0-or-later
#' Linear multiple regression fitted by OLS.
#'
#' Formula: Y = beta_0 + sum_j X_j beta_j + eps (eq. 3.1) fitted by least
#' squares: beta = (X'X)^-1X'y, Var(beta) = sigma2 (X'X)^-1 and
#' sigma2 = RSS/(n - p - 1).
#'
#' @param X Design matrix.
#' @param y Response vector.
#' @param add_intercept Prepend a column of ones.
#' @return List with estimate, beta, se, sigma2, fitted, residuals, method.
#' @references Montesinos Lopez, Montesinos Lopez & Crossa (2022),
#'   Multivariate Statistical Machine Learning Methods for Genomic Prediction,
#'   Springer, eq. (3.1) pp.71-73. DOI 10.1007/978-3-030-89010-0.
#' @export
Msm332 <- function(X, y, add_intercept = TRUE) {
  f <- .gpolsfit(X, y, add_intercept = add_intercept)
  list(estimate = f$beta[1L], beta = f$beta, se = f$se_beta, sigma2 = f$sigma2,
       fitted = f$fitted, residuals = f$residuals,
       method = "OLS linear multiple regression (MVSML 2022 eq. 3.1)")
}

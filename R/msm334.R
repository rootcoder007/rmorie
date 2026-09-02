# SPDX-License-Identifier: AGPL-3.0-or-later
#' Expected prediction error of the OLS fit.
#'
#' Formula: EPE(x_o) = sigma2 (1 + sum_j (x*_oj)^2 / lambda_j) with
#' x* = Gamma'x_o and lambda_j the eigenvalues of X'X (p.80): nearly dependent
#' features drive some lambda_j toward zero and blow the prediction error up.
#'
#' @param sigma2 Irreducible error variance.
#' @param x_star Rotated feature vector Gamma'x_o.
#' @param eigenvalues Eigenvalues of X'X, all strictly positive.
#' @return List with estimate, irreducible, variance_inflation, method.
#' @references Montesinos Lopez, Montesinos Lopez & Crossa (2022),
#'   Multivariate Statistical Machine Learning Methods for Genomic Prediction,
#'   Springer, sec. 3.5 p.80. DOI 10.1007/978-3-030-89010-0.
#' @export
#' @examples
#' Msm334(sigma2 = c(1, 2, 3, 4, 5, 6, 7, 8), x_star = c(1, 2, 3, 4, 5, 6, 7, 8), eigenvalues = c(1, 2, 3, 4, 5, 6, 7, 8))
Msm334 <- function(sigma2, x_star, eigenvalues) {
  v <- .gpepe(sigma2, x_star, eigenvalues)
  list(estimate = v, irreducible = as.numeric(sigma2),
       variance_inflation = v / as.numeric(sigma2),
       method = "expected prediction error (MVSML 2022 p.80)")
}

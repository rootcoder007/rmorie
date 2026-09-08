# SPDX-License-Identifier: AGPL-3.0-or-later
#' VAR forecast error variance decomposition -- alias of \code{\link{Fevdc}}
#'
#' DUPLICATE, resolved by aliasing (wave-2 DUPMAP: vardec -> fevdc). Both
#' names denote the same quantity: with P the lower Cholesky factor of
#' Sigma_u and Theta_s the MA coefficient matrices of the fitted VAR, the
#' share of the h-step forecast error variance of variable i due to
#' orthogonalised shock j is the ratio of sum_s (Theta_s P)[i, j]^2 to its
#' row total. \code{Fevdc} implements it; this is a re-export, not a
#' second copy.
#'
#' @param var_coefficients VAR(1) coefficient matrix A (k by k).
#' @param sigma_u Residual covariance matrix (k by k).
#' @param periods Forecast horizon.
#' @return As \code{\link{Fevdc}}.
#' @references Lutkepohl, H. (2005). New Introduction to Multiple Time
#'   Series Analysis. Springer. doi:10.1007/978-3-540-27752-1.
#' @examples
#' Vardec(matrix(c(0.5, 0.1, 0.2, 0.4), 2, 2), diag(2), 3)$periods
#' @export
Vardec <- function(var_coefficients, sigma_u, periods = 20) {
  Fevdc(var_coefficients, sigma_u, periods)
}

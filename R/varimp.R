# SPDX-License-Identifier: AGPL-3.0-or-later
#' VAR impulse response function -- alias of \code{\link{Irfun}}
#'
#' DUPLICATE, resolved by aliasing (wave-2 DUPMAP: varimp -> irfun). Both
#' names denote the orthogonalised impulse response of a VAR(p): the MA
#' recursion Phi_0 = I, Phi_h = sum_{j=1..min(h,p)} A_j Phi_{h-j}, with
#' shocks orthogonalised by the lower Cholesky factor P of Sigma_u, so
#' Theta_h = Phi_h P. The name is a trap -- it reads as "variable
#' importance", but the stub docstring and the wave-2 categorisation both
#' give VAR impulse response, which is what is aliased here.
#'
#' @param coef m by (1 + m p) coefficient matrix from \code{Varest}.
#' @param sigma_u m by m residual covariance.
#' @param horizon Periods ahead.
#' @param shock_var 0-based index of the shocked variable.
#' @return As \code{\link{Irfun}}.
#' @references Lutkepohl, H. (2005). New Introduction to Multiple Time
#'   Series Analysis. Springer, Ch. 2.3. doi:10.1007/978-3-540-27752-1.
#' @examples
#' Varimp(matrix(c(0, 0.5, 0.1, 0, 0.2, 0.4), 2, 3), diag(2), 3)$irf
#' @export
Varimp <- function(coef, sigma_u, horizon = 20, shock_var = 0) {
  Irfun(coef, sigma_u, horizon, shock_var)
}

# SPDX-License-Identifier: AGPL-3.0-or-later
#' VAR(p) multivariate autoregression -- alias of \code{\link{Varest}}
#'
#' DUPLICATE, resolved by aliasing. This module and \code{Varest} name the
#' same estimator: the reduced-form VAR(p), y_t = nu + A_1 y_\{t-1\} + ... +
#' A_p y_\{t-p\} + u_t, fitted by multivariate least squares. Sims (1980),
#' "Macroeconomics and Reality", Econometrica 48(1):1-48,
#' doi:10.2307/1912017, put the reduced-form VAR into macroeconometrics;
#' Lutkepohl (2005), doi:10.1007/978-3-540-27752-1, states the estimator,
#' and that is what \code{Varest} implements. The two differ in
#' provenance, not in arithmetic, so this is a re-export rather than a
#' second copy.
#'
#' @param Y T-by-K matrix of observations.
#' @param p Lag order.
#' @param intercept Include the constant nu.
#' @return As \code{\link{Varest}}.
#' @references Sims, C.A. (1980). Macroeconomics and Reality.
#'   Econometrica 48(1):1-48. doi:10.2307/1912017.
#' @examples
#' VarF(cbind(c(1, 2, 3, 4, 5), c(2, 1, 4, 3, 6)), 1)$loglik
#' @export
VarF <- function(Y, p = 1, intercept = TRUE) Varest(Y, p, intercept)

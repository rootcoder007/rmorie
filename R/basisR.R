# SPDX-License-Identifier: AGPL-3.0-or-later
#' Basis-function representation of a discretely observed curve
#'
#' Ramsay and Silverman (2005), Functional Data Analysis, 2nd ed., Springer,
#' Chapter 4 "Smoothing functional data by least squares", Section 4.2: a
#' curve observed at t_1..t_n is written x(t) = sum_k c_k phi_k(t) and the
#' coefficients minimise SSE(c) = sum_i (y_i - sum_k c_k phi_k(t_i))^2, whose
#' normal equations give c = (Phi Phi)^-1 Phi y.
#'
#' @param y the n observed values.
#' @param Phi n-by-K matrix of basis functions at the sampling points.
#' @return list: estimate, coef, fitted, residual, sse, df, n, method.
#' @keywords internal
#' @examples
#' BasisR(c(1, 2, 3), cbind(1, c(0, 1, 2)))$coef
#' @export
BasisR <- function(y, Phi) {
  yy <- .s03vec(y)
  P <- .s03mat(Phi)
  n <- length(yy)
  if (n == 0L) stop("basis_representation: y is empty")
  if (nrow(P) != n) stop("basis_representation: Phi must have one row per observation")
  K <- ncol(P)
  if (K == 0L) stop("basis_representation: Phi has no columns")
  cc <- .s03lstsq(P, yy, 0)
  fit <- .s03matvec(P, cc)
  res <- yy - fit
  sse <- sum(res * res)
  list(estimate = cc[1], coef = cc, fitted = fit, residual = res,
       sse = sse, df = n - K, n = n,
       method = "Ramsay-Silverman (2005) Sect. 4.2 least-squares basis expansion, c = (Phi Phi)^-1 Phi y")
}

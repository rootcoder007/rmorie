# SPDX-License-Identifier: AGPL-3.0-or-later
#' Function-on-function linear regression
#'
#' Ramsay and Silverman (2005), Functional Data Analysis, 2nd ed., Springer,
#' Chapter 16 "Functional linear models for functional responses":
#' y_i(s) = integral beta(s, t) x_i(t) dt + eps_i(s).
#'
#' Expand the coefficient surface in a product of two bases,
#' beta(s, t) = sum_j sum_k B[j, k] phi_j(t) psi_k(s), expand each predictor
#' curve on the t-basis and each response curve on the s-basis, and the model
#' reduces to the multivariate regression Cy = Cx B with
#' B = (Cx'Cx)^-1 Cx'Cy, which is the finite-dimensional form the chapter
#' works in.  The curve coefficients are the least-squares expansions of
#' Section 4.2.
#'
#' The reduction has an exact consequence used as this module's anchor: if the
#' responses ARE the predictors and the two bases are the same, then Cy = Cx
#' and B is the identity, whatever the data.
#'
#' @param X N-by-Tx matrix of predictor curves, one per row.
#' @param Y N-by-Ty matrix of response curves, one per row.
#' @param basis_X Tx-by-Kx basis for the predictor argument.
#' @param basis_Y Ty-by-Ky basis for the response argument.
#' @return list: estimate, B, Cx, Cy, fitted_coef, sse, n, method.
#' @keywords internal
#' @examples
#' X <- matrix(c(1, 2, 2, 1, 3, 3), 3, 2)
#' Fnlm(X, X, diag(2), diag(2))$B
#' @export
Fnlm <- function(X, Y, basis_X, basis_Y) {
  Xm <- .s03mat(X)
  Ym <- .s03mat(Y)
  BX <- .s03mat(basis_X)
  BY <- .s03mat(basis_Y)
  N <- nrow(Xm)
  if (N == 0L) stop("function_on_function: X is empty")
  if (nrow(Ym) != N) stop("function_on_function: X and Y must have the same number of curves")
  Tx <- ncol(Xm); Ty <- ncol(Ym)
  if (nrow(BX) != Tx) stop("function_on_function: basis_X must have one row per predictor argument")
  if (nrow(BY) != Ty) stop("function_on_function: basis_Y must have one row per response argument")
  Kx <- ncol(BX); Ky <- ncol(BY)
  if (Kx == 0L || Ky == 0L) stop("function_on_function: a basis has no columns")
  if (N < Kx) stop("function_on_function: need at least as many curves as predictor basis functions")
  Cx <- matrix(0, N, Kx)
  Cy <- matrix(0, N, Ky)
  for (i in seq_len(N)) {
    Cx[i, ] <- .s03lstsq(BX, Xm[i, ], 0)
    Cy[i, ] <- .s03lstsq(BY, Ym[i, ], 0)
  }
  Bm <- matrix(0, Kx, Ky)
  for (j in seq_len(Ky)) Bm[, j] <- .s03lstsq(Cx, Cy[, j], 0)
  fitc <- .s03matmul(Cx, Bm)
  sse <- 0
  for (i in seq_len(N)) for (j in seq_len(Ky)) {
    r <- Cy[i, j] - fitc[i, j]
    sse <- sse + r * r
  }
  list(estimate = Bm[1, 1], B = Bm, Cx = Cx, Cy = Cy, fitted_coef = fitc,
       sse = sse, n = N,
       method = "Ramsay-Silverman (2005) Ch.16 function-on-function regression, product-basis reduction Cy = Cx B")
}

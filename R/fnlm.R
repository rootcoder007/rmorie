# SPDX-License-Identifier: AGPL-3.0-or-later
#' Function-on-function (fully functional) linear regression
#'
#' Ramsay and Silverman (2005), Functional Data Analysis, 2nd ed., Springer,
#' Chapter 16 "Functional linear models for functional responses", Section
#' 16.1 p. 279 and Section 16.4.1 pp. 291-292, read as rendered page images
#' rather than from an extracted text layer.
#'
#' The model of equation (16.6) is y*(t) = integral z*(s) beta(s, t) ds +
#' eps(t), with the tensor-product expansion (16.3)
#' beta(s, t) = theta'(s) B eta(t), theta a basis of K1 functions of s and eta
#' a basis of K2 functions of t.  Substituting (16.3) into (16.6) gives (16.7),
#' y*(t) = Z* B eta(t) + eps(t), with Z* = integral z*(s) theta'(s) ds of
#' equation (16.8), and the criterion (16.2) is minimised by the normal
#' equations (16.9) on p. 292,
#' Z*' Z* B integral eta eta' dt = Z*' integral y eta' dt.
#' Writing J = integral eta(t) eta'(t) dt this is solved here as
#' B = (Z*' Z*)^-1 (Z*' M) J^-1 with M_il = integral y_i(t) eta_l(t) dt, which
#' is (16.9) rearranged, not an approximation of it.  The Kronecker form
#' (16.10) is the same equation vectorised and would give the identical B.
#'
#' No regularisation is applied: this is Section 16.4.1, "Fitting the model
#' without regularization".  The penalised variant of Section 16.4.2,
#' equations (16.13)-(16.15), needs the operators L_s and L_t as inputs.
#' All integrals are composite trapezoid rules over the whole grid.
#'
#' @param X N-by-ns matrix of covariate curves z_i(s), one curve per row.
#' @param Y N-by-nt matrix of response curves y_i(t), one curve per row.
#' @param basis_X ns-by-K1 matrix, the basis theta on the s grid.
#' @param basis_Y nt-by-K2 matrix, the basis eta on the t grid.
#' @param s,t the two grids; default to equally spaced on \[0, 1\].
#' @return list: estimate, B, beta, Z, J, fitted, residual, sse, ssy, r2, n,
#'   method.
#' @keywords internal
#' @examples
#' Fnlm(rbind(c(1, 1, 1), c(2, 2, 2)), rbind(c(3, 3, 3), c(6, 6, 6)),
#'      cbind(rep(1, 3)), cbind(rep(1, 3)))$estimate
#' @export
Fnlm <- function(X, Y, basis_X, basis_Y, s = NULL, t = NULL) {
  Xm <- .s03mat(X)
  Ym <- .s03mat(Y)
  Th <- .s03mat(basis_X)
  Et <- .s03mat(basis_Y)
  N <- nrow(Xm)
  if (N == 0L) stop("function_on_function: X is empty")
  if (nrow(Ym) != N) stop("function_on_function: X and Y must have the same number of curves")
  ns <- ncol(Xm)
  nt <- ncol(Ym)
  if (ns < 2L || nt < 2L) stop("function_on_function: need at least two points on each grid")
  if (nrow(Th) != ns) stop("function_on_function: basis_X must have one row per s point")
  if (nrow(Et) != nt) stop("function_on_function: basis_Y must have one row per t point")
  K1 <- ncol(Th)
  K2 <- ncol(Et)
  if (K1 == 0L || K2 == 0L) stop("function_on_function: a basis has no columns")
  if (N < K1) stop("function_on_function: need at least K1 curves")
  ss <- if (is.null(s)) (seq_len(ns) - 1) / (ns - 1) else .s03vec(s)
  tt <- if (is.null(t)) (seq_len(nt) - 1) / (nt - 1) else .s03vec(t)
  if (length(ss) != ns) stop("function_on_function: s must match the columns of X")
  if (length(tt) != nt) stop("function_on_function: t must match the columns of Y")

  Z <- matrix(0, N, K1)
  for (i in seq_len(N)) for (cc in seq_len(K1)) {
    Z[i, cc] <- .fnlm_trapz(ss, Xm[i, ] * Th[, cc])
  }
  M <- matrix(0, N, K2)
  for (i in seq_len(N)) for (cc in seq_len(K2)) {
    M[i, cc] <- .fnlm_trapz(tt, Ym[i, ] * Et[, cc])
  }
  J <- matrix(0, K2, K2)
  for (cc in seq_len(K2)) for (dd in seq_len(K2)) {
    J[cc, dd] <- .fnlm_trapz(tt, Et[, cc] * Et[, dd])
  }
  A <- .s03crossprod(Z)
  R <- .s03matmul(t(Z), M)
  Wm <- matrix(0, K1, K2)
  for (cc in seq_len(K2)) Wm[, cc] <- .s03ridgesolve(A, R[, cc], 0)
  B <- matrix(0, K1, K2)
  for (r in seq_len(K1)) B[r, ] <- .s03ridgesolve(J, Wm[r, ], 0)

  beta <- matrix(0, ns, nt)
  for (a in seq_len(ns)) for (b in seq_len(nt)) {
    v <- 0
    for (cc in seq_len(K1)) for (dd in seq_len(K2)) v <- v + Th[a, cc] * B[cc, dd] * Et[b, dd]
    beta[a, b] <- v
  }
  fitted <- matrix(0, N, nt)
  for (i in seq_len(N)) for (b in seq_len(nt)) {
    v <- 0
    for (cc in seq_len(K1)) for (dd in seq_len(K2)) v <- v + Z[i, cc] * B[cc, dd] * Et[b, dd]
    fitted[i, b] <- v
  }
  resid <- Ym - fitted
  sse <- 0
  ssy <- 0
  for (i in seq_len(N)) {
    sse <- sse + .fnlm_trapz(tt, resid[i, ] * resid[i, ])
    ssy <- ssy + .fnlm_trapz(tt, Ym[i, ] * Ym[i, ])
  }
  r2 <- if (ssy > 0) 1 - sse / ssy else 0
  list(estimate = B[1, 1], B = B, beta = beta, Z = Z, J = J, fitted = fitted,
       residual = resid, sse = sse, ssy = ssy, r2 = r2, n = N,
       method = "Ramsay-Silverman (2005) eqs. (16.3), (16.6)-(16.9), unregularised tensor-product fit")
}

#' .fnlm_trapz
#'
#' A step of the fnlm implementation. Called by \code{Fnlm}.
#' See the file header for the source the module follows.
#' it follows.
#'
#' @param t A vector; its length is taken and its elements indexed.
#' @param v A vector; indexed elementwise.
#' @return The value of \code{s}, as built in the body.
#' @export
.fnlm_trapz <- function(t, v) {
  s <- 0
  n <- length(t)
  if (n > 1L) for (i in seq_len(n - 1L)) s <- s + 0.5 * (v[i] + v[i + 1L]) * (t[i + 1L] - t[i])
  s
}

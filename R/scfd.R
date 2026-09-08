# SPDX-License-Identifier: AGPL-3.0-or-later
#' Scalar-on-function linear regression
#'
#' Reiss and Ogden (2007), "Functional principal component regression and
#' functional partial least squares", JASA 102(479), 984-996, on the model of
#' Ramsay and Silverman (2005), Functional Data Analysis, 2nd ed., Chapter 15:
#' y_i = alpha + integral beta(t) x_i(t) dt + eps_i.
#'
#' Expanding beta(t) = sum_k b_k phi_k(t) makes the integral linear in b,
#' integral beta x_i = sum_k b_k J[i, k] with J[i, k] = integral phi_k x_i, so
#' the model is the ordinary linear regression of y on the design [1, J].
#' That basis-expansion reduction is what makes the problem finite
#' dimensional, and is the same device Reiss and Ogden use before choosing the
#' basis by principal components.
#'
#' The inner products J are taken by the composite trapezoid rule over the
#' whole observation grid.
#'
#' @param X N-by-T matrix, one predictor curve per row.
#' @param Y N scalar responses.
#' @param basis T-by-K matrix of basis functions on the grid.
#' @param t the grid of length T; defaults to equally spaced on [0, 1].
#' @return list: estimate, alpha, coef, beta, J, fitted, residual, sse, r2,
#'   df, n, method.
#' @keywords internal
#' @examples
#' Scfd(matrix(c(1, 2, 3, 4, 5, 6), 3, 2), c(1, 2, 3), matrix(1, 2, 1))$alpha
#' @export
Scfd <- function(X, Y, basis, t = NULL) {
  Xm <- .s03mat(X)
  yy <- .s03vec(Y)
  B <- .s03mat(basis)
  N <- nrow(Xm)
  if (N == 0L) stop("scalar_on_function: X is empty")
  if (length(yy) != N) stop("scalar_on_function: Y must have one value per curve")
  T_ <- ncol(Xm)
  if (nrow(B) != T_) stop("scalar_on_function: basis must have one row per argument value")
  K <- ncol(B)
  if (K == 0L) stop("scalar_on_function: basis has no columns")
  if (T_ < 2L) stop("scalar_on_function: need at least two argument values")
  if (N <= K) stop("scalar_on_function: need more curves than basis functions")
  tt <- if (is.null(t)) .fdgrid(T_) else .s03vec(t)
  if (length(tt) != T_) stop("scalar_on_function: t must match the number of argument values")
  J <- matrix(0, N, K)
  for (i in seq_len(N)) for (j in seq_len(K)) {
    J[i, j] <- .fdtrapz(tt, B[, j] * Xm[i, ])
  }
  Z <- cbind(1, J)
  ab <- .s03lstsq(Z, yy, 0)
  alpha <- ab[1]
  b <- ab[-1]
  fit <- .s03matvec(Z, ab)
  res <- yy - fit
  sse <- sum(res * res)
  ybar <- .s03mean(yy)
  sst <- sum((yy - ybar) * (yy - ybar))
  beta <- numeric(T_)
  for (p in seq_len(T_)) {
    s <- 0
    for (j in seq_len(K)) s <- s + b[j] * B[p, j]
    beta[p] <- s
  }
  list(estimate = alpha, alpha = alpha, coef = b, beta = beta, J = J,
       fitted = fit, residual = res, sse = sse,
       r2 = if (sst > 0) 1 - sse / sst else NaN,
       df = N - K - 1L, n = N,
       method = "Reiss-Ogden (2007) / Ramsay-Silverman (2005) Ch.15 scalar-on-function regression by basis expansion of beta")
}

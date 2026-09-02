# SPDX-License-Identifier: AGPL-3.0-or-later
#' Orthogonalised impulse response function of a VAR(p)
#'
#' The MA representation y_t = sum_h Phi_h u_{t-h} is built from the
#' recursion Phi_0 = I, Phi_h = sum_{j=1}^{min(h,p)} A_j Phi_{h-j}, and the
#' shocks are orthogonalised by the lower Cholesky factor P of Sigma_u,
#' Theta_h = Phi_h P. Column \code{shock_var} of Theta_h is the response
#' to a one-standard-deviation shock in that variable.
#'
#' R mirror of \code{morie.fn.irfun}; the Python arm carries the primary
#' documentation.
#'
#' @param coef m by (1 + m p) coefficient matrix, row = equation, columns
#'   = \[intercept, A_1 row, ..., A_p row\] -- the layout \code{Varest}
#'   returns.
#' @param sigma_u m by m residual covariance.
#' @param horizon Periods ahead.
#' @param shock_var 0-based index of the shocked variable.
#' @return List with \code{irf}, \code{Phi}, \code{chol}, \code{horizon},
#'   \code{shock_var}.
#' @references Lutkepohl, H. (2005). New Introduction to Multiple Time
#'   Series Analysis. Springer, Ch. 2.3. doi:10.1007/978-3-540-27752-1.
#'   Sims, C.A. (1980). Econometrica 48(1):1-48. doi:10.2307/1912017.
#' @examples
#' Irfun(matrix(c(0, 0.5, 0.1, 0, 0.2, 0.4), 2, 3), diag(2), 3)$irf
#' @export
Irfun <- function(coef, sigma_u, horizon = 20, shock_var = 0) {
  B <- .s03mat(coef)
  S <- .s03mat(sigma_u)
  m <- nrow(B)
  p <- (ncol(B) - 1L) %/% m
  if (nrow(S) != m || ncol(S) != m) stop("sigma_u must be m by m")
  shock_var <- as.integer(shock_var)
  if (shock_var < 0L || shock_var >= m) stop("shock_var out of range")
  horizon <- as.integer(horizon)
  if (is.na(horizon) || horizon < 0L) stop("horizon must be non-negative")
  A <- vector("list", p)
  for (j in seq_len(p)) A[[j]] <- B[, (1L + (j - 1L) * m + 1L):(1L + j * m), drop = FALSE]
  Phi <- vector("list", horizon + 1L)
  Phi[[1L]] <- diag(m)
  if (horizon >= 1L) for (h in seq_len(horizon)) {
    acc <- matrix(0, m, m)
    for (j in seq_len(min(h, p))) acc <- acc + .s03matmul(A[[j]], Phi[[h - j + 1L]])
    Phi[[h + 1L]] <- acc
  }
  P <- .s03chol(S)
  irf <- matrix(0, horizon + 1L, m)
  for (h in seq_len(horizon + 1L)) irf[h, ] <- .s03matmul(Phi[[h]], P)[, shock_var + 1L]
  PhiA <- array(0, c(horizon + 1L, m, m))
  for (h in seq_len(horizon + 1L)) PhiA[h, , ] <- Phi[[h]]
  .t1_result(estimate = if (horizon >= 1L) irf[2L, shock_var + 1L] else irf[1L, shock_var + 1L],
             irf = irf, Phi = PhiA, chol = P, horizon = horizon,
             shock_var = shock_var,
             method = "Orthogonalised impulse response, Cholesky identification")
}

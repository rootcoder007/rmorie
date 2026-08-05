# SPDX-License-Identifier: AGPL-3.0-or-later
#' Forecast error variance decomposition from a VAR(1)
#'
#' The fraction of the h-step forecast error variance of each variable
#' attributable to each orthogonalised shock. With P the lower Cholesky
#' factor of Sigma_u and Theta_s = A^s the MA coefficients of a VAR(1),
#' the h-step MSE contribution of shock j to variable i is
#' sum_{s=0}^{h} (Theta_s P)[i, j]^2, and the decomposition divides that
#' by the row total.
#'
#' R mirror of \code{morie.fn.fevdc}; the Python arm carries the primary
#' documentation.
#'
#' @param var_coefficients VAR(1) coefficient matrix A (k by k).
#' @param sigma_u Residual covariance matrix (k by k).
#' @param periods Forecast horizon.
#' @return List with \code{decomposition} (a (periods+1) by k by k array),
#'   \code{mse_contributions}, \code{periods}, \code{k}.
#' @references Lutkepohl, H. (2005). New Introduction to Multiple Time
#'   Series Analysis. Springer. doi:10.1007/978-3-540-27752-1.
#' @examples
#' Fevdc(matrix(c(0.5, 0.1, 0.2, 0.4), 2, 2), diag(2), 3)$decomposition[4, , ]
#' @export
Fevdc <- function(var_coefficients, sigma_u, periods = 20) {
  A <- .s03mat(var_coefficients)
  S <- .s03mat(sigma_u)
  if (nrow(A) != ncol(A)) stop("var_coefficients must be square")
  k <- nrow(A)
  if (nrow(S) != k || ncol(S) != k) stop("sigma_u must be k by k")
  periods <- as.integer(periods)
  if (is.na(periods) || periods < 0L) stop("periods must be non-negative")
  P <- .s03chol(S)
  Theta <- vector("list", periods + 1L)
  Theta[[1L]] <- diag(k)
  Ap <- diag(k)
  if (periods >= 1L) for (h in seq_len(periods)) {
    Ap <- .s03matmul(Ap, A)
    Theta[[h + 1L]] <- Ap
  }
  decomp <- array(0, c(periods + 1L, k, k))
  mse <- array(0, c(periods + 1L, k, k))
  for (h in seq_len(periods + 1L)) {
    contrib <- matrix(0, k, k)
    for (s in seq_len(h)) {
      TP <- .s03matmul(Theta[[s]], P)
      contrib <- contrib + TP^2
    }
    tot <- rowSums(contrib)
    for (j in seq_len(k)) for (i in seq_len(k)) {
      decomp[h, i, j] <- if (tot[i] > 0) contrib[i, j] / tot[i] else 0
    }
    mse[h, , ] <- contrib
  }
  .t1_result(estimate = decomp[periods + 1L, 1L, 1L], decomposition = decomp,
             mse_contributions = mse, periods = periods, k = k,
             method = "Forecast error variance decomposition, VAR(1)")
}

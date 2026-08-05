# SPDX-License-Identifier: AGPL-3.0-or-later
#' VAR(p) vector autoregression by multivariate least squares
#'
#' SOURCE. Lutkepohl, H. (2005), New Introduction to Multiple Time Series
#' Analysis, Springer; doi:10.1007/978-3-540-27752-1. The model is
#' Eq. (2.1.1)/(3.2.1), y_t = nu + A_1 y_{t-1} + ... + A_p y_{t-p} + u_t,
#' and the estimator is the multivariate least squares estimator of
#' Section 3.2. Because every equation carries the same regressor set
#' Z_t = (1, y_{t-1}', ..., y_{t-p}')', the LS estimator separates
#' equation by equation into B = (Z'Z)^{-1} Z'Y and no SUR/GLS step is
#' needed.
#'
#' Two residual covariances are reported: the ML plug-in U'U/T and
#' U'U/(T - Kp - 1). The Gaussian log-likelihood (Eq. 3.4.5) is
#' evaluated at the former, and the three order-selection criteria are
#' log|Sigma-tilde| + c(T) * (free parameters)/T.
#'
#' NOT read from the book's own page images -- Lutkepohl (2005) is not in
#' the local corpus. The estimator is the standard multivariate LS form,
#' anchored against stats::lm reached by a different route.
#'
#' @param Y T-by-K matrix of observations; a plain vector is one series.
#' @param p Lag order, at least 1.
#' @param intercept Include the constant nu.
#' @return List with \code{coef}, \code{sigma_u}, \code{sigma_ml},
#'   \code{resid}, \code{loglik}, \code{aic}, \code{bic}, \code{hq},
#'   \code{logdet}, \code{n}, \code{k}, \code{p}.
#' @references Lutkepohl, H. (2005). New Introduction to Multiple Time
#'   Series Analysis. Springer. doi:10.1007/978-3-540-27752-1.
#' @examples
#' Varest(cbind(c(1, 2, 3, 4, 5), c(2, 1, 4, 3, 6)), 1)$loglik
#' @export
Varest <- function(Y, p = 1, intercept = TRUE) {
  A <- .s03mat(Y)
  if (nrow(A) == 0L) stop("vector_autoregression: Y is empty")
  p <- as.integer(p)
  if (is.na(p) || p < 1L) stop("vector_autoregression: p must be at least 1")
  K <- ncol(A)
  Tn <- nrow(A)
  n <- Tn - p
  if (n <= 0L) stop("vector_autoregression: not enough observations for p lags")
  Z <- matrix(0, n, (if (intercept) 1L else 0L) + K * p)
  Ye <- matrix(0, n, K)
  for (t in seq_len(n)) {
    tt <- t + p
    row <- if (intercept) 1 else numeric(0)
    for (i in seq_len(p)) row <- c(row, A[tt - i, ])
    Z[t, ] <- row
    Ye[t, ] <- A[tt, ]
  }
  m <- ncol(Z)
  if (n <= m) stop("vector_autoregression: fewer observations than regressors")
  ZtZ <- .s03crossprod(Z)
  Zt <- t(Z)
  coef <- matrix(0, K, m)
  for (j in seq_len(K)) {
    coef[j, ] <- .s03cholsolve(ZtZ, .s03matvec(Zt, Ye[, j]))
  }
  resid <- matrix(0, n, K)
  for (t in seq_len(n)) {
    for (j in seq_len(K)) {
      s <- 0
      for (q in seq_len(m)) s <- s + Z[t, q] * coef[j, q]
      resid[t, j] <- Ye[t, j] - s
    }
  }
  S <- matrix(0, K, K)
  for (i in seq_len(K)) {
    for (j in seq_len(K)) {
      s <- 0
      for (t in seq_len(n)) s <- s + resid[t, i] * resid[t, j]
      S[i, j] <- s
    }
  }
  sigma_ml <- S / n
  sigma_u <- S / (n - m)
  L <- .s03chol(sigma_ml)
  logdet <- 0
  for (i in seq_len(K)) logdet <- logdet + 2 * log(L[i, i])
  loglik <- -0.5 * n * K * log(2 * pi) - 0.5 * n * logdet - 0.5 * n * K
  npar <- K * m
  .t1_result(estimate = loglik, coef = coef, sigma_u = sigma_u,
             sigma_ml = sigma_ml, resid = resid, loglik = loglik,
             aic = logdet + 2 * npar / n,
             bic = logdet + log(n) * npar / n,
             hq = logdet + 2 * log(log(n)) * npar / n,
             logdet = logdet, n = n, k = K, p = p,
             method = "VAR(p) multivariate LS, Lutkepohl (2005) Sec. 3.2")
}

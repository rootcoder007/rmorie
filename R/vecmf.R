# SPDX-License-Identifier: AGPL-3.0-or-later

#' Vector error-correction model (VECM)
#'
#' Native Johansen (1991) reduced-rank maximum-likelihood estimation:
#' concentrate out the short-run dynamics, solve the generalized
#' eigenvalue problem on the canonical-correlation matrices, and take
#' the leading \code{coint_rank} eigenvectors as the cointegrating
#' space. Replaces the \code{urca::ca.jo} + \code{vars::vec2var}
#' delegation; the eigenvalues/vectors are cross-validated against
#' urca in tests.
#'
#' @param Y Numeric matrix (T x k) of I(1) candidate series.
#' @param k_ar Number of lagged differences. Default 1.
#' @param coint_rank Cointegration rank. Default 1.
#' @return Named list with \code{alpha, beta, Gamma, Sigma, eigenvalues,
#'   loglik, n, k, rank, method}.
#' @examples
#' morie_vecm(Y = matrix(rnorm(100), 50, 2))
#' @export
morie_vecm <- function(Y, k_ar = 1, coint_rank = 1) {
  Y <- as.matrix(Y)
  if (nrow(Y) < ncol(Y)) Y <- t(Y)
  Tt <- nrow(Y)
  k <- ncol(Y)
  if (Tt < 20 || k < 2 || coint_rank < 1 || coint_rank > k) {
    stop("Need T>=20, 1<=rank<=k.")
  }
  if (is.null(colnames(Y))) colnames(Y) <- paste0("y", seq_len(k))

  # Johansen setup (ecdet = "none" convention, K = k_ar + 1 VAR lags):
  # Z0 = dY_t, Z1 = Y_{t-K} (long-run levels), Z2 = lagged differences.
  K <- max(k_ar + 1L, 2L)
  dY <- diff(Y)
  rows <- nrow(dY) - K + 1L
  idx <- K:nrow(dY)
  Z0 <- dY[idx, , drop = FALSE]
  Z1 <- Y[idx - K + 1L, , drop = FALSE]
  # Short-run regressors: lagged differences PLUS the unrestricted
  # constant urca::ca.jo includes even under ecdet = "none".
  Z2 <- cbind(1, do.call(cbind, lapply(seq_len(K - 1L), function(i) {
    dY[idx - i, , drop = FALSE]
  })))

  # Concentrate out Z2: residuals of Z0 and Z1 on Z2.
  resid_on <- function(A, B) {
    if (is.null(B) || ncol(B) == 0L) return(A)
    A - B %*% solve(crossprod(B), crossprod(B, A))
  }
  R0 <- resid_on(Z0, Z2)
  R1 <- resid_on(Z1, Z2)
  n_eff <- nrow(R0)
  S00 <- crossprod(R0) / n_eff
  S01 <- crossprod(R0, R1) / n_eff
  S11 <- crossprod(R1) / n_eff

  # Generalized eigenproblem: |lambda*S11 - S10 S00^-1 S01| = 0,
  # solved via the symmetric transform S11^{-1/2} S10 S00^{-1} S01 S11^{-1/2}.
  e11 <- eigen(S11, symmetric = TRUE)
  S11_isqrt <- e11$vectors %*% diag(1 / sqrt(pmax(e11$values, 1e-12)),
                                    k) %*% t(e11$vectors)
  M <- S11_isqrt %*% t(S01) %*% solve(S00, S01) %*% S11_isqrt
  eM <- eigen((M + t(M)) / 2, symmetric = TRUE)
  lambda <- pmin(pmax(eM$values, 0), 1 - 1e-12)
  V <- S11_isqrt %*% eM$vectors # eigenvectors in the original scale

  # Johansen normalization: v' S11 v = I already holds by construction.
  beta <- V[, seq_len(coint_rank), drop = FALSE]
  alpha <- S01 %*% beta # since beta' S11 beta = I
  rownames(beta) <- colnames(Y)
  rownames(alpha) <- colnames(Y)

  # Short-run coefficients given the error-correction term.
  ect <- Z1 %*% beta
  X <- cbind(ect, Z2)
  B <- solve(crossprod(X), crossprod(X, Z0))
  eps <- Z0 - X %*% B
  Sigma <- crossprod(eps) / max(n_eff - ncol(X), 1)
  Gamma <- if (K > 1L) {
    lapply(seq_len(K - 1L), function(i) {
      # +1 skips the unrestricted-constant row of B.
      t(B[coint_rank + 1L + ((i - 1L) * k + 1L):(i * k), , drop = FALSE])
    })
  } else {
    list()
  }

  # Gaussian loglik at the reduced-rank solution (Johansen 1991).
  ll <- -n_eff * k / 2 * (log(2 * pi) + 1) -
    n_eff / 2 * (log(det(S00)) +
                   sum(log(1 - lambda[seq_len(coint_rank)])))

  list(
    alpha = alpha, beta = beta, Gamma = Gamma, Sigma = Sigma,
    eigenvalues = lambda,
    loglik = ll,
    n = Tt, k = k, rank = coint_rank,
    method = "VECM via native Johansen reduced-rank ML"
  )
}

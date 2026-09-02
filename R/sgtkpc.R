# SPDX-License-Identifier: AGPL-3.0-or-later
#' Kernel principal component analysis
#'
#' Schoelkopf, Smola and Mueller (1998), Nonlinear component analysis as a
#' kernel eigenvalue problem, Neural Computation 10(5), 1299-1319.  The
#' Gram matrix is centred in feature space by their equation (21), Ktilde
#' = K - 1_n K - K 1_n + 1_n K 1_n with 1_n all entries 1/n; the
#' components are the eigenvectors alpha^k normalised so lambda_k
#' <alpha^k, alpha^k> = 1, and the projection is sum_i alpha^k_i
#' Ktilde(x_i, x).  The paper is paywalled; the centring identity and the
#' normalisation are quoted in their standard published form.
#' Eigenvectors are sign-fixed before projection.
#'
#' @param X data matrix, one row per point, or a precomputed Gram matrix
#'   passed as `kernel`.
#' @param kernel "linear", "poly", "rbf", or a Gram matrix.
#' @param k number of components.
#' @param gamma,degree,coef0 kernel parameters.
#' @return list: Y, eigvals, estimate, explained, method.
#' @keywords internal
#' @examples
#' Kernelpca(matrix(c(0, 0, 1, 1, 2, 0), 3, 2, byrow = TRUE), "linear", 2)$eigvals
#' @export
Kernelpca <- function(X, kernel = "rbf", k = 2, gamma = 1, degree = 2,
                      coef0 = 1) {
  Xm <- .s03mat(X)
  n <- nrow(Xm)
  gram <- function(Xm) {
    K <- matrix(0, n, n)
    for (i in seq_len(n)) for (j in seq_len(n)) {
      if (identical(kernel, "linear")) {
        s <- 0
        for (a in seq_len(ncol(Xm))) s <- s + Xm[i, a] * Xm[j, a]
      } else if (identical(kernel, "poly")) {
        s <- 0
        for (a in seq_len(ncol(Xm))) s <- s + Xm[i, a] * Xm[j, a]
        s <- (gamma * s + coef0)^degree
      } else {
        s <- 0
        for (a in seq_len(ncol(Xm))) { dd <- Xm[i, a] - Xm[j, a]
        s <- s + dd * dd }
        s <- exp(-gamma * s)
      }
      K[i, j] <- s
    }
    K
  }
  K <- if (is.character(kernel)) gram(Xm) else .s03mat(kernel)
  rm_ <- numeric(n)
  for (i in seq_len(n)) { s <- 0
  for (j in seq_len(n)) s <- s + K[i, j]
  rm_[i] <- s / n }
  gm <- 0
  for (v in rm_) gm <- gm + v / n
  Kt <- matrix(0, n, n)
  for (i in seq_len(n)) for (j in seq_len(n)) Kt[i, j] <- K[i, j] - rm_[i] - rm_[j] + gm
  eg <- .s03jacobi(Kt)
  vals <- eg$values
  vecs <- eg$vectors
  kk <- as.integer(k)
  if (kk > n) kk <- n
  ev <- numeric(kk)
  for (t in seq_len(kk)) ev[t] <- vals[n - t + 1L]
  Y <- matrix(0, n, kk)
  for (t in seq_len(kk)) {
    col <- vecs[, n - t + 1L]
    lam <- ev[t]
    scale <- if (lam > 1e-12) 1 / sqrt(lam) else 0
    a <- col * scale
    for (i in seq_len(n)) {
      s <- 0
      for (j in seq_len(n)) s <- s + a[j] * Kt[i, j]
      Y[i, t] <- s
    }
  }
  tot <- 0
  for (v in vals) if (v > 0) tot <- tot + v
  list(Y = Y, eigvals = ev, estimate = if (kk) ev[1] else NaN,
       explained = if (tot > 0) ev / tot else rep(NaN, kk),
       method = "Kernel PCA on the centred Gram matrix (Schoelkopf et al. 1998, eq. 21)")
}

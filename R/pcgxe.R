# SPDX-License-Identifier: AGPL-3.0-or-later
#' Principal component based GxE dimension reduction
#'
#' Montesinos Lopez, Montesinos Lopez and Crossa (2022), Multivariate
#' Statistical Machine Learning Methods for Genomic Prediction, Springer,
#' volume \[Pages 35-70\], Chapter 2, Section 2.8, pp. 63-68.  The section
#' gives PC = XW, "where W is a p-by-p matrix of weights whose columns are
#' the eigenvectors of Q = X'X, that is, we first need to calculate the
#' eigenvalue decomposition of Q, which is equal to Q = W Lambda W'"; the
#' compressed matrix is X* = XW*, "where W* contains the same rows of W, but
#' only the first k columns".  It adds that the components may come from the
#' covariance matrix Q = 1/(n-1) X'X with centred columns, or from the
#' correlation matrix with standardised columns.  Its worked example prints
#' the component standard deviations and the leading scores for the fifteen
#' lines of Table 2.13; those printed numbers are the anchor.
#'
#' The SVD form GxE = U_k D_k V_k' is the same decomposition: the
#' eigenvectors of X'X are V and the scores XV are U D.
#'
#' @param GxE_matrix n-by-p matrix; columns are centred, optionally scaled.
#' @param k number of components to retain, 1 <= k <= p.
#' @param scale divide each centred column by its standard deviation.
#' @return list: estimate, scores, loadings, sdev, eigenvalues, GxE_approx,
#'   center, scale, n, method.
#' @keywords internal
#' @examples
#' Pcgxe(matrix(c(1, 2, 3, 2, 4, 7), 3, 2), 1)$estimate
#' @export
Pcgxe <- function(GxE_matrix, k, scale = FALSE) {
  X <- .s03mat(GxE_matrix)
  n <- nrow(X)
  if (n < 2L) stop("pc_gxe_reduction: need at least two rows")
  p <- ncol(X)
  kk <- as.integer(k)
  if (is.na(kk) || kk < 1L || kk > p) {
    stop("pc_gxe_reduction: k must lie between 1 and the number of columns")
  }
  mu <- numeric(p)
  for (j in seq_len(p)) {
    s <- 0
    for (i in seq_len(n)) s <- s + X[i, j]
    mu[j] <- s / n
  }
  C <- matrix(0, n, p)
  for (i in seq_len(n)) for (j in seq_len(p)) C[i, j] <- X[i, j] - mu[j]
  sc <- rep(1, p)
  if (isTRUE(scale)) {
    for (j in seq_len(p)) {
      s <- 0
      for (i in seq_len(n)) s <- s + C[i, j] * C[i, j]
      s <- sqrt(s / (n - 1L))
      if (s <= 0) stop("pc_gxe_reduction: a column has zero variance and cannot be scaled")
      sc[j] <- s
      for (i in seq_len(n)) C[i, j] <- C[i, j] / s
    }
  }
  Q <- matrix(0, p, p)
  for (a in seq_len(p)) for (b in seq_len(p)) {
    s <- 0
    for (i in seq_len(n)) s <- s + C[i, a] * C[i, b]
    Q[a, b] <- s / (n - 1L)
  }
  eg <- .s03jacobi(Q)
  ord <- rev(seq_len(p))
  ev <- eg$values[ord]
  W <- eg$vectors[, ord[seq_len(kk)], drop = FALSE]
  scores <- matrix(0, n, kk)
  for (i in seq_len(n)) for (j in seq_len(kk)) {
    s <- 0
    for (a in seq_len(p)) s <- s + C[i, a] * W[a, j]
    scores[i, j] <- s
  }
  approx <- matrix(0, n, p)
  for (i in seq_len(n)) for (a in seq_len(p)) {
    s <- 0
    for (j in seq_len(kk)) s <- s + scores[i, j] * W[a, j]
    approx[i, a] <- s
  }
  tot <- 0
  for (v in ev) tot <- tot + v
  top <- 0
  for (j in seq_len(kk)) top <- top + ev[j]
  list(estimate = if (tot > 0) top / tot else NaN,
       scores = scores, loadings = W,
       sdev = vapply(ev, function(v) if (v > 0) sqrt(v) else 0, 0),
       eigenvalues = ev, GxE_approx = approx, center = mu, scale = sc, n = n,
       method = "GxE = U_k D_k V_k' by the eigendecomposition of Q, Chapter 2 Sect. 2.8")
}

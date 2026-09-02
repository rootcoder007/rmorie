# SPDX-License-Identifier: AGPL-3.0-or-later
#' Two-way spectral partition by the sign of the Fiedler vector
#'
#' Fiedler (1973), Algebraic connectivity of graphs, Czechoslovak
#' Mathematical Journal 23(2), 298-305, identifies the eigenvector of the
#' second-smallest Laplacian eigenvalue as the one that orders a graph's
#' vertices; Shi and Malik (2000), IEEE TPAMI 22(8), 888-905, show that
#' the second-smallest generalised eigenvector of (D - W) y = lambda D y
#' is the real relaxation of the normalised cut and that thresholding it
#' gives the partition.  Neither was retrievable here as a full text; both
#' results are quoted in their standard published form.  The eigenproblem
#' is solved by cyclic Jacobi on the symmetric normalised Laplacian;
#' eigenvector signs are not determined by the eigenproblem, so the vector
#' is sign-fixed before thresholding -- without that the two arms would
#' return complementary partitions.
#'
#' @param A symmetric weight matrix.
#' @param normalized use the normalised Laplacian.
#' @return list: labels, estimate, fiedler, eigenvalues, n, method.
#' @keywords internal
#' @examples
#' A <- matrix(c(0, 1, 0, 1, 0, 1, 0, 1, 0), 3, 3)
#' Fiedlercut(A)$labels
#' @export
Fiedlercut <- function(A, normalized = TRUE) {
  W <- .s03mat(A)
  n <- nrow(W)
  d <- numeric(n)
  for (i in seq_len(n)) { s <- 0
  for (j in seq_len(n)) s <- s + W[i, j]
  d[i] <- s }
  L <- matrix(0, n, n)
  for (i in seq_len(n)) for (j in seq_len(n)) {
    L[i, j] <- (if (i == j) d[i] else 0) - W[i, j]
  }
  if (normalized) {
    for (i in seq_len(n)) for (j in seq_len(n)) {
      di <- if (d[i] > 0) sqrt(d[i]) else 0
      dj <- if (d[j] > 0) sqrt(d[j]) else 0
      L[i, j] <- if (di > 0 && dj > 0) L[i, j] / (di * dj) else 0
    }
  }
  eg <- .s03jacobi(L)
  vals <- eg$values
  vecs <- eg$vectors
  fied <- if (n > 1L) vecs[, 2] else rep(0, n)
  if (normalized) {
    for (i in seq_len(n)) fied[i] <- if (d[i] > 0) fied[i] / sqrt(d[i]) else 0
    big <- 1L
    for (i in seq_len(n)) if (abs(fied[i]) > abs(fied[big]) + 1e-15) big <- i
    if (fied[big] < 0) fied <- -fied
  }
  labels <- as.integer(fied >= 0)
  list(labels = labels, estimate = if (n > 1L) vals[2] else NaN,
       fiedler = fied, eigenvalues = vals, n = n,
       method = "Two-way spectral partition by the sign of the Fiedler vector")
}

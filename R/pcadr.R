# SPDX-License-Identifier: AGPL-3.0-or-later
#' Principal component compression of a marker matrix
#'
#' Formula: Q = X'X/(n-1) on scaled columns; W the eigenvectors of Q; PC = X W; keep the first k columns
#'
#' @param X One record per row.
#' @param k Number of components retained; None keeps all.
#'
#' @return List with ``scores``, ``loadings``, ``eigenvalues``, ``compressed``, ``prop_variance``, ``cum_variance``, ``k``, ``n``, ``p``.
#' @references Montesinos Lopez, Montesinos Lopez and Crossa (2022), Multivariate Statistical Machine Learning Methods for Genomic Prediction, Springer, doi:10.1007/978-3-030-89010-0.  Chapter 2, Sect. 2.8 pp. 63-64.  Delegates to the chapter routine in morie.fn._gp_core, which was verified against this book in the earlier tranches of this shelf recorded in ledger/SHELF_LEDGER.txt; the page and equation number above are that routine's own, re-read against the chapter PDF here.  DEFECT FOUND: neither shared core sign-fixes its eigenvectors, so morie.fn._gp_core.pca_compress and the R morie_pca return loadings and scores that differ by a column sign; the three-way parity harness caught it.  The sign is fixed HERE, in the same way morie.fn._tail1core.eigsym does it -- the largest-magnitude entry of every loading column is made positive -- rather than in the shared cores, which this slice must not edit.  With that, a repeated eigenvalue leaves its loadings determined only up to a rotation within the eigenspace and is not a stable quantity in either language.
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' Pcadim(V)
Pcadim <- function(X, k = NULL) {
  out <- morie_pca(X, k = k)
  Xm <- .t1_mat(X)
  n <- nrow(Xm)
  p <- ncol(Xm)
  kk <- if (is.null(k)) p else as.integer(k)
  if (kk < 1L || kk > p)
    stop("k must lie between 1 and the number of columns of X")
  W <- out$loadings
  PC <- out$scores
  for (j in seq_len(p)) {
    r <- which.max(abs(W[, j]))
    if (W[r, j] < 0) { W[, j] <- -W[, j]
    PC[, j] <- -PC[, j] }
  }
  .t1_result(scores = PC, loadings = W, eigenvalues = out$eigenvalues,
             compressed = PC[, seq_len(kk), drop = FALSE],
             prop_variance = out$prop_variance, cum_variance = out$cum_variance,
             k = kk, n = n, p = p,
             method = "PCA compression, MVSML Sect. 2.8")
}

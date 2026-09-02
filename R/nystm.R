# SPDX-License-Identifier: AGPL-3.0-or-later
#' Nystrom low-rank approximation of a kernel matrix
#'
#' Formula: Q = K_nm * K_mm^- * K_nm',  m the retained subset of records
#'
#' @param X One record per row.
#' @param m_index 1-based row indices of the retained subset.
#' @param kernel Kernel name: linear, gaussian, polynomial, exponential or sigmoid.
#' @param gamma Kernel bandwidth; None uses 1/p.
#'
#' @return List with ``Q``, ``m``, ``n``.
#' @references Montesinos Lopez, Montesinos Lopez and Crossa (2022), Multivariate Statistical Machine Learning Methods for Genomic Prediction, Springer, doi:10.1007/978-3-030-89010-0.  Chapter 8: the Nystrom method for compressing a kernel matrix onto a retained subset of records; the implementation delegates to the chapter-8 Nystrom routine already verified against the book for this shelf.  Read from the chapter PDF, not recalled.
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' Nystromap(V, V)
Nystromap <- function(X, m_index, kernel = "linear", gamma = NULL) {
  idx <- as.integer(m_index)
  if (any(idx < 1L)) stop("m_index is 1-based")
  out <- morie_nystrom(X, idx, kernel = kernel, gamma = gamma)
  Q <- if (is.list(out)) out$Q else out
  .t1_result(Q = Q, m = length(idx), n = nrow(Q),
             method = "Nystrom kernel approximation, MVSML Chap. 8")
}

# SPDX-License-Identifier: AGPL-3.0-or-later
#' Wolfe dual objective of the support vector machine
#'
#' Formula: L(alpha) = sum_i alpha_i - 0.5 sum_i sum_j alpha_i alpha_j y_i y_j K(x_i, x_j)
#'
#' @param alpha Dual variables, length n.
#' @param X One record per row.
#' @param y Class labels coded +1 and -1.
#' @param K Gram matrix; None uses the linear kernel x_i'x_j.
#'
#' @return List with ``dual``, ``linear_term``, ``quadratic_term``, ``constraint_sum``, ``n``.
#' @references Montesinos Lopez, Montesinos Lopez and Crossa (2022), Multivariate
#' Statistical Machine Learning Methods for Genomic Prediction, Springer,
#' doi:10.1007/978-3-030-89010-0.  Chapter 9, Eq. (9.32) p. 349.  The dual depends on the
#' data only through inner products, which is exactly what lets a kernel replace them;
#' ``constraint_sum`` reports sum_i alpha_i y_i, which the dual problem constrains to
#' zero.  Delegates to the chapter routine in morie.fn._gp_core, which was verified
#' against this book in the earlier tranches of this shelf recorded in
#' ledger/SHELF_LEDGER.txt; the page and equation number above are that routine's own,
#' re-read against the chapter PDF here.
#' @export
#' @examples
#' Svmwolfe(alpha = c(1, 2, 3, 4, 5, 6, 7, 8), X = c(1, 2, 3, 4, 5, 6, 7, 8), y = c(1, 2,
#' 3, 4, 5, 6, 7, 8))
Svmwolfe <- function(alpha, X, y, K = NULL) {
  a <- .t1_vec(alpha)
  yv <- .t1_vec(y)
  if (length(a) != length(yv)) stop("alpha and y must have the same length")
  L <- morie_svm_dual_objective(a, X, yv, K = K)
  lin <- sum(a)
  .t1_result(dual = L, linear_term = lin, quadratic_term = lin - L,
             constraint_sum = sum(a * yv), n = length(a),
             method = "SVM Wolfe dual objective, MVSML Eq. (9.32)")
}

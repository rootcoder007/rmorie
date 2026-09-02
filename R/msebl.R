# SPDX-License-Identifier: AGPL-3.0-or-later
#' Sum-of-squared-error loss for continuous outcomes
#'
#' Formula: L(w) = 0.5 * sum_i sum_j (yhat_ij - y_ij)^2
#'
#' @param Y Observed values, one row per record; a flat vector is read as one column.
#' @param Yhat Predicted values, same shape as the observed matrix.
#'
#' @return List with ``loss``, ``mean_loss``, ``n``, ``L``.
#' @references Montesinos Lopez, Montesinos Lopez and Crossa (2022), Multivariate Statistical Machine Learning Methods for Genomic Prediction, Springer, doi:10.1007/978-3-030-89010-0.  Chapter 10, Sect. 10.7, pp. 400-403.  Read from the chapter PDF, not recalled.  The book notes that dividing by two is for convenience in the backpropagation gradient, and that it is also common to report the SSE divided by n times L; ``mean_loss`` is that per-cell value.
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' Ssello(V, V)
Ssello <- function(Y, Yhat) {
  Y <- .t1_mat(Y)
  H <- .t1_mat(Yhat)
  if (nrow(Y) == 0L || nrow(Y) != nrow(H) || ncol(Y) != ncol(H))
    stop("Y and Yhat must be non-empty and the same shape")
  loss <- 0.5 * sum((H - Y)^2)
  .t1_result(loss = loss, mean_loss = loss / (nrow(Y) * ncol(Y)),
             n = nrow(Y), L = ncol(Y), method = "SSE loss, MVSML Sect. 10.7.1")
}

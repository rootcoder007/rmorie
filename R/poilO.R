# SPDX-License-Identifier: AGPL-3.0-or-later
#' Poisson loss for count outcomes.
#'
#' Formula: L(w) = sum_i sum_j [ yhat_ij - y_ij log(yhat_ij) ]
#'
#' @param Y Observed counts; a flat vector is read as one column.
#' @param Yhat Predicted Poisson means, strictly positive.
#'
#' @return List with ``loss``, ``mean_loss``, ``n``, ``L``.
#' @references Montesinos Lopez, Montesinos Lopez and Crossa (2022), Multivariate Statistical Machine Learning Methods for Genomic Prediction, Springer, doi:10.1007/978-3-030-89010-0.  Chapter 10, Sect. 10.7, pp. 400-403.  Read from the chapter PDF, not recalled.  The Poisson display carries its sign correctly and is implemented exactly as printed.
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' Poislo(V, V)
Poislo <- function(Y, Yhat) {
  Y <- .t1_mat(Y); H <- .t1_mat(Yhat)
  if (nrow(Y) == 0L || nrow(Y) != nrow(H) || ncol(Y) != ncol(H))
    stop("Y and Yhat must be non-empty and the same shape")
  if (any(H <= 0)) stop("predicted Poisson means must be strictly positive")
  loss <- sum(H - Y * log(H))
  .t1_result(loss = loss, mean_loss = loss / (nrow(Y) * ncol(Y)),
             n = nrow(Y), L = ncol(Y), method = "Poisson loss, MVSML Sect. 10.7.2")
}

# SPDX-License-Identifier: AGPL-3.0-or-later
#' Root mean squared error of a set of predictions.
#'
#' Formula: RMSE = sqrt((1/T) sum_i (y_i - yhat_i)^2)
#'
#' @param y Numeric vector.
#' @param yhat Predicted values, same length as y.
#'
#' @return List with ``rmse``, ``mse``, ``n``.
#' @references Montesinos Lopez, Montesinos Lopez and Crossa (2022), Multivariate Statistical Machine Learning Methods for Genomic Prediction, Springer, doi:10.1007/978-3-030-89010-0.  Chapter 4, Sect. 4.5.1, Eq. (4.1) p. 129 (MSE), Eq. (4.2) p. 129 (Pearson accuracy) and Eq. (4.3) p. 131 (MAE).  Read from the chapter PDF, not recalled.  The RMSE is named in the paragraph under Eq. (4.1) as the square root of MSE_TST.
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' Rmsetst(V, V)
Rmsetst <- function(y, yhat) {
  y <- as.numeric(y); yhat <- as.numeric(yhat)
  if (length(y) != length(yhat)) stop("y and yhat must have the same length")
  if (length(y) == 0L) stop("y must be non-empty")
  mse <- mean((y - yhat)^2)
  .t1_result(rmse = sqrt(mse), mse = mse, n = length(y),
             method = "Test-set root mean squared error, MVSML Sect. 4.5.1")
}

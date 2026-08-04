# SPDX-License-Identifier: AGPL-3.0-or-later
#' Mean absolute error of a set of predictions.
#'
#' Formula: MAE = (1/T) sum_i |y_i - yhat_i|
#'
#' @param y Numeric vector.
#' @param yhat Predicted values, same length as y.
#'
#' @return List with ``mae``, ``n``.
#' @references Montesinos Lopez, Montesinos Lopez and Crossa (2022), Multivariate Statistical Machine Learning Methods for Genomic Prediction, Springer, doi:10.1007/978-3-030-89010-0.  Chapter 4, Sect. 4.5.1, Eq. (4.1) p. 129 (MSE), Eq. (4.2) p. 129 (Pearson accuracy) and Eq. (4.3) p. 131 (MAE).  Read from the chapter PDF, not recalled.
#' @export
Maetst <- function(y, yhat) {
  y <- as.numeric(y); yhat <- as.numeric(yhat)
  if (length(y) != length(yhat)) stop("y and yhat must have the same length")
  if (length(y) == 0L) stop("y must be non-empty")
  .t1_result(mae = mean(abs(y - yhat)), n = length(y),
             method = "Test-set mean absolute error, MVSML Eq. (4.3)")
}

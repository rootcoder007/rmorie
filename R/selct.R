# SPDX-License-Identifier: AGPL-3.0-or-later
#' Genomic selection accuracy: Pearson correlation of observed and predicted.
#'
#' Formula: r = sum (yhat - mean yhat)(y - mean y) / sqrt(sum (yhat - mean yhat)^2 * sum (y - mean y)^2)
#'
#' @param y Numeric vector.
#' @param yhat Predicted values, same length as y.
#'
#' @return List with ``accuracy``, ``r2``, ``n``.
#' @references Montesinos Lopez, Montesinos Lopez and Crossa (2022), Multivariate Statistical Machine Learning Methods for Genomic Prediction, Springer, doi:10.1007/978-3-030-89010-0.  Chapter 4, Sect. 4.5.1, Eq. (4.1) p. 129 (MSE), Eq. (4.2) p. 129 (Pearson accuracy) and Eq. (4.3) p. 131 (MAE).  Read from the chapter PDF, not recalled.  Eq. (4.2) is Pearson's correlation between the T test-set predictions and the T observed values; the book calls it the prediction accuracy in plant breeding.
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' Predacc(V, V)
Predacc <- function(y, yhat) {
  y <- as.numeric(y); yhat <- as.numeric(yhat)
  if (length(y) != length(yhat)) stop("y and yhat must have the same length")
  n <- length(y)
  if (n < 2L) stop("need at least two observations")
  my <- sum(y) / n; mh <- sum(yhat) / n
  num <- sum((yhat - mh) * (y - my))
  dy <- sum((y - my)^2); dh <- sum((yhat - mh)^2)
  if (dy <= 0 || dh <= 0) stop("observed and predicted must both vary")
  r <- num / sqrt(dh * dy)
  .t1_result(accuracy = r, r2 = r * r, n = n,
             method = "Pearson prediction accuracy, MVSML Eq. (4.2)")
}

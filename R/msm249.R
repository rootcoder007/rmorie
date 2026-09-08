# SPDX-License-Identifier: AGPL-3.0-or-later
#' Sum-of-squares loss
#'
#' Formula: E = (1/2) sum_i sum_j (y-hat_ij - y_ij)^2 (eq. 10.5), the loss
#' whose partial derivatives with respect to the weights drive backpropagation.
#'
#' @param y_hat Predicted output matrix.
#' @param y Observed output matrix.
#' @return List with estimate, sse, method.
#' @references Montesinos Lopez, Montesinos Lopez & Crossa (2022),
#'   Multivariate Statistical Machine Learning Methods for Genomic Prediction,
#'   Springer, eq. (10.5) p.409. DOI 10.1007/978-3-030-89010-0.
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' Msm249(V, V)
Msm249 <- function(y_hat, y) {
  v <- .gpannsse(y_hat, y)
  list(estimate = v, sse = v, method = "SSE loss (MVSML 2022 eq. 10.5)")
}

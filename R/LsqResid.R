# SPDX-License-Identifier: AGPL-3.0-or-later

#' Residual sum at the least-squares optimum
#'
#' The residuals of the fitted line sum to zero, which is the dS/dB
#' normal equation.
#'
#' @param x,y equal-length data vectors, n >= 2.
#' @return list(residual_sum, A, B).
#' @references Morin, D. J. (2016). Probability: For the Enthusiastic
#'   Beginner. Createspace. Eq (6.92).
#' @examples
#' LsqResid(c(2, 3, 3, 5, 7), c(1, 1, 3, 4, 6))$residual_sum
#' @export
LsqResid <- function(x, y) {
  fit <- LsqFit(x, y)
  x <- as.numeric(x)
  y <- as.numeric(y)
  list(residual_sum = sum(y - (fit$A * x + fit$B)), A = fit$A, B = fit$B)
}

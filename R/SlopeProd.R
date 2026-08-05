# SPDX-License-Identifier: AGPL-3.0-or-later

#' Product of the forward and reverse regression slopes
#'
#' A is the slope of y on x, C the slope of x on y; A C = r^2 is
#' asserted, so a sign or divisor error fails loudly.
#'
#' @param x,y equal-length data vectors, n >= 2.
#' @return list(r, slope_product_AC).
#' @references Morin, D. J. (2016). Probability: For the Enthusiastic
#'   Beginner. Createspace. Eq (6.53).
#' @examples
#' SlopeProd(c(2, 3, 3, 5, 7), c(1, 1, 3, 4, 6))$slope_product_AC
#' @export
SlopeProd <- function(x, y) {
  value <- SampR(x, y)$r
  a_slope <- LsqFit(x, y)$A
  c_slope <- LsqFit(y, x)$A
  if (abs(a_slope * c_slope - value * value) > 1e-9 * max(1, value * value)) {
    stop("A*C != r^2.", call. = FALSE)
  }
  list(r = value, slope_product_AC = a_slope * c_slope)
}

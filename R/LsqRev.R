# SPDX-License-Identifier: AGPL-3.0-or-later

#' Reverse least-squares line
#'
#' The least-squares line of x on y.
#'
#' @param x,y equal-length data vectors, n >= 2.
#' @return list(C, D, S).
#' @references Morin, D. J. (2016). Probability: For the Enthusiastic
#'   Beginner. Createspace. Eq (6.50).
#' @examples
#' LsqRev(c(2, 3, 3, 5, 7), c(1, 1, 3, 4, 6))$C
#' @export
LsqRev <- function(x, y) {
  r <- LsqFit(y, x)
  list(C = r$A, D = r$B, S = r$S)
}

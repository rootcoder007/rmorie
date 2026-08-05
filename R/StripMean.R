# SPDX-License-Identifier: AGPL-3.0-or-later

#' Strip mean from the reverse regression line
#'
#' The mean of X within the strip at y0 is the reverse regression
#' slope times y0.
#'
#' @param r correlation, in [-1, 1].
#' @param sigma_x spread of X, >= 0.
#' @param sigma_y spread of Y, > 0.
#' @param y0 the y value defining the strip.
#' @return list(x, slope).
#' @references Morin, D. J. (2016). Probability: For the Enthusiastic
#'   Beginner. Createspace. Eq (6.74).
#' @examples
#' StripMean(0.5, 15, 15, 20)$x
#' @export
StripMean <- function(r, sigma_x, sigma_y, y0) {
  slope <- RevSlope(r, sigma_x, sigma_y)$slope
  y0 <- as.numeric(y0)
  if (length(y0) != 1L || is.na(y0)) stop("y0 must be a single value.", call. = FALSE)
  list(x = slope * y0, slope = slope)
}

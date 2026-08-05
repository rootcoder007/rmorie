# SPDX-License-Identifier: AGPL-3.0-or-later

#' Reverse regression slope
#'
#' r sigma_x / sigma_y, the slope of X regressed on Y.
#'
#' @param r correlation, in [-1, 1].
#' @param sigma_x spread of X, >= 0.
#' @param sigma_y spread of Y, > 0.
#' @return list(slope).
#' @references Morin, D. J. (2016). Probability: For the Enthusiastic
#'   Beginner. Createspace. Eq (6.36).
#' @examples
#' RevSlope(0.5, 15, 15)$slope
#' @export
RevSlope <- function(r, sigma_x, sigma_y) {
  r <- as.numeric(r)
  sigma_x <- as.numeric(sigma_x)
  sigma_y <- as.numeric(sigma_y)
  if (length(r) != 1L || is.na(r) || r < -1 || r > 1) {
    stop("r must be a single value in [-1, 1].", call. = FALSE)
  }
  if (length(sigma_y) != 1L || is.na(sigma_y) || sigma_y <= 0 ||
        length(sigma_x) != 1L || is.na(sigma_x) || sigma_x < 0) {
    stop("sigmas must be positive.", call. = FALSE)
  }
  list(slope = r * sigma_x / sigma_y)
}

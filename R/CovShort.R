# SPDX-License-Identifier: AGPL-3.0-or-later

#' Covariance by the shortcut formula
#'
#' mean(xy) - mean(x) mean(y), cross-checked against the deviation form.
#'
#' @param x,y equal-length data vectors, n >= 2.
#' @return list(cov).
#' @references Morin, D. J. (2016). Probability: For the Enthusiastic
#'   Beginner. Createspace. Eq (6.14).
#' @examples
#' CovShort(c(2, 3, 3, 5, 7), c(1, 1, 3, 4, 6))$cov
#' @export
CovShort <- function(x, y) {
  x <- as.numeric(x)
  y <- as.numeric(y)
  if (length(x) != length(y) || length(x) < 2L || any(is.na(x)) || any(is.na(y))) {
    stop("x and y must be equal-length vectors, n >= 2.", call. = FALSE)
  }
  direct <- mean((x - mean(x)) * (y - mean(y)))
  shortcut <- mean(x * y) - mean(x) * mean(y)
  if (abs(direct - shortcut) > 1e-9 * max(1, abs(direct))) {
    stop("covariance forms disagree.", call. = FALSE)
  }
  list(cov = shortcut)
}

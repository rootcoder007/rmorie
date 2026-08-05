# SPDX-License-Identifier: AGPL-3.0-or-later

#' Covariance of zero-mean data
#'
#' Refuses data whose means are not zero: use the shortcut form
#' instead.
#'
#' @param x,y equal-length zero-mean data vectors.
#' @return list(cov).
#' @references Morin, D. J. (2016). Probability: For the Enthusiastic
#'   Beginner. Createspace. Eq (6.8).
#' @examples
#' CovZMean(c(-1, 0, 1), c(-2, 0, 2))$cov
#' @export
CovZMean <- function(x, y) {
  x <- as.numeric(x)
  y <- as.numeric(y)
  if (length(x) != length(y) || length(x) == 0L || any(is.na(x)) || any(is.na(y))) {
    stop("x and y must be equal-length, non-empty.", call. = FALSE)
  }
  if (abs(mean(x)) > 1e-9 || abs(mean(y)) > 1e-9) {
    stop("this form needs zero-mean data; use eq (6.14) otherwise.", call. = FALSE)
  }
  list(cov = mean(x * y))
}

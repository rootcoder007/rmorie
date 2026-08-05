# SPDX-License-Identifier: AGPL-3.0-or-later

#' Means of the book's five-point data set
#'
#' Defaults to the book's five worked points, whose means are 4 and 3.
#'
#' @param x,y data vectors; default to the book's five worked points.
#' @return list(xbar, ybar).
#' @references Morin, D. J. (2016). Probability: For the Enthusiastic
#'   Beginner. Createspace. Eq (6.83).
#' @examples
#' BookMeans()$xbar
#' @export
BookMeans <- function(x = NULL, y = NULL) {
  if (is.null(x)) x <- c(2, 3, 3, 5, 7)
  if (is.null(y)) y <- c(1, 1, 3, 4, 6)
  x <- as.numeric(x)
  y <- as.numeric(y)
  if (length(x) == 0L || length(y) == 0L || any(is.na(x)) || any(is.na(y))) {
    stop("x and y must be non-empty.", call. = FALSE)
  }
  list(xbar = mean(x), ybar = mean(y))
}

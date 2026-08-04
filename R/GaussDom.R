# SPDX-License-Identifier: AGPL-3.0-or-later

#' Domain check for the Gaussian approximation
#'
#' well_inside is the decision abs(x)/sqrt(n) < 0.1.
#'
#' @param x deviation.
#' @param n number of trials, >= 1.
#' @return list(ratio, well_inside).
#' @references Morin, D. J. (2016). Probability: For the Enthusiastic
#'   Beginner. Createspace. Eq (7.5).
#' @examples
#' GaussDom(2, 10000)$well_inside
#' @export
GaussDom <- function(x, n) {
  x <- as.numeric(x)
  if (length(x) != 1L || is.na(x)) stop("x must be a single value.", call. = FALSE)
  if (length(n) != 1L || is.na(n) || n < 1 || n != as.integer(n)) {
    stop("n must be a single integer >= 1.", call. = FALSE)
  }
  ratio <- abs(x) / sqrt(as.integer(n))
  list(ratio = ratio, well_inside = ratio < 0.1)
}

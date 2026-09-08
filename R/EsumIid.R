# SPDX-License-Identifier: AGPL-3.0-or-later

#' Expectation of a sum of n i.i.d. variables
#'
#' E(X1 + ... + Xn) = n E(X).
#'
#' @param e_x the common expectation E(X).
#' @param n number of i.i.d. terms, >= 0.
#' @return list(e_sum, n).
#' @references Morin, D. J. (2016). Probability: For the Enthusiastic
#'   Beginner. Createspace. Eq (3.15).
#' @examples
#' EsumIid(3.5, 4)$e_sum
#' @export
EsumIid <- function(e_x, n) {
  e_x <- as.numeric(e_x)
  if (length(e_x) != 1L || is.na(e_x)) stop("e_x must be a single value.", call. = FALSE)
  if (length(n) != 1L || is.na(n) || n < 0 || n != as.integer(n)) {
    stop("n must be a non-negative integer.", call. = FALSE)
  }
  n <- as.integer(n)
  list(e_sum = n * e_x, n = n)
}

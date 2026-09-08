# SPDX-License-Identifier: AGPL-3.0-or-later

#' Taylor series for the exponential
#'
#' The truncated sum beside exp(x).
#'
#' @param x series argument.
#' @param terms number of terms, >= 1.
#' @return list(partial_sums, e_x, final_error).
#' @references Morin, D. J. (2016). Probability: For the Enthusiastic
#'   Beginner. Createspace. Eq (7.7).
#' @examples
#' ExpTaylor(1)$final_error
#' @export
ExpTaylor <- function(x, terms = 30) {
  x <- as.numeric(x)
  if (length(x) != 1L || is.na(x)) stop("x must be a single value.", call. = FALSE)
  if (length(terms) != 1L || is.na(terms) || terms < 1 ||
        terms != as.integer(terms)) {
    stop("terms must be a single integer >= 1.", call. = FALSE)
  }
  partial <- 0
  partials <- numeric(as.integer(terms))
  for (k in 0:(as.integer(terms) - 1L)) {
    partial <- partial + x^k / factorial(k)
    partials[k + 1L] <- partial
  }
  closed <- exp(x)
  list(partial_sums = partials, e_x = closed,
       final_error = abs(partials[length(partials)] - closed))
}

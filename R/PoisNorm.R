# SPDX-License-Identifier: AGPL-3.0-or-later

#' Poisson normalisation via the exponential series
#'
#' The truncated Taylor sum for e^a times e^-a, which is 1 when the
#' series has converged.
#'
#' @param a series argument.
#' @param terms number of Taylor terms, >= 0.
#' @return list(normalization, error).
#' @references Morin, D. J. (2016). Probability: For the Enthusiastic
#'   Beginner. Createspace. Eq (7.10).
#' @examples
#' PoisNorm(3)$error
#' @export
PoisNorm <- function(a, terms = 40) {
  a <- as.numeric(a)
  if (length(a) != 1L || is.na(a)) stop("a must be a single value.", call. = FALSE)
  if (length(terms) != 1L || is.na(terms) || terms < 1 ||
        terms != as.integer(terms)) {
    stop("terms must be a single integer >= 1.", call. = FALSE)
  }
  partial <- 0
  for (k in 0:(as.integer(terms) - 1L)) partial <- partial + a^k / factorial(k)
  total <- partial * exp(-a)
  list(normalization = total, error = abs(total - 1))
}

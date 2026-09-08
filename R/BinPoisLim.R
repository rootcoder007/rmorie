# SPDX-License-Identifier: AGPL-3.0-or-later

#' Binomial against its Poisson limit
#'
#' The binomial pmf with p = a/n beside the Poisson pmf with mean a.
#'
#' @param k successes, >= 0.
#' @param n trials, >= 1.
#' @param a the product n p held fixed as n grows.
#' @return list(binomial, poisson, abs_error).
#' @references Morin, D. J. (2016). Probability: For the Enthusiastic
#'   Beginner. Createspace. Eqs (4.34)-(4.35).
#' @examples
#' BinPoisLim(2, 1000, 3)$abs_error
#' @export
BinPoisLim <- function(k, n, a) {
  if (length(n) != 1L || is.na(n) || n < 1 || n != as.integer(n)) {
    stop("n must be a single integer >= 1.", call. = FALSE)
  }
  a <- as.numeric(a)
  exact <- BinomPmf(k, n, a / as.integer(n))$probability
  limit <- PoisPmf(k, a)$probability
  list(binomial = exact, poisson = limit, abs_error = abs(exact - limit))
}

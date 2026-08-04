# SPDX-License-Identifier: AGPL-3.0-or-later

#' The (1+a)^n product expansion from the log series
#'
#' exp(n sum_j (-1)^(j+1) a^j / j), the product expansion of (1+a)^n.
#'
#' @param a perturbation with abs(a) < 1, so the log series converges.
#' @param n exponent.
#' @param terms number of log-series terms, >= 1.
#' @return list(exact, product_form, rel_error).
#' @references Morin, D. J. (2016). Probability: For the Enthusiastic
#'   Beginner. Createspace. Eq (7.21).
#' @examples
#' PowLogSer(0.1, 20)$rel_error
#' @export
PowLogSer <- function(a, n, terms = 12) {
  a <- as.numeric(a)
  n <- as.numeric(n)
  if (length(a) != 1L || is.na(a) || abs(a) >= 1) {
    stop("need abs(a) < 1 for the log series.", call. = FALSE)
  }
  if (length(n) != 1L || is.na(n)) stop("n must be a single value.", call. = FALSE)
  if (length(terms) != 1L || is.na(terms) || terms < 1 ||
        terms != as.integer(terms)) {
    stop("terms must be a single integer >= 1.", call. = FALSE)
  }
  s <- 0
  for (j in 1:as.integer(terms)) s <- s + (-1)^(j + 1) * a^j / j
  product_form <- exp(n * s)
  exact <- (1 + a)^n
  list(exact = exact, product_form = product_form,
       rel_error = abs(exact - product_form) / max(abs(exact), 1e-300))
}

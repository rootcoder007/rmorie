# SPDX-License-Identifier: AGPL-3.0-or-later

#' Standard deviation of a binomial count
#'
#' The defaults are the book's worked dice total: n = 10,000 rolls at
#' p = 1/6 gives sigma = 37.
#'
#' @param n number of trials, >= 0.
#' @param p success probability, in \[0, 1\].
#' @return list(n, p, sd, sd_tot); sd_tot is an alias of sd kept for
#'   the eq (3.56) callers.
#' @references Morin, D. J. (2016). Probability: For the Enthusiastic
#'   Beginner. Createspace. Eqs (3.47), (3.56).
#' @examples
#' SdBinom(100, 0.5)$sd
#' @export
SdBinom <- function(n = 10000, p = 1 / 6) {
  if (length(n) != 1L || is.na(n) || n < 0 || n != as.integer(n)) {
    stop("n must be a single integer >= 0.", call. = FALSE)
  }
  p <- as.numeric(p)
  if (length(p) != 1L || is.na(p) || p < 0 || p > 1) {
    stop("p must be a single value in [0, 1].", call. = FALSE)
  }
  n <- as.integer(n)
  value <- sqrt(n * p * (1 - p))
  list(n = n, p = p, sd = value, sd_tot = value)
}

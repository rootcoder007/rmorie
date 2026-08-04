# SPDX-License-Identifier: AGPL-3.0-or-later

#' Stirling approximation to the half-heads probability
#'
#' P(exactly n Heads in 2n fair flips) = C(2n, n)/4^n (eq 2.65) against
#' the Stirling form 1/sqrt(pi n) (eq 2.66).
#'
#' @param n half the number of flips, >= 1.
#' @return list(n, approx, exact, relative_error).
#' @references Morin, D. J. (2016). Probability: For the Enthusiastic
#'   Beginner. Createspace. Eqs. (2.65)-(2.66).
#' @examples
#' HalfHeads(50)$relative_error
#' @export
HalfHeads <- function(n) {
  n <- as.integer(n)
  if (length(n) != 1L || is.na(n) || n < 1L) {
    stop("n must be a single integer >= 1.", call. = FALSE)
  }
  approx <- 1 / sqrt(pi * n)
  exact <- exp(lchoose(2 * n, n) - 2 * n * log(2))
  list(n = n, approx = approx, exact = exact,
       relative_error = abs(approx - exact) / exact)
}

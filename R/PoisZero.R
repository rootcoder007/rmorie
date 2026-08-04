# SPDX-License-Identifier: AGPL-3.0-or-later

#' Poisson probability of no events
#'
#' The default a = 7 is the book's typos-per-page example, giving about
#' a 0.1 percent chance of a clean page.
#'
#' @param a expected count, >= 0.
#' @return list(a, p_zero).
#' @references Morin, D. J. (2016). Probability: For the Enthusiastic
#'   Beginner. Createspace. Eq (4.99).
#' @examples
#' PoisZero()$p_zero
#' @export
PoisZero <- function(a = 7) {
  a <- as.numeric(a)
  if (length(a) != 1L || is.na(a) || a < 0) {
    stop("a must be a single value >= 0.", call. = FALSE)
  }
  list(a = a, p_zero = PoisPmf(0, a)$probability)
}

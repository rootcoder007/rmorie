# SPDX-License-Identifier: AGPL-3.0-or-later

#' Gaussian ordinate many standard deviations out
#'
#' sigma f(n sigma) = e^(-n^2/2)/sqrt(2 pi), independent of sigma.  The
#' default n = 20 is the book's 1e-87 illustration.
#'
#' @param n_sigmas how many standard deviations out to evaluate.
#' @param sigma standard deviation, > 0.
#' @return list(n_sigmas, area_fraction).
#' @references Morin, D. J. (2016). Probability: For the Enthusiastic
#'   Beginner. Createspace. Eq (5.25).
#' @examples
#' GaussTail()$area_fraction
#' @export
GaussTail <- function(n_sigmas = 20, sigma = 1) {
  sigma <- as.numeric(sigma)
  n_sigmas <- as.numeric(n_sigmas)
  if (length(sigma) != 1L || is.na(sigma) || sigma <= 0) {
    stop("sigma must be a single value > 0.", call. = FALSE)
  }
  if (length(n_sigmas) != 1L || is.na(n_sigmas)) {
    stop("n_sigmas must be a single value.", call. = FALSE)
  }
  x <- n_sigmas * sigma
  value <- exp(-x^2 / (2 * sigma^2)) / sqrt(2 * pi * sigma^2) * sigma
  list(n_sigmas = n_sigmas, area_fraction = value)
}

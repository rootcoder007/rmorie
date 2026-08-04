# SPDX-License-Identifier: AGPL-3.0-or-later

#' Expected Gaussian count over N repetitions
#'
#' N f(x) for a Gaussian f.  The defaults are the book's dice-sum
#' experiment: ten dice, mean 35, sigma 5.4, repeated 100,000 times.
#'
#' @param x outcome value.
#' @param n_reps number of repetitions, > 0.
#' @param mu,sigma Gaussian mean and standard deviation; sigma > 0.
#' @return list(expected_count, mu, sigma).
#' @references Morin, D. J. (2016). Probability: For the Enthusiastic
#'   Beginner. Createspace. Eq (5.28).
#' @examples
#' GaussCount(35)$expected_count
#' @export
GaussCount <- function(x, n_reps = 100000, mu = 35, sigma = 5.4) {
  n_r <- as.numeric(n_reps)
  sigma <- as.numeric(sigma)
  mu <- as.numeric(mu)
  x <- as.numeric(x)
  if (length(n_r) != 1L || is.na(n_r) || n_r <= 0) {
    stop("n_reps must be a single value > 0.", call. = FALSE)
  }
  if (length(sigma) != 1L || is.na(sigma) || sigma <= 0) {
    stop("sigma must be a single value > 0.", call. = FALSE)
  }
  dens <- exp(-(x - mu)^2 / (2 * sigma^2)) / sqrt(2 * pi * sigma^2)
  list(expected_count = n_r * dens, mu = mu, sigma = sigma)
}

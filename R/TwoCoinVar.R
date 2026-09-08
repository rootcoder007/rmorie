# SPDX-License-Identifier: AGPL-3.0-or-later

#' Variance of the sum of two fair coin flips
#'
#' Convolves two Bernoulli(1/2) pmfs and takes the variance of the
#' resulting 0/1/2 distribution: mean 1, variance 1/2.
#'
#' @return list(variance, mean).
#' @references Morin, D. J. (2016). Probability: For the Enthusiastic
#'   Beginner. Createspace. Eq (3.28).
#' @examples
#' TwoCoinVar()$variance
#' @export
TwoCoinVar <- function() {
  values <- c(0, 1, 2)
  probs <- c(0.25, 0.5, 0.25)
  mu <- sum(values * probs)
  list(variance = sum(probs * (values - mu)^2), mean = mu)
}

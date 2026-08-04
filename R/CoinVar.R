# SPDX-License-Identifier: AGPL-3.0-or-later

#' Variance of one fair coin flip
#'
#' Heads = 1, Tails = 0: mean 1/2, variance 1/4.
#'
#' @return list(variance, mean).
#' @references Morin, D. J. (2016). Probability: For the Enthusiastic
#'   Beginner. Createspace. Eq (3.21).
#' @examples
#' CoinVar()$variance
#' @export
CoinVar <- function() {
  values <- c(0, 1)
  probs <- c(0.5, 0.5)
  mu <- sum(values * probs)
  list(variance = sum(probs * (values - mu)^2), mean = mu)
}

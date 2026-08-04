# SPDX-License-Identifier: AGPL-3.0-or-later

#' Variance of a fair die roll
#'
#' For a uniform roll on 1..k the mean is (k+1)/2 and the variance is
#' (k^2 - 1)/12; the book's worked six-sided value is 2.92.
#'
#' @param sides number of faces, >= 1.
#' @return list(variance, mean, sides).
#' @references Morin, D. J. (2016). Probability: For the Enthusiastic
#'   Beginner. Createspace. Eq (3.20).
#' @examples
#' DieVar()$variance
#' @export
DieVar <- function(sides = 6) {
  if (length(sides) != 1L || is.na(sides) || sides < 1 ||
        sides != as.integer(sides)) {
    stop("sides must be a single integer >= 1.", call. = FALSE)
  }
  k <- as.integer(sides)
  values <- as.numeric(seq_len(k))
  probs <- rep(1 / k, k)
  mu <- sum(values * probs)
  v <- sum(probs * (values - mu)^2)
  list(variance = v, mean = mu, sides = k)
}

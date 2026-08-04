# SPDX-License-Identifier: AGPL-3.0-or-later

#' Standard deviation of the sample mean
#'
#' sigma/sqrt(N), reported with the decision that it never exceeds
#' sigma (true for every N >= 1).
#'
#' @param sigma per-observation standard deviation, >= 0.
#' @param N sample size, >= 1.
#' @return list(sd_mean, sigma, bounded).
#' @references Morin, D. J. (2016). Probability: For the Enthusiastic
#'   Beginner. Createspace. Eq (3.93).
#' @examples
#' SdXbar(3, 9)$sd_mean
#' @export
SdXbar <- function(sigma, N) {
  sigma <- as.numeric(sigma)
  if (length(sigma) != 1L || is.na(sigma) || sigma < 0) {
    stop("sigma must be a single value >= 0.", call. = FALSE)
  }
  if (length(N) != 1L || is.na(N) || N < 1 || N != as.integer(N)) {
    stop("N must be a single integer >= 1.", call. = FALSE)
  }
  value <- sqrt(sigma^2 / as.integer(N))
  if (value > sigma + 1e-12) stop("sd of the mean exceeded sigma.", call. = FALSE)
  list(sd_mean = value, sigma = sigma, bounded = TRUE)
}

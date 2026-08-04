# SPDX-License-Identifier: AGPL-3.0-or-later

#' Standard deviation of a sum of n i.i.d. variables
#'
#' sigma_sum = sqrt(n) sigma.
#'
#' @param sigma per-variable standard deviation, >= 0.
#' @param n number of i.i.d. terms, >= 0.
#' @return list(sigma, n, sd_sum).
#' @references Morin, D. J. (2016). Probability: For the Enthusiastic
#'   Beginner. Createspace. Eqs. (1.71), (3.45).
#' @examples
#' SdIidSum(2, 9)$sd_sum
#' @export
SdIidSum <- function(sigma, n) {
  sigma <- as.numeric(sigma)
  if (length(sigma) != 1L || is.na(sigma) || sigma < 0) {
    stop("sigma must be a single value >= 0.", call. = FALSE)
  }
  n <- as.integer(n)
  if (length(n) != 1L || is.na(n) || n < 0L) {
    stop("n must be a single integer >= 0.", call. = FALSE)
  }
  list(sigma = sigma, n = n, sd_sum = sqrt(n) * sigma)
}

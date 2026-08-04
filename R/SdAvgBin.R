# SPDX-License-Identifier: AGPL-3.0-or-later

#' Standard deviations of a binomial total and its average
#'
#' sigma_single = sqrt(pq), sigma_tot = sqrt(npq), sigma_avg =
#' sigma_tot / n, cross-checked against sigma_single / sqrt(n).  The
#' defaults are the book's worked dice average, sigma_avg = 0.0037.
#'
#' @param n number of trials, >= 1.
#' @param p success probability, in [0, 1].
#' @return list(sd_single, sd_tot, sd_avg).
#' @references Morin, D. J. (2016). Probability: For the Enthusiastic
#'   Beginner. Createspace. Eqs (3.57)-(3.58).
#' @examples
#' SdAvgBin()$sd_avg
#' @export
SdAvgBin <- function(n = 10000, p = 1 / 6) {
  if (length(n) != 1L || is.na(n) || n < 1 || n != as.integer(n)) {
    stop("n must be a single integer >= 1.", call. = FALSE)
  }
  p <- as.numeric(p)
  if (length(p) != 1L || is.na(p) || p < 0 || p > 1) {
    stop("p must be a single value in [0, 1].", call. = FALSE)
  }
  n <- as.integer(n)
  sd_single <- sqrt(p * (1 - p))
  sd_tot <- sqrt(n * p * (1 - p))
  sd_avg <- sd_tot / n
  if (abs(sd_avg - sd_single / sqrt(n)) > 1e-12) {
    stop("sigma_tot/n != sigma_single/sqrt(n).", call. = FALSE)
  }
  list(sd_single = sd_single, sd_tot = sd_tot, sd_avg = sd_avg)
}

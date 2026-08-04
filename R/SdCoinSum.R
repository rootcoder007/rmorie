# SPDX-License-Identifier: AGPL-3.0-or-later

#' Standard deviation of a fair-coin Heads count
#'
#' sigma = sqrt(n)/2 for the Heads count in n fair flips.
#'
#' @param n number of fair flips, >= 0.
#' @return list(n, sd, sd_tot); sd_tot is an alias of sd kept for the
#'   eq (3.51) callers.
#' @references Morin, D. J. (2016). Probability: For the Enthusiastic
#'   Beginner. Createspace. Eqs (3.48), (3.51).
#' @examples
#' SdCoinSum(100)$sd
#' @export
SdCoinSum <- function(n) {
  if (length(n) != 1L || is.na(n) || n < 0 || n != as.integer(n)) {
    stop("n must be a single integer >= 0.", call. = FALSE)
  }
  n <- as.integer(n)
  value <- sqrt(n) / 2
  list(n = n, sd = value, sd_tot = value)
}

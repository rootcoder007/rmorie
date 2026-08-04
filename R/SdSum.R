# SPDX-License-Identifier: AGPL-3.0-or-later

#' Standard deviation of a sum of independent variables
#'
#' sigma_sum = sqrt(sum sigma_i^2).
#'
#' @param sigmas per-variable standard deviations, each >= 0.
#' @return list(sd_sum).
#' @references Morin, D. J. (2016). Probability: For the Enthusiastic
#'   Beginner. Createspace. Eqs (3.42)-(3.43).
#' @examples
#' SdSum(c(3, 4))$sd_sum
#' @export
SdSum <- function(sigmas) {
  s <- as.numeric(sigmas)
  if (length(s) == 0L || any(is.na(s)) || any(s < 0)) {
    stop("sigmas must be a non-empty vector of values >= 0.", call. = FALSE)
  }
  list(sd_sum = sqrt(sum(s^2)))
}

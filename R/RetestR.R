# SPDX-License-Identifier: AGPL-3.0-or-later

#' Test-retest correlation
#'
#' r = sigma_signal / sqrt(sigma_signal^2 + sigma_noise^2); equal signal
#' and noise give r = 1/sqrt(2).
#'
#' @param sigma_signal spread of the underlying ability, >= 0.
#' @param sigma_noise spread of the measurement noise, >= 0.
#' @return list(r).
#' @references Morin, D. J. (2016). Probability: For the Enthusiastic
#'   Beginner. Createspace. Eq (6.37).
#' @examples
#' RetestR(1, 1)$r
#' @export
RetestR <- function(sigma_signal, sigma_noise) {
  list(r = LinModel(1, sigma_signal, sigma_noise)$r)
}

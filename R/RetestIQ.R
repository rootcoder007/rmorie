# SPDX-License-Identifier: AGPL-3.0-or-later

#' IQ test-retest worked example
#'
#' Equal signal and noise spreads of 15/sqrt(2) reproduce the observed
#' IQ spread of 15 and a retest correlation of about 0.71.
#'
#' @return list(r, sigma_y).
#' @references Morin, D. J. (2016). Probability: For the Enthusiastic
#'   Beginner. Createspace. Eq (6.38).
#' @examples
#' RetestIQ()$r
#' @export
RetestIQ <- function() {
  sigma_x <- 15 / sqrt(2)
  m <- LinModel(1, sigma_x, sigma_x)
  list(r = m$r, sigma_y = m$sigma_y)
}

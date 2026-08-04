# SPDX-License-Identifier: AGPL-3.0-or-later

#' Mean of an exponential waiting time
#'
#' The integral of t e^(-t/tau)/tau dt is tau.
#'
#' @param tau mean waiting time, > 0.
#' @return list(mean).
#' @references Morin, D. J. (2016). Probability: For the Enthusiastic
#'   Beginner. Createspace. Eq (4.83).
#' @examples
#' ExpMean(2.5)$mean
#' @export
ExpMean <- function(tau) {
  tau <- as.numeric(tau)
  if (length(tau) != 1L || is.na(tau) || tau <= 0) {
    stop("tau must be a single value > 0.", call. = FALSE)
  }
  list(mean = tau)
}

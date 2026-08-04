# SPDX-License-Identifier: AGPL-3.0-or-later

#' Second moment and variance of an exponential waiting time
#'
#' E(T^2) = 2 tau^2 and Var(T) = tau^2.
#'
#' @param tau mean waiting time, > 0.
#' @return list(second_moment, variance).
#' @references Morin, D. J. (2016). Probability: For the Enthusiastic
#'   Beginner. Createspace. Eq (4.85).
#' @examples
#' ExpMom2(2.5)$variance
#' @export
ExpMom2 <- function(tau) {
  tau <- as.numeric(tau)
  if (length(tau) != 1L || is.na(tau) || tau <= 0) {
    stop("tau must be a single value > 0.", call. = FALSE)
  }
  list(second_moment = 2 * tau^2, variance = tau^2)
}

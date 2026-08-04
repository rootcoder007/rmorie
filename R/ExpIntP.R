# SPDX-License-Identifier: AGPL-3.0-or-later

#' Exponential waiting-time interval probability
#'
#' P(the next event falls in [t, t + dt]) factorises into surviving to
#' t and then firing in dt.
#'
#' @param t elapsed time, >= 0.
#' @param dt interval width, >= 0.
#' @param lam rate, > 0.
#' @return list(probability).
#' @references Morin, D. J. (2016). Probability: For the Enthusiastic
#'   Beginner. Createspace. Eqs (4.23), (4.25).
#' @examples
#' ExpIntP(1, 0.01, 2)$probability
#' @export
ExpIntP <- function(t, dt, lam) {
  t <- as.numeric(t); dt <- as.numeric(dt); lam <- as.numeric(lam)
  if (length(t) != 1L || length(dt) != 1L || length(lam) != 1L ||
        is.na(t) || is.na(dt) || is.na(lam) || t < 0 || dt < 0 || lam <= 0) {
    stop("need t >= 0, dt >= 0, lambda > 0.", call. = FALSE)
  }
  list(probability = exp(-lam * t) * lam * dt)
}

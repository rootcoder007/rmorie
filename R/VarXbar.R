# SPDX-License-Identifier: AGPL-3.0-or-later

#' Variance of the sample mean
#'
#' E[(xbar - mu)^2] = sigma^2 / N.
#'
#' @param sigma per-observation standard deviation, >= 0.
#' @param N sample size, >= 1.
#' @return list(var_mean).
#' @references Morin, D. J. (2016). Probability: For the Enthusiastic
#'   Beginner. Createspace. Eq (3.92).
#' @examples
#' VarXbar(3, 9)$var_mean
#' @export
VarXbar <- function(sigma, N) {
  sigma <- as.numeric(sigma)
  if (length(sigma) != 1L || is.na(sigma) || sigma < 0) {
    stop("sigma must be a single value >= 0.", call. = FALSE)
  }
  if (length(N) != 1L || is.na(N) || N < 1 || N != as.integer(N)) {
    stop("N must be a single integer >= 1.", call. = FALSE)
  }
  list(var_mean = sigma^2 / as.integer(N))
}

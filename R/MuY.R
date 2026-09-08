# SPDX-License-Identifier: AGPL-3.0-or-later

#' Mean of the linear model output
#'
#' mu_y = m mu_x + mu_z.
#'
#' @param m slope.
#' @param mu_x mean of the signal.
#' @param mu_z mean of the noise.
#' @return list(mu_y).
#' @references Morin, D. J. (2016). Probability: For the Enthusiastic
#'   Beginner. Createspace. Eq (6.4).
#' @examples
#' MuY(2, 3, 1)$mu_y
#' @export
MuY <- function(m, mu_x, mu_z) {
  m <- as.numeric(m)
  mu_x <- as.numeric(mu_x)
  mu_z <- as.numeric(mu_z)
  if (length(m) != 1L || length(mu_x) != 1L || length(mu_z) != 1L ||
        is.na(m) || is.na(mu_x) || is.na(mu_z)) {
    stop("m, mu_x and mu_z must be single values.", call. = FALSE)
  }
  list(mu_y = m * mu_x + mu_z)
}

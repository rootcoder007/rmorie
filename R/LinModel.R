# SPDX-License-Identifier: AGPL-3.0-or-later

#' The Y = mX + Z correlation model
#'
#' mu_y = m mu_x + mu_z with mu_x = mu_z = 0, sigma_y =
#' sqrt(m^2 sigma_x^2 + sigma_z^2) and r = m sigma_x / sigma_y.  The
#' defaults are the book's worked spread, sigma_y = 13.
#'
#' @param m slope of the underlying relation.
#' @param sigma_x spread of the signal, >= 0.
#' @param sigma_z spread of the independent noise, >= 0.
#' @return list(mu_y, sigma_y, r, mu_z, sigma_z).
#' @references Morin, D. J. (2016). Probability: For the Enthusiastic
#'   Beginner. Createspace. Eqs (6.3)-(6.6), (6.17), (6.76).
#' @examples
#' LinModel()$sigma_y
#' @export
LinModel <- function(m = 1, sigma_x = 7.5, sigma_z = 10.6) {
  m <- as.numeric(m)
  sigma_x <- as.numeric(sigma_x)
  sigma_z <- as.numeric(sigma_z)
  if (length(m) != 1L || length(sigma_x) != 1L || length(sigma_z) != 1L ||
        is.na(m) || is.na(sigma_x) || is.na(sigma_z)) {
    stop("m, sigma_x and sigma_z must be single values.", call. = FALSE)
  }
  if (sigma_x < 0 || sigma_z < 0) stop("sigmas must be >= 0.", call. = FALSE)
  sigma_y <- sqrt(m^2 * sigma_x^2 + sigma_z^2)
  if (sigma_y == 0) stop("degenerate model: sigma_y = 0.", call. = FALSE)
  list(mu_y = 0, sigma_y = sigma_y, r = m * sigma_x / sigma_y, mu_z = 0,
       sigma_z = sigma_z)
}

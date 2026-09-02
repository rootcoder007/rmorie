# SPDX-License-Identifier: AGPL-3.0-or-later
#' Covariance and semivariogram under second-order stationarity
#'
#' gamma(h) = C(0) - C(h). The identity requires second-order
#' stationarity: an intrinsically stationary process has a semivariogram
#' but need not have a covariance function at all.
#'
#' @param cov_func Function C(h) taking a numeric vector and returning one.
#' @param h Numeric vector of non-negative lag distances.
#' @return Named list: gamma, covariance, sill.
#' @references Schabenberger & Gotway (2005), Sec 1.4.2 / Ch 2.
#' @examples
#' spssoc(function(x) 2 * exp(-x), h = c(0, 1, 2))
#' @export
spssoc <- function(cov_func, h) {
  if (!is.function(cov_func)) stop("`cov_func` must be a function C(h)")
  h <- as.numeric(h)
  if (any(h < 0)) stop("lag distances `h` must be non-negative")
  c0 <- as.numeric(cov_func(0))[1]
  ch <- as.numeric(cov_func(h))
  list(gamma = c0 - ch, covariance = ch, sill = c0)
}

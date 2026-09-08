# SPDX-License-Identifier: AGPL-3.0-or-later
#' Matern covariance function class
#'
#' C(h) = sigma^2 (1/gamma(nu)) (theta h / 2)^nu 2 K_nu(theta h), for
#' nu > 0 and theta > 0. Smoothness increases with nu. Because
#' K_nu(t) is approximately gamma(nu)/2 (t/2)^-nu as t approaches 0, the
#' process variance is exactly sigma^2, so C(0) = sigma2.
#'
#' The book names three members: nu = 1/2 is the exponential model
#' (eq 4.11), nu = 1 is Whittle's model (eq 4.12), and nu approaching
#' infinity is the gaussian model (eq 4.10).
#'
#' Note the parameterisation: `a` is the book's theta from eq (4.9), NOT
#' a practical range. The practical range of a Matern model is itself a
#' function of nu.
#'
#' @param h Numeric vector of non-negative lag distances.
#' @param sigma2 Process variance, positive.
#' @param nu Smoothness, positive.
#' @param a Scale theta, positive.
#' @return Named list: covariance, semivariogram, sigma2, nu, theta.
#' @references Schabenberger & Gotway (2005), Sec 4.3.2, eq (4.9), p. 143.
#' @examples
#' spmatr(h = c(0, 0.5, 1, 3), sigma2 = 2, nu = 1.5, a = 1)
#' @export
spmatr <- function(h, sigma2 = 1, nu = 0.5, a = 1) {
  if (nu <= 0) stop("`nu` must be > 0 for the Matern class")
  if (a <= 0) stop("`a` (theta) must be > 0")
  if (sigma2 <= 0) stop("`sigma2` must be > 0")
  h <- as.numeric(h)
  if (any(h < 0)) stop("lag distances `h` must be non-negative")
  t <- a * h
  cval <- numeric(length(t))
  pos <- t > 0
  cval[!pos] <- sigma2
  if (any(pos)) {
    tp <- t[pos]
    cval[pos] <- sigma2 * (1 / gamma(nu)) * (tp / 2)^nu * 2 * besselK(tp, nu)
  }
  list(covariance = cval, semivariogram = sigma2 - cval,
       sigma2 = as.numeric(sigma2), nu = as.numeric(nu), theta = as.numeric(a))
}

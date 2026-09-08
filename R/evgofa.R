# SPDX-License-Identifier: AGPL-3.0-or-later
#' Anderson-Darling goodness-of-fit test for a fitted GEV
#'
#' The A^2 statistic is Anderson and Darling (1952), read from the corpus
#' PDF as Hedderich, Sachs and Reynarowych eq (7.33); the single
#' implementation lives in \code{\link{Adcore}} and is called, not
#' copied.  The probability-integral transform is the generalized
#' extreme value distribution function, Coles (2001), An Introduction to
#' Statistical Modeling of Extreme Values, eq (3.2):
#' \code{G(z) = exp(-(1 + xi (z-mu)/sigma)^(-1/xi))} on
#' \code{1 + xi (z-mu)/sigma > 0}, and
#' \code{G(z) = exp(-exp(-(z-mu)/sigma))} when \code{xi = 0}.
#'
#' Parameters are supplied by the caller, not fitted.  That is
#' deliberate: the package GEV/GPD fitters are Nelder-Mead and agree
#' across language arms only to about 1e-4, which would make the
#' statistic irreproducible.  No p-value is returned: Stephens (1986)
#' critical values depend on which parameters were estimated and how,
#' and that chapter was not obtainable here.
#'
#' @param x Numeric block maxima.
#' @param mu,sigma,xi Location, scale and shape.  Supplied, not fitted.
#' @return list: statistic, u, mu, sigma, xi, n, method.
#' @examples
#' Adgev(c(0.1, 0.9, 1.4, 2.2, 3.1), 1, 1, 0.1)$statistic
#' @export
Adgev <- function(x, mu = 0, sigma = 1, xi = 0) {
  x <- as.numeric(x)
  u <- Gevcdf(x, mu, sigma, xi)
  list(
    statistic = Adcore(u), u = sort(u),
    mu = as.numeric(mu), sigma = as.numeric(sigma), xi = as.numeric(xi),
    n = length(x),
    method = paste(
      "Anderson-Darling A^2 for a fitted GEV",
      "(Anderson and Darling 1952; Coles 2001 eq. 3.2)"
    )
  )
}

#' GEV distribution function (Coles 2001 eq. 3.2)
#'
#' Returns 0 below and 1 above the support endpoint implied by
#' \code{xi}.  See \code{\link{Adgev}} for the source.
#'
#' @param x Numeric vector.
#' @param mu,sigma,xi Location, scale and shape.
#' @return Numeric vector of probabilities.
#' @examples
#' Gevcdf(c(0, 1, 2), 1, 1, 0.1)
#' @export
Gevcdf <- function(x, mu = 0, sigma = 1, xi = 0) {
  x <- as.numeric(x)
  sigma <- as.numeric(sigma)
  if (sigma <= 0) stop("sigma must be positive")
  xi <- as.numeric(xi)
  z <- (x - as.numeric(mu)) / sigma
  if (xi == 0) {
    return(exp(-exp(-z)))
  }
  t <- 1 + xi * z
  out <- numeric(length(z))
  ok <- t > 0
  out[ok] <- exp(-(t[ok]^(-1 / xi)))
  out[!ok] <- if (xi > 0) 0 else 1
  out
}

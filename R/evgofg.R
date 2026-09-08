# SPDX-License-Identifier: AGPL-3.0-or-later
#' Anderson-Darling goodness-of-fit test for a fitted GPD
#'
#' Same A^2 statistic as \code{\link{Adgev}}, computed by
#' \code{\link{Adcore}}, which holds the single implementation taken
#' from the corpus PDF (Hedderich, Sachs and Reynarowych eq (7.33);
#' Anderson and Darling 1952).  Only the probability-integral transform
#' changes: here it is the generalized Pareto distribution function,
#' Coles (2001) eq (4.2),
#' \code{H(y) = 1 - (1 + xi y/sigma)^(-1/xi)} for \code{y > 0}, and
#' \code{H(y) = 1 - exp(-y/sigma)} when \code{xi = 0}.
#'
#' \code{y} are threshold excesses and must be positive; the caller
#' subtracts the threshold.  Parameters are supplied, not fitted, for
#' the same reproducibility reason as \code{\link{Adgev}}.  No p-value
#' is returned.
#'
#' @param y Numeric threshold excesses, all positive.
#' @param sigma,xi Scale and shape.  Supplied, not fitted.
#' @return list: statistic, u, sigma, xi, n, method.
#' @examples
#' Adgpd(c(0.2, 0.7, 1.3, 2.0, 3.4), 1, 0.1)$statistic
#' @export
Adgpd <- function(y, sigma = 1, xi = 0) {
  y <- as.numeric(y)
  if (any(y <= 0)) {
    stop("threshold excesses must be positive; subtract the threshold first")
  }
  u <- Gpdcdf(y, sigma, xi)
  list(
    statistic = Adcore(u), u = sort(u),
    sigma = as.numeric(sigma), xi = as.numeric(xi), n = length(y),
    method = paste(
      "Anderson-Darling A^2 for a fitted GPD",
      "(Anderson and Darling 1952; Coles 2001 eq. 4.2)"
    )
  )
}

#' GPD distribution function of threshold excesses (Coles 2001 eq. 4.2)
#'
#' See \code{\link{Adgpd}} for the source.
#'
#' @param y Numeric threshold excesses.
#' @param sigma,xi Scale and shape.
#' @return Numeric vector of probabilities.
#' @examples
#' Gpdcdf(c(0.5, 1, 2), 1, 0.1)
#' @export
Gpdcdf <- function(y, sigma = 1, xi = 0) {
  y <- as.numeric(y)
  sigma <- as.numeric(sigma)
  if (sigma <= 0) stop("sigma must be positive")
  xi <- as.numeric(xi)
  out <- numeric(length(y))
  neg <- y <= 0
  if (xi == 0) {
    out <- 1 - exp(-y / sigma)
  } else {
    t <- 1 + xi * y / sigma
    ok <- t > 0
    out[ok] <- 1 - t[ok]^(-1 / xi)
    out[!ok] <- 1
  }
  out[neg] <- 0
  out
}

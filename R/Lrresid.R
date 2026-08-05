# SPDX-License-Identifier: AGPL-3.0-or-later
#' Residual analysis for the logistic regression model
#'
#' Source READ FROM THE CORPUS PDF, pages rendered with pdftoppm:
#' Hedderich, Sachs and Reynarowych, Applied Statistics: Methods Using R,
#' section 8.4.5 "Residual Analysis", printed pages 837-838, equations
#' (8.67), (8.68) and (8.69).
#'
#' Pearson residuals (8.67):
#' \code{r_i = (y_i - n_i p_i) / sqrt(n_i p_i (1 - p_i))}.
#'
#' Model deviance and deviance residuals (8.68):
#' \code{D = sum d_i^2} with
#' \code{d_i = sign(y_i - n_i p_i) sqrt(2 [y_i log(y_i / (n_i p_i)) +
#' (n_i - y_i) log((n_i - y_i) / (n_i (1 - p_i)))])}.
#'
#' BOOK ERRATUM in (8.68): the printed radicand is
#' \code{-2(y log(y/(n p))) + (n - y) log(...)}.  Read literally the
#' leading factor is -2 on the first term only, which makes the radicand
#' negative for a well-fitting observation and the square root
#' imaginary.  The bracket is the Kullback-Leibler divergence of the
#' saturated model from the fitted one and is non-negative, so the
#' factor is +2 over the whole bracket -- the standard published
#' binomial deviance residual, and the one base R returns from
#' \code{residuals(fit, "deviance")}, against which this is anchored.
#' The sign is \code{sign(y_i - n_i p_i)}, which the book writes as the
#' leading +/-.
#'
#' Influence measure (8.69):
#' \code{Delta D_i = d_i^2 + r_i^2 h_ii / (1 - h_ii)}, given on page 838
#' as R code \code{idev <- deviance.resid^2 + pearson.resid^2 *
#' hats/(1-hats)}.
#'
#' Terms \code{0 log 0} arising when y_i = 0 or y_i = n_i are taken as 0.
#'
#' @param y Observed counts of successes; 0/1 for ungrouped data.
#' @param pihat Fitted probabilities, strictly inside (0, 1).
#' @param n Binomial denominators; defaults to all ones.
#' @param hat Optional hat-matrix diagonal; enables delta_d (8.69).
#' @return list: pearson, deviance, D, n, and with hat also hat, delta_d.
#' @examples
#' Lrresid(c(0, 1), c(0.3, 0.7))$D
#' @export
Lrresid <- function(y, pihat, n = NULL, hat = NULL) {
  y <- as.numeric(y)
  p <- as.numeric(pihat)
  m <- length(y)
  if (m == 0L) stop("y must not be empty")
  if (length(p) != m) stop("y and pihat must have the same length")
  nn <- if (is.null(n)) rep(1, m) else as.numeric(n)
  if (length(nn) != m) stop("y and n must have the same length")
  if (any(!(p > 0 & p < 1))) {
    stop("every fitted probability must lie strictly inside (0, 1)")
  }
  if (any(nn <= 0)) stop("every binomial denominator must be positive")
  if (any(y < 0 | y > nn)) stop("every y must satisfy 0 <= y <= n")
  xlogxy <- function(a, b) ifelse(a == 0, 0, a * log(a / b))
  mu <- nn * p
  pear <- (y - mu) / sqrt(nn * p * (1 - p))
  s <- 2 * (xlogxy(y, mu) + xlogxy(nn - y, nn * (1 - p)))
  s[s < 0] <- 0
  dev <- ifelse(y >= mu, sqrt(s), -sqrt(s))
  out <- list(pearson = pear, deviance = dev, D = sum(dev^2), n = nn)
  if (!is.null(hat)) {
    h <- as.numeric(hat)
    if (length(h) != m) stop("y and hat must have the same length")
    if (any(h < 0 | h >= 1)) stop("every hat value must lie in [0, 1)")
    out$hat <- h
    out$delta_d <- dev^2 + pear^2 * h / (1 - h)
  }
  out
}

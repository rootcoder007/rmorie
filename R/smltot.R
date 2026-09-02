# SPDX-License-Identifier: AGPL-3.0-or-later
#' Estimate a population total from a simple random sample
#'
#' The total is the mean scaled by N, so its variance is scaled by N^2;
#' the coefficient of variation, identical for the two, is returned to
#' make that visible.
#'
#' Formula: Yhat = N ybar; v(Yhat) = N^2 (1 - f) s^2 / n, f = n/N
#'
#' @param y Sample observations.
#' @param N Population size.
#' @param level Confidence level.
#' @return List with \code{estimate}, \code{se}, \code{ci_lower},
#'   \code{ci_upper}, \code{mean}, \code{cv}, \code{fpc}, \code{N},
#'   \code{n}.
#' @references Cochran (1977), Sampling Techniques, 3rd edition, Chapter
#'   2: Yhat = N ybar with V(Yhat) = N^2 (N - n)/N S^2/n for simple random
#'   sampling without replacement. Cross-checked against the CRAN package
#'   samplingbook 1.2.4, whose Smean uses the same finite-population-
#'   corrected variance for the mean.
#' @export
Srstotal <- function(y, N, level = 0.95) {
  y <- .t1_vec(y); n <- length(y)
  if (n < 2L) stop("a variance needs at least two observations")
  N <- as.numeric(N)
  if (N < n) stop("N must be at least n")
  if (level <= 0 || level >= 1)
    stop("level must lie strictly between 0 and 1")
  m <- mean(y); s2 <- stats::var(y)
  k <- if (is.infinite(N)) 1 else (N - n) / N
  var <- N^2 * k * s2 / n
  se <- sqrt(var); est <- N * m
  z <- stats::qnorm((1 + level) / 2)
  .t1_result(estimate = est, se = se, ci_lower = est - z * se,
             ci_upper = est + z * se, mean = m,
             cv = if (est != 0) se / est else NaN, fpc = k, N = N, n = n,
             method = "SRS population total, Yhat = N ybar")
}

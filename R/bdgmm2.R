# SPDX-License-Identifier: AGPL-3.0-or-later
#' Generalised moment selection test statistic.
#'
#' t_j = sqrt(n) mbar_j / sigma_j; xi_j = t_j / kappa_n with
#' kappa_n = sqrt(log n); moment j is retained when xi_j > -1, and
#' S = sum over retained of [max(t_j, 0)]^2.
#'
#' @param mbar Sample moment means, positive meaning violation.
#' @param sigma Moment standard deviations, strictly positive.
#' @param n Sample size.
#' @param kappa Tuning sequence; NULL uses sqrt(log n).
#'
#' @return List with S, t, xi, retained, nretained, kappa, n, J.
#' @references Andrews and Soares (2010), Econometrica 78(1), 119-157,
#'   Sects. 3-4.  Standard published form; the article is not in the
#'   local corpus and was not read.
#' @export
Gmsbound <- function(mbar, sigma, n, kappa = NULL) {
  m <- .t1_vec(mbar); s <- .t1_vec(sigma); J <- length(m)
  if (length(s) != J) stop("mbar and sigma must have the same length")
  if (any(s <= 0)) stop("standard deviations must be strictly positive")
  n <- as.numeric(n)
  if (n <= 1) stop("n must exceed 1")
  k <- if (is.null(kappa)) sqrt(log(n)) else as.numeric(kappa)
  if (k <= 0) stop("kappa must be strictly positive")
  t <- sqrt(n) * m / s
  xi <- t / k
  keep <- as.integer(xi > -1)
  .t1_result(S = sum((pmax(t, 0)^2)[keep == 1L]), t = t, xi = xi,
             retained = keep, nretained = sum(keep), kappa = k, n = n,
             J = J,
             method = "Generalised moment selection (Andrews-Soares 2010)")
}

# SPDX-License-Identifier: AGPL-3.0-or-later
#' The l-th absolute moment of a zero-mean normal
#'
#' This is the fact that makes the sign quantizer work: the estimator
#' turns an inner product into \code{E|x|} for a Gaussian with variance
#' \code{||k||^2}, and \code{E|x| = sigma sqrt(2/pi)} -- which is exactly
#' why \code{sqrt(pi/2)} sits in front of the estimator, cancelling it.
#'
#' Formula: \code{E|X|^l = sigma^l 2^(l/2) Gamma((l+1)/2) / sqrt(pi)}.
#'
#' @param sigma Standard deviation.
#' @param l Moment order; need not be an integer.
#' @return List with \code{moment}, \code{estimate}, \code{sigma}, \code{l}.
#' @references Zandieh, A., Daliri, M. & Han, I. (2024). arXiv:2406.03482,
#'   fact 3.4.
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' Tqmom(V, V)
Tqmom <- function(sigma, l) {
  sigma <- as.numeric(sigma)
  l <- as.numeric(l)
  val <- sigma^l * 2^(l / 2) * gamma((l + 1) / 2) / sqrt(pi)
  .t1_result(moment = val, estimate = val, sigma = sigma, l = l,
             method = "Absolute moment of a centred normal")
}

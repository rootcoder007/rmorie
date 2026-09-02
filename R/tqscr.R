# SPDX-License-Identifier: AGPL-3.0-or-later
#' Distortion of an inner-product score under TurboQuant_prod
#'
#' The 1/d in the bound means the inner-product distortion falls with
#' DIMENSION as well as with bits. \code{expected_max} is a Gaussian-tail
#' ESTIMATE of the largest of n_keys errors, not a bound from the paper.
#'
#' Formula: D_prod <= sqrt(3) pi^2 ||q||^2 / d . 4^-b (Theorem 2);
#'   rms = sqrt(D_prod); expected max ~= rms sqrt(2 log n)
#'
#' @param b Bits per coordinate.
#' @param d Key dimension.
#' @param query_norm ||q||_2 of the query.
#' @param n_keys Number of keys scored against.
#' @return List with \code{variance}, \code{rms}, \code{lower_bound},
#'   \code{ratio}, \code{expected_max}, \code{b}, \code{d},
#'   \code{n_keys}.
#' @references Zandieh et al., arXiv:2504.19874, Theorem 2 and Theorem 3.
#'   Fetched from arXiv. The expected_max figure is the standard Gaussian
#'   maximum approximation and is NOT from the paper.
#' @export
#' @examples
#' Scoredist(b = 5L, d = 5L)
Scoredist <- function(b, d, query_norm = 1, n_keys = 1) {
  b <- as.numeric(b)
  d <- as.integer(d)
  qn <- as.numeric(query_norm)
  nk <- as.integer(n_keys)
  if (b < 0) stop("the bit-width must be non-negative")
  if (d < 1L) stop("the dimension must be at least 1")
  if (qn < 0) stop("the query norm must be non-negative")
  if (nk < 1L) stop("there must be at least one key")
  q <- 4^(-b)
  var <- sqrt(3) * pi^2 * qn^2 / d * q
  lo <- q / d
  rms <- sqrt(var)
  em <- if (nk > 1L) rms * sqrt(2 * log(nk)) else rms
  .t1_result(variance = var, rms = rms, lower_bound = lo,
             ratio = if (lo > 0) var / lo else NaN, expected_max = em,
             b = b, d = as.numeric(d), n_keys = as.numeric(nk),
             method = "Score distortion, arXiv:2504.19874 Theorems 2 and 3")
}

# SPDX-License-Identifier: AGPL-3.0-or-later
#' Getis-Ord global G, with its exact randomisation moments
#'
#' Formula: \eqn{G = \sum_i \sum_{j \ne i} w_{ij} x_i x_j /
#' \sum_i \sum_{j \ne i} x_i x_j}, \eqn{E\[G\] = S_0/(n(n-1))}, and the
#' randomisation variance of Getis and Ord (1992) built from
#' \eqn{S_0, S_1, S_2} and the moments \eqn{m_r = \sum_i x_i^r}:
#' \eqn{B_0 = (n^2-3n+3)S_1 - nS_2 + 3S_0^2},
#' \eqn{B_1 = -\[(n^2-n)S_1 - 2nS_2 + 6S_0^2\]},
#' \eqn{B_2 = -\[2nS_1 - (n+3)S_2 + 6S_0^2\]},
#' \eqn{B_3 = 4(n-1)S_1 - 2(n+1)S_2 + 8S_0^2}, \eqn{B_4 = S_1 - S_2 + S_0^2}.
#' G is only interpretable for non-negative \code{x}, so negative input
#' is rejected rather than silently returned.
#'
#' @param x Non-negative variable of length n.
#' @param W n by n spatial weights; the diagonal is forced to zero.
#' @return List with \code{estimate} (G), \code{statistic},
#'   \code{p_value}, \code{expected}, \code{var}, \code{S0}, \code{S1},
#'   \code{S2}, \code{n}, \code{method}.
#' @references Getis and Ord (1992), Geographical Analysis 24:189-206.  Paywalled; the
#' coded moments were read from Bivand and Ono's spdep, R/globalG.R and
#' spweights.constants in R/utils.R.  B1 uses spdep's B1correct = TRUE default (6 S0^2,
#' not the 3 S0^2 of CrimeStat IV).
#' @export
#' @examples
#' set.seed(1)
#' x <- runif(10, 1, 10)
#' W <- matrix(rbinom(100, 1, 0.3), 10, 10)
#' diag(W) <- 0
#' Getisordg(x, W)
Getisordg <- function(x, W) {
  x <- .t4_vec(x)
  n <- length(x)
  W <- matrix(as.numeric(as.matrix(W)), nrow = n)
  if (nrow(W) != n || ncol(W) != n) stop("W must be n x n with n = length(x)")
  if (any(x < 0)) stop("Getis-Ord G is undefined for negative x")
  diag(W) <- 0
  numer <- sum(W * outer(x, x))
  denom <- sum(outer(x, x)) - sum(x^2)
  if (denom == 0) stop("degenerate x: cross-product sum is zero")
  g <- numer / denom
  s0 <- sum(W)
  s1 <- sum(W * W + W * t(W))
  s2 <- sum((rowSums(W) + colSums(W))^2)
  nn <- n * n
  n1 <- n - 1
  n2 <- n - 2
  n3 <- n - 3
  eg <- s0 / (n * n1)
  s02 <- s0 * s0
  b0 <- (nn - 3 * n + 3) * s1 - n * s2 + 3 * s02
  b1 <- -((nn - n) * s1 - 2 * n * s2 + 6 * s02)
  b2 <- -(2 * n * s1 - (n + 3) * s2 + 6 * s02)
  b3 <- 4 * n1 * s1 - 2 * (n + 1) * s2 + 8 * s02
  b4 <- s1 - s2 + s02
  m1 <- sum(x)
  m2 <- sum(x^2)
  m3 <- sum(x^3)
  m4 <- sum(x^4)
  vg <- (b0 * m2^2 + b1 * m4 + b2 * m1^2 * m2 + b3 * m1 * m3 + b4 * m1^4) /
    (((m1^2 - m2)^2) * n * n1 * n2 * n3) - eg^2
  z <- if (vg > 0) (g - eg) / sqrt(vg) else NaN
  p <- if (vg > 0) 2 * stats::pnorm(abs(z), lower.tail = FALSE) else NaN
  .t4_result(estimate = g, statistic = z, p_value = p, expected = eg,
             var = vg, S0 = s0, S1 = s1, S2 = s2, n = as.integer(n),
             method = "Getis-Ord global G")
}

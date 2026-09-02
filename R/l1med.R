# SPDX-License-Identifier: AGPL-3.0-or-later
#' Multivariate median that minimises total distance, not squared distance
#'
#' Swapping the squared loss for the plain one buys a breakdown point of
#' one half in any dimension, at the cost of the closed form. The spatial
#' rank of the solution is returned as a check: at the true L1 median the
#' sum of unit vectors to the data points vanishes.
#'
#' Formula: \code{min_mu sum_i ||x_i - mu||}, by Weiszfeld iteration for
#' a fixed number of steps.
#'
#' @param X Points.
#' @param tol Ignored; interface compatibility only.
#' @param max_iter Iterations.
#' @return List with \code{estimate}, \code{cost},
#'   \code{spatial_rank_norm}, \code{n}, \code{d}.
#' @references Weiszfeld, E. (1937). Tohoku Math J 43:355-386; Small,
#'   C. G. (1990) Int Statist Rev 58:263-277.
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' L1med(V)
L1med <- function(X, tol = NULL, max_iter = 200) {
  A <- as.matrix(X)
  n <- nrow(A)
  d <- ncol(A)
  mu <- colSums(A) / n
  for (it in seq_len(as.integer(max_iter))) {
    dist <- sqrt(rowSums((A - matrix(mu, n, d, byrow = TRUE))^2))
    ok <- dist >= 1e-12
    if (!any(ok)) next
    w <- 1 / dist[ok]
    mu <- colSums(A[ok, , drop = FALSE] * w) / sum(w)
  }
  dist <- sqrt(rowSums((A - matrix(mu, n, d, byrow = TRUE))^2))
  cost <- sum(dist)
  ok <- dist >= 1e-12
  sr <- if (any(ok)) colSums((A[ok, , drop = FALSE] -
      matrix(mu, sum(ok), d, byrow = TRUE)) / dist[ok]) else rep(0, d)
  .t1_result(estimate = mu, cost = cost, spatial_rank_norm = sqrt(sum(sr^2)),
             n = n, d = d, method = "L1 median with spatial-rank check")
}

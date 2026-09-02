# SPDX-License-Identifier: AGPL-3.0-or-later
#' Iteratively reweighted mean converging on the geometric median
#'
#' The objective is convex but not differentiable at the data points, and
#' that is the difficulty: the update divides by the distance to each
#' point, so landing exactly on one blows up. A point whose distance
#' underflows is skipped. \code{tol} is accepted and ignored -- a fixed
#' iteration count is what makes the arms agree.
#'
#' Formula: \code{mu = (sum w_i x_i)/(sum w_i)},
#' \code{w_i = 1/||x_i - mu||}.
#'
#' @param X Points.
#' @param tol Ignored; interface compatibility only.
#' @param max_iter Iterations.
#' @return List with \code{estimate}, \code{cost}, \code{n}, \code{d}.
#' @references Weiszfeld, E. (1937). Tohoku Math J 43:355-386.
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' Weisz(V)
Weisz <- function(X, tol = NULL, max_iter = 200) {
  A <- as.matrix(X); n <- nrow(A); d <- ncol(A)
  mu <- colSums(A) / n
  for (it in seq_len(as.integer(max_iter))) {
    dist <- sqrt(rowSums((A - matrix(mu, n, d, byrow = TRUE))^2))
    ok <- dist >= 1e-12
    if (!any(ok)) next
    w <- 1 / dist[ok]
    mu <- colSums(A[ok, , drop = FALSE] * w) / sum(w)
  }
  cost <- sum(sqrt(rowSums((A - matrix(mu, n, d, byrow = TRUE))^2)))
  .t1_result(estimate = mu, cost = cost, n = n, d = d,
             method = "Weiszfeld iteration, geometric median")
}

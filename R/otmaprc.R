# SPDX-License-Identifier: AGPL-3.0-or-later
#' Brenier optimal transport map in one dimension
#'
#' Formula: T(x_(i)) = y_(i): the monotone rearrangement matching order statistic to order statistic
#'
#' @param x Source sample.
#' @param y Target sample, the same length.
#' @param p Cost exponent |x - y|^p.

#' @param x See Usage.
#' @param y See Usage.
#' @param p See Usage.
#' @return List with ``map`` (image of each input in its original order), ``cost``,
#' ``order_x``, ``order_y``, ``n``.
#' @references Brenier (1991), Polar factorization and monotone rearrangement of
#' vector-valued functions, Communications on Pure and Applied Mathematics 44:375-417.
#' Not held locally; the one-dimensional monotone-rearrangement solution is the standard
#' published result.
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' Brenier1d(V, V)
Brenier1d <- function(x, y, p = 2) {
  x <- .t1_vec(x)
  y <- .t1_vec(y)
  n <- length(x)
  if (length(y) != n) stop("x and y must have the same length")
  ox <- order(x, seq_len(n))
  oy <- order(y, seq_len(n))
  mp <- numeric(n)
  mp[ox] <- y[oy]
  .t1_result(map = mp, cost = sum(abs(x - mp)^as.numeric(p)) / n,
             order_x = ox - 1L, order_y = oy - 1L, n = n,
             method = "Brenier map in one dimension (monotone rearrangement)")
}

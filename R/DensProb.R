# SPDX-License-Identifier: AGPL-3.0-or-later

#' Interval probability from a density on a grid
#'
#' Trapezoid integration on a 513-point refinement of \[a, b\].  The
#' supplied density must integrate to within 5 percent of 1 over the
#' whole grid, which catches an unnormalised input.
#'
#' @param grid strictly increasing abscissae, length >= 2.
#' @param density non-negative density values on grid.
#' @param a,b interval bounds, grid\[1\] <= a <= b <= grid\[length(grid)\].
#' @return list(probability, a, b).
#' @references Morin, D. J. (2016). Probability: For the Enthusiastic
#'   Beginner. Createspace. Eq (4.2).
#' @examples
#' g <- seq(-6, 6, length.out = 1201); DensProb(g, stats::dnorm(g), -1, 1)$probability
#' @export
DensProb <- function(grid, density, a, b) {
  grid <- as.numeric(grid)
  density <- as.numeric(density)
  if (length(grid) != length(density) || length(grid) < 2L) {
    stop("grid and density must be equal-length vectors, n >= 2.", call. = FALSE)
  }
  if (any(diff(grid) <= 0)) stop("grid must be strictly increasing.", call. = FALSE)
  if (any(density < 0)) stop("density must be >= 0.", call. = FALSE)
  trapz <- function(y, x) sum(diff(x) * (y[-1] + y[-length(y)]) / 2)
  total <- trapz(density, grid)
  if (abs(total - 1) > 0.05) {
    stop(sprintf("density integrates to %.4f, not ~1.", total), call. = FALSE)
  }
  a <- as.numeric(a)
  b <- as.numeric(b)
  if (!(grid[1] <= a && a <= b && b <= grid[length(grid)])) {
    stop("need grid[1] <= a <= b <= grid[n].", call. = FALSE)
  }
  xs <- seq(a, b, length.out = 513)
  ys <- stats::approx(grid, density, xout = xs, rule = 2)$y
  list(probability = trapz(ys, xs), a = a, b = b)
}

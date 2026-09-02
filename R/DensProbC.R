# SPDX-License-Identifier: AGPL-3.0-or-later

#' Centered-interval probability from a density
#'
#' The interval is \[center - width/2, center + width/2\].
#'
#' @param grid,density the density on a strictly increasing grid.
#' @param center,width the interval centre and total width.
#' @return list(probability, center, width).
#' @references Morin, D. J. (2016). Probability: For the Enthusiastic
#'   Beginner. Createspace. Eq (4.4).
#' @examples
#' g <- seq(-6, 6, length.out = 1201); DensProbC(g, stats::dnorm(g), 0, 2)$probability
#' @export
DensProbC <- function(grid, density, center, width) {
  half <- as.numeric(width) / 2
  r <- DensProb(grid, density, as.numeric(center) - half, as.numeric(center) + half)
  list(probability = r$probability, center = as.numeric(center),
       width = as.numeric(width))
}

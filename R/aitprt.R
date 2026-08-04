# SPDX-License-Identifier: AGPL-3.0-or-later
#' Perturbation of one composition by another.
#'
#' Formula: x (+) y = C( x_1 y_1, ..., x_D y_D )
#'
#' @param x,y Strictly positive vectors of parts, the same length.
#' @param total Constant the result sums to.
#' @return List with \code{composition}, \code{total}, \code{D}.
#' @references Aitchison (1986), The Statistical Analysis of Compositional
#'   Data, Chapter 2. Verified against the reference implementation in the
#'   CRAN package compositions 2.0-9, whose perturbe is acomp(x * y) --
#'   the elementwise product, then closed.
#' @export
Perturb <- function(x, y, total = 1) {
  x <- .t1_vec(x); y <- .t1_vec(y)
  if (length(x) != length(y)) stop("x and y must have the same number of parts")
  if (any(x <= 0) || any(y <= 0)) stop("compositions must be strictly positive")
  p <- x * y
  k <- as.numeric(total)
  .t1_result(composition = k * p / sum(p), total = k, D = length(x),
             method = "Perturbation on the simplex")
}

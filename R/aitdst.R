# SPDX-License-Identifier: AGPL-3.0-or-later
#' Aitchison distance between two compositions
#'
#' Formula: d_a(x, y)^2 = (1/D) sum_\{i<j\} ( log(x_i/x_j) - log(y_i/y_j) )^2
#'
#' @param x Composition with strictly positive parts.
#' @param y Second composition, same length as x, strictly positive.
#'
#' @return List with ``distance``, ``distance2``, ``D``.
#' @references Aitchison, J. (1986), The Statistical Analysis of Compositional Data, Chapman and Hall, is this shelf's primary book and is NOT in the reference library, so it could not be read.  The definitions below were taken instead from Mateu-Figueras, G., Pawlowsky-Glahn, V. and Egozcue, J. J., The normal distribution in some constrained sample spaces, arXiv:0802.2643 (published as SORT 37(1):29-56, 2013), Sect. 4.1, which prints them with equation numbers and attributes them to Aitchison (1982, 1986); that paper was FETCHED and is archived in the reference library with a row in EXTERNAL_SOURCES.md.  The squared distance printed immediately after equation (10).  It is relative: it is unchanged by perturbing both arguments by the same composition, and scales by |a| under powering.
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' Compdist(V, V)
Compdist <- function(x, y) {
  x <- .t1_vec(x)
  y <- .t1_vec(y)
  D <- length(x)
  if (D != length(y)) stop("x and y must have the same number of parts")
  if (D < 2L) stop("a distance on the simplex needs at least two parts")
  if (any(x <= 0) || any(y <= 0)) stop("compositions must be strictly positive")
  lx <- log(x)
  ly <- log(y)
  tot <- 0
  for (i in seq_len(D - 1L)) {
    for (j in (i + 1L):D) {
      tot <- tot + ((lx[i] - lx[j]) - (ly[i] - ly[j]))^2
    }
  }
  d2 <- tot / D
  .t1_result(
    distance = sqrt(d2), distance2 = d2, D = D,
    method = "Aitchison distance"
  )
}

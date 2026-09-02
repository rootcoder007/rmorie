# SPDX-License-Identifier: AGPL-3.0-or-later
#' Aitchison norm of a composition
#'
#' Formula: ||x||_a = sqrt( (1/D) sum_\{i<j\} log(x_i/x_j)^2 )
#'
#' @param x Composition with strictly positive parts.
#'
#' @return List with ``norm``, ``norm2``, ``D``.
#' @references Aitchison, J. (1986), The Statistical Analysis of Compositional Data, Chapman and Hall, is this shelf's primary book and is NOT in the reference library, so it could not be read.  The definitions below were taken instead from Mateu-Figueras, G., Pawlowsky-Glahn, V. and Egozcue, J. J., The normal distribution in some constrained sample spaces, arXiv:0802.2643 (published as SORT 37(1):29-56, 2013), Sect. 4.1, which prints them with equation numbers and attributes them to Aitchison (1982, 1986); that paper was FETCHED and is archived in the reference library with a row in EXTERNAL_SOURCES.md.  The norm associated with equation (10): ||x||_a^2 = <x, x>_a.
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' Compnorm(V)
Compnorm <- function(x) {
  x <- .t1_vec(x); D <- length(x)
  if (D < 2L) stop("a norm on the simplex needs at least two parts")
  if (any(x <= 0)) stop("compositions must be strictly positive")
  lx <- log(x); tot <- 0
  for (i in seq_len(D - 1L)) for (j in (i + 1L):D) tot <- tot + (lx[i] - lx[j])^2
  n2 <- tot / D
  .t1_result(norm = sqrt(n2), norm2 = n2, D = D, method = "Aitchison norm")
}

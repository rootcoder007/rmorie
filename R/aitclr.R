# SPDX-License-Identifier: AGPL-3.0-or-later
#' Centred log-ratio transform of a composition
#'
#' Formula: clr(x)_i = log( x_i / g(x) ),  g(x) the geometric mean; sum_i clr(x)_i = 0
#'
#' @param x Composition with strictly positive parts.
#'
#' @return List with ``clr``, ``geomean``, ``D``, ``sum_check``.
#' @references Aitchison, J. (1986), The Statistical Analysis of Compositional Data, Chapman and Hall, is this shelf's primary book and is NOT in the reference library, so it could not be read.  The definitions below were taken instead from Mateu-Figueras, G., Pawlowsky-Glahn, V. and Egozcue, J. J., The normal distribution in some constrained sample spaces, arXiv:0802.2643 (published as SORT 37(1):29-56, 2013), Sect. 4.1, which prints them with equation numbers and attributes them to Aitchison (1982, 1986); that paper was FETCHED and is archived in the reference library with a row in EXTERNAL_SOURCES.md.  clr(x) = (ln(x_1/g(x)), ..., ln(x_D/g(x))).  The transform maps S^D onto the hyperplane of vectors summing to zero, so ``sum_check`` is zero up to rounding and is reported as a check on the caller's data rather than as a result.
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' Clr(V)
Clr <- function(x) {
  x <- .t1_vec(x)
  if (length(x) == 0L) stop("x must be non-empty")
  if (any(x <= 0)) stop("compositions must be strictly positive")
  lg <- mean(log(x)); z <- log(x) - lg
  .t1_result(clr = z, geomean = exp(lg), D = length(x), sum_check = sum(z),
             method = "Centred log-ratio transform")
}

# SPDX-License-Identifier: AGPL-3.0-or-later
#' Additive log-ratio transform of a composition against a reference part.
#'
#' Formula: alr(x)_i = log( x_i / x_ref ),  i != ref
#'
#' @param x Composition with strictly positive parts.
#' @param ref 1-based index of the reference part; the default D uses the last part, as in Aitchison's own display.
#'
#' @return List with ``alr``, ``ref``, ``parts``, ``D``.
#' @references Aitchison, J. (1986), The Statistical Analysis of Compositional Data, Chapman and Hall, is this shelf's primary book and is NOT in the reference library, so it could not be read.  The definitions below were taken instead from Mateu-Figueras, G., Pawlowsky-Glahn, V. and Egozcue, J. J., The normal distribution in some constrained sample spaces, arXiv:0802.2643 (published as SORT 37(1):29-56, 2013), Sect. 4.1, which prints them with equation numbers and attributes them to Aitchison (1982, 1986); that paper was FETCHED and is archived in the reference library with a row in EXTERNAL_SOURCES.md.  alr(x) = (ln(x_1/x_D), ..., ln(x_{D-1}/x_D)), the reference part being the last one.  ``ref`` generalises that to any part; ``parts`` reports which 1-based indices the returned coordinates belong to, in their original order.  The index is 1-based in BOTH language arms so the two agree.
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' Alr(V)
Alr <- function(x, ref = NULL) {
  x <- .t1_vec(x); D <- length(x)
  if (D < 2L) stop("an additive log-ratio needs at least two parts")
  if (any(x <= 0)) stop("compositions must be strictly positive")
  k <- if (is.null(ref)) D else as.integer(ref)
  if (k < 1L || k > D) stop("ref must be a 1-based part index")
  idx <- setdiff(seq_len(D), k)
  .t1_result(alr = log(x[idx]) - log(x[k]), ref = k, parts = idx, D = D,
             method = "Additive log-ratio transform")
}

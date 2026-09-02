# SPDX-License-Identifier: AGPL-3.0-or-later
#' Inverse centred log-ratio transform.
#'
#' Formula: clr^-1(z) = C( exp(z_1), ..., exp(z_D) )
#'
#' @param z Centred log-ratio coordinates, length D.
#' @param total Constant kappa the closure sums to.
#'
#' @return List with ``composition``, ``total``, ``D``.
#' @references Aitchison, J. (1986), The Statistical Analysis of Compositional Data, Chapman and Hall, is this shelf's primary book and is NOT in the reference library, so it could not be read.  The definitions below were taken instead from Mateu-Figueras, G., Pawlowsky-Glahn, V. and Egozcue, J. J., The normal distribution in some constrained sample spaces, arXiv:0802.2643 (published as SORT 37(1):29-56, 2013), Sect. 4.1, which prints them with equation numbers and attributes them to Aitchison (1982, 1986); that paper was FETCHED and is archived in the reference library with a row in EXTERNAL_SOURCES.md.  The inverse of clr is the closure of the exponentiated coordinates.  It is well defined for any real z, not only for z summing to zero: adding a constant to every z_i leaves the closed result unchanged.
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' Clrinv(V)
Clrinv <- function(z, total = 1) {
  z <- .t1_vec(z)
  if (length(z) == 0L) stop("z must be non-empty")
  e <- exp(z - max(z)); s <- sum(e); k <- as.numeric(total)
  .t1_result(composition = k * e / s, total = k, D = length(z),
             method = "Inverse centred log-ratio transform")
}

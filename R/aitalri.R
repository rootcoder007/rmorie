# SPDX-License-Identifier: AGPL-3.0-or-later
#' Inverse additive log-ratio transform
#'
#' Formula: alr^-1(y) = C( exp(y_1), ..., exp(y_\{D-1\}), 1 ) with the 1 inserted at the
#' reference position
#'
#' @param y Additive log-ratio coordinates, length D - 1.
#' @param ref 1-based index the reference part is restored to; the default is the last position D.
#' @param total Constant kappa the closure sums to.
#'
#' @return List with ``composition``, ``ref``, ``total``, ``D``.
#' @references Aitchison, J. (1986), The Statistical Analysis of Compositional Data,
#' Chapman and Hall, is this shelf's primary book and is NOT in the reference library, so
#' it could not be read.  The definitions below were taken instead from Mateu-Figueras,
#' G., Pawlowsky-Glahn, V. and Egozcue, J. J., The normal distribution in some
#' constrained sample spaces, arXiv:0802.2643 (published as SORT 37(1):29-56, 2013),
#' Sect. 4.1, which prints them with equation numbers and attributes them to Aitchison
#' (1982, 1986); that paper was FETCHED and is archived in the reference library with a
#' row in EXTERNAL_SOURCES.md.  Inverting alr puts a 1 back in the reference slot and
#' closes: the reference part is the one whose log-ratio against itself is zero.  ``ref``
#' must match the ``ref`` used in the forward transform for the round trip to return the
#' original composition.
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' Alrinv(V)
Alrinv <- function(y, ref = NULL, total = 1) {
  y <- .t1_vec(y)
  D <- length(y) + 1L
  if (length(y) == 0L) stop("y must be non-empty")
  k <- if (is.null(ref)) D else as.integer(ref)
  if (k < 1L || k > D) stop("ref must be a 1-based part index")
  full <- numeric(D)
  full[setdiff(seq_len(D), k)] <- y
  e <- exp(full - max(full))
  s <- sum(e)
  t <- as.numeric(total)
  .t1_result(
    composition = t * e / s, ref = k, total = t, D = D,
    method = "Inverse additive log-ratio transform"
  )
}

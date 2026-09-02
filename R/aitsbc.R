# SPDX-License-Identifier: AGPL-3.0-or-later
#' Form the subcomposition on a selected subset of parts
#'
#' \code{parts} is a vector of one-based part indices.
#'
#' Formula: sub(x; S) = C( x_i : i in S )
#'
#' @param x Strictly positive vector of parts.
#' @param parts One-based indices of the parts retained (at least two).
#' @param total Constant the subcomposition sums to.
#' @return List with \code{composition}, \code{parts}, \code{total},
#'   \code{D_sub}, \code{D}.
#' @references Aitchison (1986), The Statistical Analysis of Compositional
#'   Data, Chapter 2, where the subcomposition is defined as the closure
#'   of the selected subvector.
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' Subcomp(V, V)
Subcomp <- function(x, parts, total = 1) {
  x <- .t1_vec(x)
  if (any(x <= 0)) stop("compositions must be strictly positive")
  D <- length(x)
  idx <- as.integer(parts)
  if (length(idx) < 2L) stop("a subcomposition needs at least two parts")
  if (any(idx < 1L | idx > D)) stop("parts must be one-based indices in 1..D")
  sub <- x[idx]
  k <- as.numeric(total)
  .t1_result(composition = k * sub / sum(sub), parts = idx, total = k,
             D_sub = length(idx), D = D, method = "Subcomposition")
}

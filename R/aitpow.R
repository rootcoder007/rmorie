# SPDX-License-Identifier: AGPL-3.0-or-later
#' Power a composition by a real scalar
#'
#' Formula: a (.) x = C( x_1^a, ..., x_D^a )
#'
#' @param a Scalar.
#' @param x Strictly positive vector of parts.
#' @param total Constant the result sums to.
#' @return List with \code{composition}, \code{a}, \code{total}, \code{D}.
#' @references Aitchison (1986), The Statistical Analysis of Compositional
#'   Data, Chapter 2. Verified against the reference implementation in the
#'   CRAN package compositions 2.0-9, whose power operator on an acomp
#'   closes the elementwise power.
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' Powering(V, V)
Powering <- function(a, x, total = 1) {
  x <- .t1_vec(x)
  if (any(x <= 0)) stop("compositions must be strictly positive")
  a <- as.numeric(a)
  p <- x^a
  k <- as.numeric(total)
  .t1_result(
    composition = k * p / sum(p), a = a, total = k, D = length(x),
    method = "Powering on the simplex"
  )
}

# SPDX-License-Identifier: AGPL-3.0-or-later
#' Closure operator C(x) onto the simplex of constant sum \code{total}
#'
#' Formula: C(x) = kappa * (x_1, ..., x_D) / sum_j x_j
#'
#' @param x Strictly positive vector of parts.
#' @param total Constant kappa the closed vector sums to.
#' @return List with \code{closed}, \code{total}, \code{sum_raw}, \code{D}.
#' @references Aitchison (1986), The Statistical Analysis of Compositional
#'   Data, Chapter 2. Definition verified against the reference
#'   implementation in the CRAN package compositions 2.0-9 (van den
#'   Boogaart & Tolosana-Delgado), whose clo/acomp divide by the row sum
#'   and rescale to the requested total.
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' Compclos(V)
Compclos <- function(x, total = 1) {
  x <- .t1_vec(x)
  if (any(x <= 0)) stop("compositions must be strictly positive")
  s <- sum(x)
  k <- as.numeric(total)
  .t1_result(closed = k * x / s, total = k, sum_raw = s, D = length(x),
             method = "Closure C(x) onto the simplex")
}

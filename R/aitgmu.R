# SPDX-License-Identifier: AGPL-3.0-or-later
#' Geometric mean of the parts of a single composition.
#'
#' Formula: g(x) = (x_1 x_2 ... x_D)^(1/D) = exp( (1/D) sum_j log x_j )
#'
#' @param x Strictly positive vector of parts.
#' @return List with \code{geomean}, \code{log_geomean}, \code{D}.
#' @references Aitchison (1986), The Statistical Analysis of Compositional
#'   Data, Chapter 4, where g(x) is the normalising constant of the
#'   centred log-ratio transform.
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' Compgeo(V)
Compgeo <- function(x) {
  x <- .t1_vec(x)
  if (any(x <= 0)) stop("compositions must be strictly positive")
  D <- length(x)
  lg <- sum(log(x)) / D
  .t1_result(geomean = exp(lg), log_geomean = lg, D = D,
             method = "Geometric mean of a composition")
}

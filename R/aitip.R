# SPDX-License-Identifier: AGPL-3.0-or-later
#' Aitchison inner product on the simplex.
#'
#' Formula: <x, y>_a = sum_i clr(x)_i clr(y)_i
#'                   = (1/D) sum_{i<j} log(x_i/x_j) log(y_i/y_j)
#'
#' @param x,y Strictly positive vectors of parts, the same length.
#' @return List with \code{inner}, \code{inner_pairwise}, \code{cos_angle},
#'   \code{D}. The two inner products agree; both are returned as a
#'   self-check.
#' @references Aitchison (1986), The Statistical Analysis of Compositional
#'   Data, Chapter 4. Verified against the reference implementation in the
#'   CRAN package compositions 2.0-9, whose scalar applies the centred
#'   log-ratio to both arguments and sums the elementwise product.
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' Compip(V, V)
Compip <- function(x, y) {
  x <- .t1_vec(x); y <- .t1_vec(y)
  if (length(x) != length(y)) stop("x and y must have the same number of parts")
  if (any(x <= 0) || any(y <= 0)) stop("compositions must be strictly positive")
  D <- length(x)
  Lx <- log(x); Ly <- log(y)
  zx <- Lx - sum(Lx) / D
  zy <- Ly - sum(Ly) / D
  ip <- sum(zx * zy)
  pw <- 0
  for (i in seq_len(D)) for (j in seq_len(D)) if (j > i)
    pw <- pw + (Lx[i] - Lx[j]) * (Ly[i] - Ly[j])
  nx <- sqrt(sum(zx^2)); ny <- sqrt(sum(zy^2))
  cosang <- if (nx > 0 && ny > 0) ip / (nx * ny) else NaN
  .t1_result(inner = ip, inner_pairwise = pw / D, cos_angle = cosang, D = D,
             method = "Aitchison inner product")
}

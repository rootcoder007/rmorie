# SPDX-License-Identifier: AGPL-3.0-or-later
#' Mean of a compositional sample taken in log-ratio coordinates
#'
#' Formula: mean clr = (1/n) sum_k clr(x_k); centre = clr^-1(mean clr)
#'
#' @param X One composition per row; strictly positive.
#' @param total Constant the returned centre sums to.
#' @return List with \code{clr_mean}, \code{center}, \code{sum_clr_mean},
#'   \code{n}, \code{D}.
#' @references Aitchison (1986), The Statistical Analysis of Compositional
#'   Data, Chapter 4. Consistent with the sibling module aitcen in this
#'   package, whose centre is the closed vector of column geometric means.
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' Complrm(V)
Complrm <- function(X, total = 1) {
  X <- as.matrix(X)
  if (any(X <= 0)) stop("compositions must be strictly positive")
  n <- nrow(X)
  D <- ncol(X)
  L <- log(X)
  Z <- L - rowSums(L) / D
  zm <- colMeans(Z)
  e <- exp(zm)
  k <- as.numeric(total)
  .t1_result(
    clr_mean = zm, center = k * e / sum(e), sum_clr_mean = sum(zm),
    n = n, D = D,
    method = "Log-ratio mean (clr average, closed back)"
  )
}

# SPDX-License-Identifier: AGPL-3.0-or-later
#' Variation matrix: the pairwise log-ratio variances
#'
#' Sample variances (denominator n-1) throughout, matching the sibling
#' modules aitcen and aittvr.
#'
#' Formula: tau_ij = var( log(x_i / x_j) ), totvar = (1/D) sum_\{i<j\} tau_ij
#'
#' @param X One composition per row; strictly positive.
#' @return List with \code{variation} (D x D, zero diagonal),
#'   \code{totvar}, \code{n}, \code{D}.
#' @references Aitchison (1986), The Statistical Analysis of Compositional
#'   Data, Chapter 4. The relation totvar = (1/D) sum_\{i<j\} tau_ij is the
#'   one already used by the sibling module aittvr in this package.
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' Compvar(V)
Compvar <- function(X) {
  X <- as.matrix(X)
  if (any(X <= 0)) stop("compositions must be strictly positive")
  n <- nrow(X)
  D <- ncol(X)
  L <- log(X)
  tau <- matrix(0, D, D)
  tot <- 0
  for (i in seq_len(D)) {
    for (j in seq_len(D)) {
      if (j > i) {
        v <- stats::var(L[, i] - L[, j])
        tau[i, j] <- v
        tau[j, i] <- v
        tot <- tot + v
      }
    }
  }
  .t1_result(
    variation = tau, totvar = tot / D, n = n, D = D,
    method = "Compositional variation matrix"
  )
}

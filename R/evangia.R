# SPDX-License-Identifier: AGPL-3.0-or-later

#' Empirical angular measure of the k largest radii
#'
#' Formula: H_n(B) = (1/k) sum 1\{||X_i|| >= r_n, X_i/||X_i|| in B\}
#'
#' The data are first rank-transformed to standard Frechet margins, so
#' the radial and angular parts separate.  Each of the k largest radii
#' contributes an atom of mass 1/k at its angle w = X_1/(X_1 + X_2),
#' which makes H a probability measure by construction; the mean
#' constraint of a valid angular measure is mean(w) = 1/2.
#'
#' @param X An n x 2 matrix of observations.
#' @param k Number of upper order statistics of the radius to keep.
#' @return List with \code{H}, \code{atoms}, \code{weights},
#'   \code{estimate} (mean angle), \code{n_used}, \code{n},
#'   \code{method}.
#' @references Einmahl, de Haan & Sinha (1997), Stoch. Proc. Appl.
#'   70(2):143-171.
#' @export
#' @examples
#' set.seed(1)
#' Evangia(matrix(rnorm(40), 20, 2), k = 2)
Evangia <- function(X, k) {
  M <- .s03mat(X)
  n <- nrow(M)
  if (n == 0L) stop("empty input: X has no rows")
  if (ncol(M) != 2L) stop("X must have exactly two columns")
  k <- as.integer(k)
  if (k < 1L || k > n) stop("k must lie between 1 and the number of rows")
  r0 <- .s03rank(M[, 1])
  r1 <- .s03rank(M[, 2])
  f0 <- (n + 1) / (n + 1 - r0)
  f1 <- (n + 1) / (n + 1 - r1)
  rad <- f0 + f1
  idx <- order(-rad, seq_len(n))[seq_len(k)]
  atoms <- sort(f0[idx] / rad[idx])
  w <- rep(1 / k, k)
  .t1_result(H = sum(w), atoms = atoms, weights = w,
             estimate = sum(atoms) / k, n_used = k, n = n,
             method = "empirical angular measure of the k largest radii")
}

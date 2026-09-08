# SPDX-License-Identifier: AGPL-3.0-or-later
#' Eigenvalues of the normalised Laplacian, in increasing order
#'
#' The spectrum lies in \[0, 2\] for every graph, and the multiplicity of
#' the eigenvalue 0 is the number of connected components.
#'
#' Formula: spectrum of Lcal = T^-1/2 (T - W) T^-1/2,
#'   lambda_0 = 0 <= lambda_1 <= ... <= lambda_\{n-1\} <= 2
#'
#' @param W Symmetric non-negative weight matrix.
#' @param tol An eigenvalue below this counts as zero.
#' @return List with \code{values} (increasing), \code{lambda1},
#'   \code{n_components}, \code{max_value}, \code{n}.
#' @references Chung (1997), Spectral Graph Theory, CBMS 92, Sections
#'   1.2-1.3: the eigenvalues of Lcal lie in \[0, 2\] and the multiplicity
#'   of 0 equals the number of connected components. Fetched from the
#'   author's own copy of the chapter.
#' @export
#' @examples
#' Lapspec(W = 5L)
Lapspec <- function(W, tol = 1e-10) {
  W <- as.matrix(W)
  n <- nrow(W)
  if (ncol(W) != n) stop("W must be square")
  d <- rowSums(W)
  s <- ifelse(d == 0, 0, 1 / sqrt(ifelse(d == 0, 1, d)))
  L <- -W
  diag(L) <- d - diag(W)
  Lc <- diag(s, n) %*% L %*% diag(s, n)
  vals <- rev(.t1_eigsym(Lc)$values)
  nz <- vals[vals > tol]
  .t1_result(values = vals,
             lambda1 = if (length(nz)) nz[1] else NaN,
             n_components = sum(vals <= tol), max_value = vals[n], n = n,
             method = "Spectrum of the normalised Laplacian")
}

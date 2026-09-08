# SPDX-License-Identifier: AGPL-3.0-or-later
#' Eigenvectors of the normalised Laplacian for the k smallest values
#'
#' Eigenvectors are sign-fixed on their largest-magnitude entry, without
#' which the two language arms would disagree on an arbitrary sign. Under
#' a repeated eigenvalue the individual vectors remain arbitrary within
#' their eigenspace; only the eigenvalues and the projector are defined.
#'
#' Formula: Lcal u = lambda u, lambda_0 <= ... <= lambda_\{k-1\};
#'   Fiedler vector = u for the smallest lambda > tol
#'
#' @param W Symmetric non-negative weight matrix.
#' @param k Number of eigenpairs returned (1 <= k <= n).
#' @param tol An eigenvalue below this counts as zero.
#' @return List with \code{values}, \code{vectors}, \code{fiedler},
#'   \code{lambda1}, \code{n_components}, \code{n}, \code{k}.
#' @references Chung (1997), Spectral Graph Theory, CBMS 92, Section 1.2,
#'   which writes Lcal as an operator on functions g : V -> R and works
#'   with its harmonic eigenfunctions. Fetched from the author's own copy
#'   of the chapter.
#' @export
#' @rdname Lapeig-laplmo
Lapeig <- function(W, k = 2, tol = 1e-10) {
  W <- as.matrix(W)
  n <- nrow(W)
  if (ncol(W) != n) stop("W must be square")
  k <- as.integer(k)
  if (k < 1L || k > n) stop("k must satisfy 1 <= k <= n")
  d <- rowSums(W)
  s <- ifelse(d == 0, 0, 1 / sqrt(ifelse(d == 0, 1, d)))
  L <- -W
  diag(L) <- d - diag(W)
  e <- .t1_eigsym(diag(s, n) %*% L %*% diag(s, n))
  ord <- rev(seq_len(n))
  vals <- e$values[ord]
  V <- e$vectors[, ord[seq_len(k)], drop = FALSE]
  nzi <- which(vals > tol)
  fied <- if (length(nzi)) e$vectors[, ord[nzi[1]]] else rep(0, n)
  .t1_result(values = vals[seq_len(k)], vectors = V, fiedler = fied,
             lambda1 = if (length(nzi)) vals[nzi[1]] else NaN,
             n_components = sum(vals <= tol), n = n, k = k,
             method = "Normalised-Laplacian eigenvectors (sign-fixed)")
}

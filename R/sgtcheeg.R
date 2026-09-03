# SPDX-License-Identifier: AGPL-3.0-or-later
#' Sgtcheegerconstant constant by a sweep over the Fiedler vector
#'
#' Formula: h(G) = min_S |boundary(S)| / min(vol S, vol S^c); lambda_2/2 <= h(G) <=
#' sqrt(2 lambda_2) for the NORMALISED Laplacian I - D^-1/2 A D^-1/2
#'
#' @param A Symmetric non-negative adjacency matrix.

#' @param A See Usage.
#' @return List with ``sweep_min``, ``lower_bound`` (lambda_2/2), ``upper_bound`` (sqrt(2
#' lambda_2)), ``lambda2``, ``cut_set``, ``fiedler``, ``n``.
#' @references Sgtcheegerconstant (1970), A lower bound for the smallest eigenvalue of
#' the Laplacian, in Problems in Analysis; Chung (1997), Spectral Graph Theory, AMS.
#' Neither is held locally; the conductance definition and the sweep-cut construction are
#' standard published results. The sweep value is checked against exhaustive enumeration
#' over all subsets in the batch's anchor file.
#' @export
#' @examples
#' A <- matrix(c(4, 1, 0.5, 1, 3, 0.8, 0.5, 0.8, 2), nrow = 3)
#' res <- Sgtcheegerconstant(A = A)
#' res
Sgtcheegerconstant <- function(A) {
  A <- as.matrix(A)
  n <- nrow(A)
  diag(A) <- 0
  deg <- rowSums(A)
  if (any(deg <= 0)) stop("isolated vertices: conductance is undefined")
  ds <- 1 / sqrt(deg)
  L <- diag(n) - (ds %o% ds) * A
  e <- .t1_eigsym(L)
  lam2 <- e$values[n - 1]
  f <- e$vectors[, n - 1] * ds
  ord <- order(f, seq_len(n))
  total <- sum(deg)
  best <- Inf
  bestset <- integer(0)
  for (k in seq_len(n - 1)) {
    Sset <- ord[seq_len(k)]
    cut <- sum(A[Sset, -Sset, drop = FALSE])
    vol <- sum(deg[Sset])
    den <- min(vol, total - vol)
    if (den > 0) {
      val <- cut / den
      if (val < best) { best <- val
      bestset <- sort(Sset) }
    }
  }
  .t1_result(sweep_min = best, lower_bound = lam2 / 2,
             upper_bound = sqrt(2 * lam2), lambda2 = lam2,
             cut_set = bestset - 1L, fiedler = f, n = n,
             method = "Sgtcheegerconstant constant (Fiedler sweep upper bound)")
}

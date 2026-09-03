# SPDX-License-Identifier: AGPL-3.0-or-later
#' Lloyd k-means from a deterministic furthest-point seeding (internal)
#'
#' Random restarts are what k-means normally needs and also what would
#' make the two language arms disagree. Seeding from row 1 and then
#' repeatedly taking the row furthest from every chosen centre is
#' deterministic, and ties break to the lowest index in both arms.
#'
#' @param rows Numeric matrix, one point per row.
#' @param k Number of clusters.
#' @param iters Maximum Lloyd iterations.
#' @return List with \code{labels} (0-based) and \code{centres}.
#' @keywords internal
#' @examples
#' set.seed(1)
#' r <- rmorie:::.sgt_kmeans_det(rows = rnorm(10), k = 8L); TRUE
.sgt_kmeans_det <- function(rows, k, iters = 100L) {
  rows <- as.matrix(rows)
  n <- nrow(rows)
  p <- ncol(rows)
  centres <- matrix(rows[1, ], nrow = 1L)
  while (nrow(centres) < k) {
    best <- -1L
    bestd <- -1
    for (i in seq_len(n)) {
      dmin <- -1
      for (m in seq_len(nrow(centres))) {
        s <- sum((rows[i, ] - centres[m, ])^2)
        if (dmin < 0 || s < dmin) dmin <- s
      }
      if (dmin > bestd + 1e-12) { bestd <- dmin
      best <- i }
    }
    centres <- rbind(centres, rows[best, ])
  }
  lab <- integer(n)
  for (it in seq_len(iters)) {
    changed <- FALSE
    for (i in seq_len(n)) {
      best <- 0L
      bestd <- -1
      for (m in seq_len(k)) {
        s <- sum((rows[i, ] - centres[m, ])^2)
        if (bestd < 0 || s < bestd - 1e-12) { bestd <- s
        best <- m - 1L }
      }
      if (lab[i] != best) changed <- TRUE
      lab[i] <- best
    }
    for (m in seq_len(k)) {
      idx <- which(lab == m - 1L)
      if (length(idx)) centres[m, ] <- colSums(rows[idx, , drop = FALSE]) / length(idx)
    }
    if (!changed) break
  }
  list(labels = lab, centres = centres)
}

#' Ng-Jordan-Weiss k-way spectral clustering
#'
#' Take the \code{k} eigenvectors of \code{L_sym} with the smallest
#' eigenvalues, normalise each ROW of that n-by-k matrix to unit length,
#' and run k-means on the rows. The row normalisation projects nodes onto
#' the unit sphere, where well-separated blocks become mutually
#' orthogonal directions rather than clusters of different radius.
#'
#' Formula: \code{L_sym = I - D^-1/2 A D^-1/2}; \code{U} = its \code{k}
#' lowest eigenvectors; \code{T_ij = U_ij / ||U_i||}; k-means on rows of T.
#'
#' @param A Symmetric non-negative weight matrix, positive degrees.
#' @param k Number of clusters, \code{1 <= k <= n}.
#' @return List with \code{labels} (0-based), \code{eigvecs}, \code{eigvals},
#'   \code{k}, \code{n}.
#' @references Ng, A. Y., Jordan, M. I. & Weiss, Y. (2002). On spectral
#'   clustering: analysis and an algorithm. Advances in Neural
#'   Information Processing Systems 14, pages 849-856, MIT Press.
#' @export
#' @examples
#' A <- matrix(c(4, 1, 0.5, 1, 3, 0.8, 0.5, 0.8, 2), nrow = 3)
#' res <- Sgtsck(A = A)
#' res
Sgtsck <- function(A, k = 2) {
  s <- .sgt_lapsym(A, "Sgtsck")
  n <- s$n
  k <- as.integer(k)
  if (k < 1L || k > n) stop("Sgtsck: need 1 <= k <= n")
  e <- .s03jacobi(s$L)
  U <- e$vectors[, seq_len(k), drop = FALSE]
  Tm <- U
  for (i in seq_len(n)) {
    nrm <- sqrt(sum(U[i, ]^2))
    if (nrm > 0) Tm[i, ] <- U[i, ] / nrm
  }
  km <- .sgt_kmeans_det(Tm, k)
  .t1_result(labels = km$labels, eigvecs = Tm, eigvals = e$values[seq_len(k)],
             k = k, n = n,
             method = "Ng-Jordan-Weiss k-way spectral clustering")
}

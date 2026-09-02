# SPDX-License-Identifier: AGPL-3.0-or-later
#' Laplacian eigenmap embedding
#'
#' Embeds graph nodes by the low eigenvectors of the generalized
#' Laplacian problem \eqn{L f = \lambda D f} with \eqn{L = D - W}
#' (source Sec. 2, algorithm step 3), dropping the trivial constant
#' eigenvector at \eqn{\lambda_0 = 0}. Solved through the equivalent
#' symmetric problem \eqn{L_{sym} = D^{-1/2} L D^{-1/2}} whose
#' eigenvectors v give \eqn{f = D^{-1/2} v}, using a deterministic
#' cyclic Jacobi rotation identical to the Python arm (so both arms
#' return bit-comparable embeddings; LAPACK eigenvector conventions are
#' not relied upon). Sign convention: each eigenvector is scaled so its
#' largest-magnitude entry is positive.
#'
#' @param A Symmetric non-negative weight matrix; every node needs
#'   positive degree.
#' @param k Embedding dimension.
#' @return List with embedding (n by k), eigenvalues, all_eigenvalues
#'   (ascending), k, n.
#' @references Belkin, M. and Niyogi, P. (2003). Laplacian eigenmaps
#'   for dimensionality reduction and data representation. Neural
#'   Computation, 15(6), 1373-1396, Secs. 2-3. Archived:
#'   fetched-wave3/belkin-niyogi-2003-laplacian-eigenmaps.pdf.
#' @examples
#' C4 <- matrix(0, 4, 4)
#' C4[cbind(1:4, c(2, 3, 4, 1))] <- 1; C4 <- C4 + t(C4)
#' LapEig(C4, k = 2)
#' @export
LapEig <- function(A, k = 2L) {
  W <- as.matrix(A)
  n <- nrow(W)
  if (ncol(W) != n) stop("A must be square")
  k <- as.integer(k)
  if (k < 1L || k >= n) stop("k must satisfy 1 <= k < n")
  d <- rowSums(W)
  if (any(d <= 0)) stop("every node must have positive degree")
  dis <- 1 / sqrt(d)
  Lsym <- diag(n) - (dis %o% dis) * W
  Lsym <- (Lsym + t(Lsym)) / 2

  jacobi <- function(S, max_sweeps = 100L, tol = 1e-13) {
    a <- S
    V <- diag(n)
    for (sweep in seq_len(max_sweeps)) {
      off <- 0
      for (p in seq_len(n - 1L)) {
        for (q in (p + 1L):n) off <- off + a[p, q]^2
      }
      if (off <= tol * tol) break
      for (p in seq_len(n - 1L)) {
        for (q in (p + 1L):n) {
          apq <- a[p, q]
          if (abs(apq) <= 1e-300) next
          theta <- (a[q, q] - a[p, p]) / (2 * apq)
          t0 <- (if (theta >= 0) 1 else -1) / (abs(theta) + sqrt(theta^2 + 1))
          cc <- 1 / sqrt(t0^2 + 1)
          ss <- t0 * cc
          for (kk in seq_len(n)) {
            akp <- a[kk, p]
            akq <- a[kk, q]
            a[kk, p] <- cc * akp - ss * akq
            a[kk, q] <- ss * akp + cc * akq
          }
          for (kk in seq_len(n)) {
            apk <- a[p, kk]
            aqk <- a[q, kk]
            a[p, kk] <- cc * apk - ss * aqk
            a[q, kk] <- ss * apk + cc * aqk
          }
          for (kk in seq_len(n)) {
            vkp <- V[kk, p]
            vkq <- V[kk, q]
            V[kk, p] <- cc * vkp - ss * vkq
            V[kk, q] <- ss * vkp + cc * vkq
          }
        }
      }
    }
    list(values = diag(a), vectors = V)
  }

  eg <- jacobi(Lsym)
  ord <- order(eg$values)
  lam <- eg$values[ord]
  emb <- matrix(0, n, k)
  out_vals <- numeric(k)
  for (cidx in seq_len(k)) {
    idx <- ord[cidx + 1L]
    f <- dis * eg$vectors[, idx]
    big <- 1L
    for (r in 2:n) if (abs(f[r]) > abs(f[big]) + 1e-15) big <- r
    sgn <- if (f[big] >= 0) 1 else -1
    emb[, cidx] <- sgn * f
    out_vals[cidx] <- lam[cidx + 1L]
  }
  list(embedding = emb, eigenvalues = out_vals, all_eigenvalues = lam,
       k = k, n = n,
       method = "Laplacian eigenmap, generalized L f = lambda D f via Jacobi")
}

#' @rdname LapEig
#' @export
laplacian_eigenmaps <- LapEig

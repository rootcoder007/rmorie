# SPDX-License-Identifier: AGPL-3.0-or-later
#' Isomap: classical MDS on geodesic distances
#'
#' Tenenbaum, de Silva and Langford (2000), A global geometric framework
#' for nonlinear dimensionality reduction, Science 290(5500), 2319-2323.
#' The three steps printed there are: build the k-nearest-neighbour graph
#' with Euclidean edge weights; estimate the geodesics by shortest paths,
#' "computed by Floyd's algorithm"; and apply classical MDS to them, i.e.
#' double-centre the squared distances, tau(D) = -H S H / 2 with S_ij =
#' D_ij^2 and H = I - (1/n) 1 1', and take the top-d eigenvectors scaled
#' by the square roots of their eigenvalues.  The Science paper is
#' paywalled; the steps and the double-centring are quoted in their
#' standard published form.  Eigenvectors are sign-fixed, since an MDS
#' embedding is determined only up to reflection.  Unreachable pairs are
#' counted and reported rather than silently patched.
#'
#' @param X point cloud, one row per point.
#' @param k_nn neighbours per point.
#' @param dim embedding dimension.
#' @return list: Y, estimate, eigvals, n_infinite, method.
#' @keywords internal
#' @examples
#' Isomapmds(matrix(c(0, 0, 1, 0, 2, 0, 3, 0), 4, 2, byrow = TRUE), 2, 1)$eigvals
#' @export
Isomapmds <- function(X, k_nn = 3, dim = 2) {
  P <- .s03mat(X)
  n <- nrow(P)
  D <- matrix(0, n, n)
  for (i in seq_len(n)) for (j in seq_len(n)) {
    s <- 0
    for (a in seq_len(ncol(P))) { dd <- P[i, a] - P[j, a]
    s <- s + dd * dd }
    D[i, j] <- sqrt(s)
  }
  G <- matrix(Inf, n, n)
  kk <- as.integer(k_nn)
  for (i in seq_len(n)) {
    G[i, i] <- 0
    ord <- order(D[i, ], seq_len(n))
    up <- min(kk + 1L, n)
    if (up >= 2L) for (t in seq(2L, up)) {
      j <- ord[t]
      G[i, j] <- D[i, j]
      G[j, i] <- D[i, j]
    }
  }
  for (m in seq_len(n)) for (i in seq_len(n)) for (j in seq_len(n)) {
    if (G[i, m] + G[m, j] < G[i, j]) G[i, j] <- G[i, m] + G[m, j]
  }
  ninf <- 0L
  for (i in seq_len(n)) for (j in seq_len(n)) {
    if (is.infinite(G[i, j])) { ninf <- ninf + 1L
    G[i, j] <- 0 }
  }
  S <- G * G
  rm_ <- numeric(n)
  for (i in seq_len(n)) { s <- 0
  for (j in seq_len(n)) s <- s + S[i, j]
  rm_[i] <- s / n }
  gm <- 0
  for (v in rm_) gm <- gm + v / n
  B <- matrix(0, n, n)
  for (i in seq_len(n)) for (j in seq_len(n)) B[i, j] <- -0.5 * (S[i, j] - rm_[i] - rm_[j] + gm)
  eg <- .s03jacobi(B)
  vals <- eg$values
  vecs <- eg$vectors
  d <- as.integer(dim)
  if (d > n) d <- n
  ev <- numeric(d)
  for (t in seq_len(d)) ev[t] <- vals[n - t + 1L]
  Y <- matrix(0, n, d)
  for (t in seq_len(d)) {
    lam <- ev[t]
    s <- if (lam > 0) sqrt(lam) else 0
    for (i in seq_len(n)) Y[i, t] <- vecs[i, n - t + 1L] * s
  }
  list(Y = Y, estimate = if (d) ev[1] else NaN, eigvals = ev,
       n_infinite = ninf,
       method = "Isomap: k-NN graph, Floyd geodesics, classical MDS (Tenenbaum et al. 2000)")
}

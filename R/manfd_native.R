# SPDX-License-Identifier: AGPL-3.0-or-later
# R arm of manfd -- functional manifold learning. Mirrors
# src/morie/fn/manfd.py operation for operation, on the shared numerics
# in R/aaa_helpers_w3num.R.
#
# Functional data often lives on a low-dimensional manifold inside the
# infinite-dimensional space of curves. A family of growth curves
# differing only in the timing of a growth spurt is a ONE-parameter
# family, but the straight-line distance between two such curves says
# almost nothing about how far apart their timings are: shift a peak far
# enough and the L2 distance saturates, because the curves stop
# overlapping at all and moving further apart cannot make them any more
# different.
#
# That saturation is why linear methods mislead here. The fix is to
# measure distance ALONG the set instead of through the ambient space:
# build a neighbourhood graph, take shortest paths through it as
# geodesic distances, and embed those. Near neighbours are where the
# straight-line distance is trustworthy, and the graph stitches those
# local truths into a global one.
#
# The eigendecomposition is a Jacobi sweep written out here rather than
# a call to eigen(). That is deliberate: eigenvectors are only defined
# up to sign and up to rotation within a repeated eigenvalue, so two
# library routines can both be correct and disagree, and a module whose
# output flips sign depending on which linear algebra it was linked
# against is not reproducible. The sweep count is fixed, the ordering is
# by descending eigenvalue with ties broken by index, and each vector is
# signed so its largest-magnitude entry is positive.
#
# A neighbourhood graph that is not connected is the failure this method
# actually hits, and it is reported rather than patched. If k is too
# small the graph falls into pieces, the geodesic between pieces is
# infinite, and there is no embedding -- the honest response is to say
# so and name how many components there were, not to substitute a large
# finite number and produce coordinates that mean nothing.
#
# References
#   Chen, D. and Mueller, H.-G. (2012) "Nonlinear manifold
#     representations for functional data." The Annals of Statistics
#     40(1), 1-29. doi:10.1214/11-AOS936.
#   Tenenbaum, J.B., de Silva, V. and Langford, J.C. (2000) "A global
#     geometric framework for nonlinear dimensionality reduction."
#     Science 290(5500), 2319-2323.
#   Torgerson, W.S. (1952) "Multidimensional scaling: I. Theory and
#     method." Psychometrika 17(4), 401-419.
#   Floyd, R.W. (1962) "Algorithm 97: shortest path." Communications of
#     the ACM 5(6), 345.
#   Jacobi, C.G.J. (1846) Journal fuer die reine und angewandte
#     Mathematik 30, 51-94.

.MANFD_METHODS <- c("isomap", "mds", "geodesic_only")

#' Pairwise L2 distance between curves, by the trapezoid rule
#'
#' The trapezoid rather than a plain sum of squares: functional data is
#' a sample of a function, and the distance between two functions is an
#' integral.
#'
#' @param Y A matrix with one curve per row.
#' @param grid The sampling points, or NULL for the integers.
#' @return The distance matrix.
#' @export
morie_manfd_l2 <- function(Y, grid = NULL) {
  Y <- as.matrix(Y)
  storage.mode(Y) <- "double"
  n <- nrow(Y)
  p <- ncol(Y)
  if (is.null(grid)) grid <- as.numeric(seq_len(p) - 1L)
  grid <- as.numeric(grid)
  if (length(grid) != p) stop("the grid must match the curve length")
  if (p > 1L) for (t in seq_len(p - 1L))
    if (grid[t + 1L] <= grid[t])
      stop("the grid must be strictly increasing")
  D <- matrix(0, n, n)
  for (i in seq_len(n)) if (i < n) for (j in (i + 1L):n) {
    terms <- numeric(0)
    if (p > 1L) for (t in seq_len(p - 1L)) {
      a <- Y[i, t] - Y[j, t]
      b <- Y[i, t + 1L] - Y[j, t + 1L]
      terms <- c(terms, 0.5 * (a * a + b * b) * (grid[t + 1L] - grid[t]))
    }
    v <- if (length(terms)) sqrt(.w3_csum(terms)) else 0
    D[i, j] <- v
    D[j, i] <- v
  }
  D
}

#' The k-nearest-neighbour graph, as a weighted adjacency matrix
#'
#' Symmetrised by union: i and j are joined if EITHER lists the other.
#' Without that the graph is directed and the shortest path between two
#' points can depend on which way you walk it, which a distance may not
#' do.
#'
#' @param D A distance matrix.
#' @param k Neighbours per point.
#' @param symmetric Whether to symmetrise by union.
#' @return The adjacency matrix, infinite where there is no edge.
#' @export
morie_manfd_knn <- function(D, k, symmetric = TRUE) {
  n <- nrow(D)
  k <- as.integer(k)
  if (k < 1L) stop("each point needs at least one neighbour")
  if (k >= n) stop("k must be smaller than the sample size")
  A <- matrix(Inf, n, n)
  for (i in seq_len(n)) {
    A[i, i] <- 0
    ord <- order(D[i, ], seq_len(n))
    taken <- 0L
    for (j in ord) {
      if (j == i) next
      A[i, j] <- D[i, j]
      taken <- taken + 1L
      if (taken >= k) break
    }
  }
  if (symmetric) for (i in seq_len(n)) for (j in seq_len(n))
    if (A[j, i] < A[i, j]) A[i, j] <- A[j, i]
  A
}

#' All-pairs shortest paths by Floyd-Warshall, and the components
#'
#' An unreachable pair keeps an infinite distance, which is the truth
#' about it.
#'
#' @param A An adjacency matrix.
#' @return A list with the geodesic matrix and the component count.
#' @export
morie_manfd_paths <- function(A) {
  n <- nrow(A)
  G <- A
  for (m in seq_len(n)) for (i in seq_len(n)) {
    if (is.infinite(G[i, m])) next
    for (j in seq_len(n)) {
      if (is.infinite(G[m, j])) next
      v <- G[i, m] + G[m, j]
      if (v < G[i, j]) G[i, j] <- v
    }
  }
  seen <- rep(FALSE, n)
  comp <- 0L
  for (i in seq_len(n)) {
    if (seen[i]) next
    comp <- comp + 1L
    for (j in seq_len(n)) if (is.finite(G[i, j])) seen[j] <- TRUE
  }
  list(G = G, components = comp)
}

#' Symmetric eigendecomposition by cyclic Jacobi rotations
#'
#' Written out rather than delegated because eigenvectors are defined
#' only up to sign, and up to rotation inside a repeated eigenvalue, so
#' two correct library routines can disagree. A fixed number of sweeps,
#' a fixed ordering and a fixed sign convention make the answer a
#' function of the matrix alone.
#'
#' @param A A symmetric matrix.
#' @param sweeps The number of sweeps.
#' @return A list with descending eigenvalues and their vectors as
#'   columns, each signed so its largest-magnitude entry is positive.
#' @export
morie_manfd_jacobi <- function(A, sweeps = 60L) {
  n <- nrow(A)
  a <- as.matrix(A)
  storage.mode(a) <- "double"
  v <- diag(1, n, n)
  for (it in seq_len(as.integer(sweeps))) {
    off <- 0
    for (i in seq_len(n)) if (i < n) for (j in (i + 1L):n)
      off <- off + a[i, j] * a[i, j]
    if (off <= 1e-30) break
    if (n > 1L) for (p in seq_len(n - 1L)) for (q in (p + 1L):n) {
      if (abs(a[p, q]) <= 1e-300) next
      theta <- (a[q, q] - a[p, p]) / (2 * a[p, q])
      t <- (if (theta >= 0) 1 else -1) /
        (abs(theta) + sqrt(theta * theta + 1))
      cc <- 1 / sqrt(t * t + 1)
      s <- t * cc
      for (r in seq_len(n)) {
        arp <- a[r, p]
        arq <- a[r, q]
        a[r, p] <- cc * arp - s * arq
        a[r, q] <- s * arp + cc * arq
      }
      for (r in seq_len(n)) {
        apr <- a[p, r]
        aqr <- a[q, r]
        a[p, r] <- cc * apr - s * aqr
        a[q, r] <- s * apr + cc * aqr
      }
      for (r in seq_len(n)) {
        vrp <- v[r, p]
        vrq <- v[r, q]
        v[r, p] <- cc * vrp - s * vrq
        v[r, q] <- s * vrp + cc * vrq
      }
    }
  }
  vals <- vapply(seq_len(n), function(i) a[i, i], numeric(1))
  ord <- order(-vals, seq_len(n))
  ev <- vals[ord]
  vec <- v[, ord, drop = FALSE]
  # Sign convention: the largest-magnitude entry of each vector is made
  # positive. Without it the embedding could come out mirrored for no
  # reason a reader could see.
  for (j in seq_len(n)) {
    best <- 1L
    for (r in seq_len(n)) if (abs(vec[r, j]) > abs(vec[best, j])) best <- r
    if (vec[best, j] < 0) vec[, j] <- -vec[, j]
  }
  list(values = ev, vectors = vec)
}

#' Torgerson scaling: double-centre the squared distances, embed
#'
#' A negative eigenvalue means the distances were not Euclidean, which
#' happens routinely with geodesics and is reported rather than silently
#' clipped away.
#'
#' @param D A distance matrix.
#' @param dim The embedding dimension.
#' @param sweeps Jacobi sweeps.
#' @return A list with the coordinates, the eigenvalues, the count of
#'   negative eigenvalues and the centred matrix.
#' @export
morie_manfd_scaling <- function(D, dim = 2L, sweeps = 60L) {
  n <- nrow(D)
  d2 <- D * D
  rmean <- vapply(seq_len(n), function(i) .w3_csum(d2[i, ]) / n, numeric(1))
  cmean <- vapply(seq_len(n), function(j) .w3_csum(d2[, j]) / n, numeric(1))
  gmean <- .w3_csum(rmean) / n
  B <- matrix(0, n, n)
  for (i in seq_len(n)) for (j in seq_len(n))
    B[i, j] <- -0.5 * (d2[i, j] - rmean[i] - cmean[j] + gmean)
  je <- morie_manfd_jacobi(B, sweeps)
  dim <- as.integer(dim)
  if (dim < 1L || dim > n)
    stop("the embedding dimension must lie in 1..n")
  coords <- matrix(0, n, dim)
  for (i in seq_len(n)) for (j in seq_len(dim)) {
    lam <- je$values[j]
    coords[i, j] <- if (lam > 0) je$vectors[i, j] * sqrt(lam) else 0
  }
  list(coords = coords, values = je$values,
       n_negative = sum(je$values < -1e-9), B = B)
}

#' Embed functional data on its manifold
#'
#' @param Y A matrix with one curve per row.
#' @param k Neighbours per point in the graph.
#' @param method A member of the method list.
#' @param grid The sampling points, or NULL.
#' @param dim Embedding dimension.
#' @param sweeps Jacobi sweeps.
#' @return A list with the coordinates, the eigenvalues, the geodesic
#'   distances, the number of connected components and the residual
#'   variance of the embedding.
#' @export
morie_manfd <- function(Y, k = 4L, method = "isomap", grid = NULL,
                        dim = 2L, sweeps = 60L) {
  if (!(method %in% .MANFD_METHODS))
    stop("method must be one of ", paste(.MANFD_METHODS, collapse = ", "))
  ys <- as.matrix(Y)
  storage.mode(ys) <- "double"
  n <- nrow(ys)
  if (n < 3L) stop("need at least three curves")
  D <- morie_manfd_l2(ys, grid)
  A <- morie_manfd_knn(D, k)
  sp <- morie_manfd_paths(A)
  G <- sp$G
  disconnected <- sp$components > 1L

  coords <- NULL
  ev <- numeric(0)
  n_neg <- 0L
  if (method != "geodesic_only" && !disconnected) {
    src <- if (method == "isomap") G else D
    cs <- morie_manfd_scaling(src, dim, sweeps)
    coords <- cs$coords
    ev <- cs$values
    n_neg <- cs$n_negative
  }

  resid <- NaN
  if (!is.null(coords)) {
    src <- if (method == "isomap") G else D
    a <- numeric(0)
    b <- numeric(0)
    for (i in seq_len(n)) if (i < n) for (j in (i + 1L):n) {
      a <- c(a, src[i, j])
      b <- c(b, sqrt(.w3_csum((coords[i, ] - coords[j, ]) *
                                (coords[i, ] - coords[j, ]))))
    }
    ma <- .w3_csum(a) / length(a)
    mb <- .w3_csum(b) / length(b)
    saa <- .w3_csum((a - ma) * (a - ma))
    sbb <- .w3_csum((b - mb) * (b - mb))
    sab <- .w3_csum((a - ma) * (b - mb))
    if (saa > 0 && sbb > 0) {
      r <- sab / sqrt(saa * sbb)
      resid <- 1 - r * r
    }
  }

  finite <- G[is.finite(G)]
  list(coords = if (is.null(coords)) matrix(0, 0L, 0L) else coords,
       eigenvalues = ev, distance = D, geodesic = G,
       n_components = sp$components, disconnected = disconnected,
       n_negative_eigenvalues = n_neg, residual_variance = resid,
       estimate = resid, se = NaN,
       geodesic_max = if (length(finite)) max(finite) else NaN,
       n = n, k = as.integer(k), dim = as.integer(dim), method = method,
       name = "functional manifold representation")
}

#' One-line summary of the manfd module
#'
#' @return A character scalar.
#' @export
morie_manfd_cheatsheet <- function()
  paste0("manfd: functional manifold learning. methods ",
         paste(.MANFD_METHODS, collapse = ", "),
         "; L2 curve distances, k-NN graph geodesics, Torgerson ",
         "scaling on a written-out Jacobi eigensolver")

.vor_clip <- function(poly, a, b) {
  out <- list()
  m <- length(poly)
  for (k in seq_len(m)) {
    p <- poly[[k]]
    q <- poly[[k %% m + 1L]]
    fp <- a[1] * p[1] + a[2] * p[2] - b
    fq <- a[1] * q[1] + a[2] * q[2] - b
    if (fp <= 0) out[[length(out) + 1L]] <- p
    if ((fp < 0 && fq > 0) || (fq < 0 && fp > 0)) {
      t <- fp / (fp - fq)
      out[[length(out) + 1L]] <- p + t * (q - p)
    }
  }
  out
}

#' Voronoi cells of a point pattern inside a convex window
#'
#' The cell of \eqn{x_i} is the window intersected with the half-planes
#' \eqn{(x_j - x_i) \cdot (u - (x_i + x_j)/2) \le 0} for every other point,
#' computed by Sutherland-Hodgman clipping. Returned with the cell areas,
#' the Voronoi intensity \eqn{1/area} at each point (Barr and Schoenberg
#' 2010), perimeters, centroids, the Voronoi edges between points (the
#' Delaunay neighbours) with their lengths, the adjacency matrix, the
#' neighbour counts and the Voronoi entropy \eqn{-\sum_k P_k \log P_k} of
#' their distribution. Areas and neighbours equal \code{deldir::deldir}
#' with \code{digits = 15}.
#'
#' @param points Distinct points (n x 2) inside \code{window}.
#' @param window Rectangle \code{c(xmin, xmax, ymin, ymax)} or a convex
#'   polygon (m x 2 matrix, counter-clockwise).
#' @return List with \code{areas}, \code{cells}, \code{intensity},
#'   \code{perimeters}, \code{centroids}, \code{edges} (matrix of 1-based
#'   i, j, length), \code{neighbours}, \code{adjacency},
#'   \code{neighbour_counts}, \code{entropy}.
#' @references Okabe, A., Boots, B., Sugihara, K. and Chiu, S. N. (2000).
#'   Spatial Tessellations, 2nd ed. Wiley.
#'
#'   Barr, C. D. and Schoenberg, F. P. (2010). On the Voronoi estimator for
#'   the intensity of an inhomogeneous planar Poisson process. Biometrika
#'   97, 977-984.
#'
#'   Bormashenko, E. et al. (2018). Characterization of self-assembled 2D
#'   patterns with Voronoi entropy. Entropy 20, 956.
#' @examples
#' VoronoiCells(rbind(c(0.25, 0.5), c(0.75, 0.5)), c(0, 1, 0, 1))$areas
#' @export
VoronoiCells <- function(points, window) {
  P <- as.matrix(points)
  n <- nrow(P)
  W <- if (is.null(dim(window)) && length(window) == 4L) {
    rbind(c(window[1], window[3]), c(window[2], window[3]), c(window[2], window[4]), c(window[1], window[4]))
  } else as.matrix(window)
  cells <- vector("list", n)
  areas <- numeric(n)
  for (i in seq_len(n)) {
    poly <- lapply(seq_len(nrow(W)), function(k) as.numeric(W[k, ]))
    for (j in seq_len(n)) {
      if (j == i) next
      a <- P[j, ] - P[i, ]
      b <- a[1] * (P[i, 1] + P[j, 1]) / 2 + a[2] * (P[i, 2] + P[j, 2]) / 2
      poly <- .vor_clip(poly, a, b)
      if (!length(poly)) break
    }
    M <- if (length(poly)) do.call(rbind, poly) else matrix(0, 0, 2)
    cells[[i]] <- M
    if (nrow(M) >= 3) {
      x2 <- c(M[-1, 1], M[1, 1])
      y2 <- c(M[-1, 2], M[1, 2])
      areas[i] <- 0.5 * sum(M[, 1] * y2 - x2 * M[, 2])
    }
  }
  scale <- max(abs(W))
  if (scale == 0) scale <- 1
  nbrs <- vector("list", n)
  edges <- matrix(0, 0, 3)
  for (i in seq_len(n)) {
    near <- integer(0)
    M <- cells[[i]]
    for (j in seq_len(n)) {
      if (j == i) next
      a <- P[j, ] - P[i, ]
      b <- a[1] * (P[i, 1] + P[j, 1]) / 2 + a[2] * (P[i, 2] + P[j, 2]) / 2
      on <- M[abs(M %*% a - b) <= 1e-9 * scale * sqrt(sum(a^2)), , drop = FALSE]
      len <- if (nrow(on) >= 2) max(as.matrix(stats::dist(on))) else 0
      if (len > 1e-9 * scale) {
        near <- c(near, j)
        if (i < j) edges <- rbind(edges, c(i, j, len))
      }
    }
    nbrs[[i]] <- near
  }
  perim <- vapply(cells, function(M) {
    x <- c(M[, 1], M[1, 1])
    y <- c(M[, 2], M[1, 2])
    sum(sqrt(diff(x)^2 + diff(y)^2))
  }, 0)
  cent <- t(vapply(seq_len(n), function(i) {
    M <- cells[[i]]
    x <- M[, 1]
    y <- M[, 2]
    x2 <- c(x[-1], x[1])
    y2 <- c(y[-1], y[1])
    cr <- x * y2 - x2 * y
    c(sum((x + x2) * cr), sum((y + y2) * cr)) / (6 * areas[i])
  }, c(0, 0)))
  counts <- lengths(nbrs)
  pk <- as.numeric(table(counts)) / n
  adj <- matrix(0L, n, n)
  for (i in seq_len(n)) adj[i, nbrs[[i]]] <- 1L
  list(areas = areas, cells = cells, intensity = 1 / areas, perimeters = perim, centroids = cent,
       edges = edges, neighbours = nbrs, adjacency = adj, neighbour_counts = counts,
       entropy = 0 - sum(pk * log(pk)))
}

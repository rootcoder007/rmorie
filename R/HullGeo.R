.hg_cross <- function(o, a, b) (a[1] - o[1]) * (b[2] - o[2]) - (a[2] - o[2]) * (b[1] - o[1])

.hg_area <- function(P) {
  n <- nrow(P)
  j <- c(2:n, 1)
  0.5 * sum(P[, 1] * P[j, 2] - P[j, 1] * P[, 2])
}

.hg_perim <- function(P) {
  n <- nrow(P)
  j <- c(2:n, 1)
  sum(sqrt((P[j, 1] - P[, 1])^2 + (P[j, 2] - P[, 2])^2))
}

.hg_circum <- function(a, b, c) {
  d <- 2 * (a[1] * (b[2] - c[2]) + b[1] * (c[2] - a[2]) + c[1] * (a[2] - b[2]))
  if (d == 0) return(NULL)
  a2 <- sum(a^2)
  b2 <- sum(b^2)
  c2 <- sum(c^2)
  ux <- (a2 * (b[2] - c[2]) + b2 * (c[2] - a[2]) + c2 * (a[2] - b[2])) / d
  uy <- (a2 * (c[1] - b[1]) + b2 * (a[1] - c[1]) + c2 * (b[1] - a[1])) / d
  c(ux, uy, sqrt((ux - a[1])^2 + (uy - a[2])^2))
}

#' Convex-hull shape metrics and alpha shapes
#'
#' \code{ConvexHull}: Andrew's monotone chain, counter-clockwise from the
#' lexicographically smallest point, collinear points dropped.
#' \code{HullMetrics}: solidity (polygon area over hull area), elongation
#' \eqn{1 - w/l} of the minimum-area enclosing rectangle (rotating
#' calipers), its orientation, the FRAGSTATS fractal dimension
#' \eqn{2 \ln(P/4) / \ln A} and the isoperimetric roundness
#' \eqn{4 \pi A / P^2} of hull and polygon. \code{Delaunay}: Bowyer-Watson
#' triangulation (general position assumed). \code{AlphaShape}: Delaunay
#' triangles with circumradius at most \code{radius}, their area, boundary
#' edges and perimeter (Edelsbrunner, Kirkpatrick and Seidel 1983).
#' Identical to the Python arm \code{morie.fn.hullgeo}; indices are 1-based
#' here.
#'
#' @param points,polygon Two-column coordinates (polygon vertices in order).
#' @param radius Alpha radius.
#' @return Matrix (\code{ConvexHull}, \code{Delaunay}) or list.
#' @references Andrew, A. M. (1979). Another efficient algorithm for convex
#'   hulls in two dimensions. Information Processing Letters 9, 216-219.
#'
#'   Freeman, H. and Shapira, R. (1975). Determining the minimum-area
#'   encasing rectangle for an arbitrary closed curve. Communications of the
#'   ACM 18, 409-413.
#'
#'   Edelsbrunner, H., Kirkpatrick, D. G. and Seidel, R. (1983). On the shape
#'   of a set of points in the plane. IEEE Transactions on Information Theory
#'   29, 551-559.
#' @examples
#' HullMetrics(rbind(c(0, 0), c(4, 0), c(4, 1), c(0, 1)))$elongation
#' Delaunay(rbind(c(0, 0), c(2, 0), c(0, 2), c(2.2, 2.1)))
#' AlphaShape(rbind(c(0, 0), c(2, 0), c(0, 2), c(2.2, 2.1)), 10)$area
#' @export
ConvexHull <- function(points) {
  S <- unique(as.matrix(points) * 1)
  S <- S[order(S[, 1], S[, 2]), , drop = FALSE]
  if (nrow(S) <= 2) return(unname(S))
  half <- function(idx) {
    h <- integer(0)
    for (k in idx) {
      while (length(h) >= 2 && .hg_cross(S[h[length(h) - 1], ], S[h[length(h)], ], S[k, ]) <= 0) h <- h[-length(h)]
      h <- c(h, k)
    }
    h
  }
  lo <- half(seq_len(nrow(S)))
  up <- half(rev(seq_len(nrow(S))))
  unname(S[c(lo[-length(lo)], up[-length(up)]), , drop = FALSE])
}

#' @rdname ConvexHull
#' @export
HullMetrics <- function(polygon) {
  P <- as.matrix(polygon) * 1
  H <- ConvexHull(P)
  ah <- abs(.hg_area(H))
  ph <- .hg_perim(H)
  ap <- abs(.hg_area(P))
  pp <- .hg_perim(P)
  best <- NULL
  for (i in seq_len(nrow(H))) {
    a <- H[i, ]
    b <- H[if (i == nrow(H)) 1 else i + 1, ]
    L <- sqrt(sum((b - a)^2))
    ux <- (b[1] - a[1]) / L
    uy <- (b[2] - a[2]) / L
    s <- (H[, 1] - a[1]) * ux + (H[, 2] - a[2]) * uy
    t <- -(H[, 1] - a[1]) * uy + (H[, 2] - a[2]) * ux
    w1 <- max(s) - min(s)
    w2 <- max(t) - min(t)
    if (is.null(best) || w1 * w2 < best[1] - 1e-12 * max(1, best[1])) best <- c(w1 * w2, w1, w2, atan2(uy, ux) * 180 / pi)
  }
  len <- max(best[2], best[3])
  wid <- min(best[2], best[3])
  orient <- if (best[2] >= best[3]) best[4] else best[4] + 90
  list(hull = H, hull_area = ah, hull_perimeter = ph, area = ap, perimeter = pp,
       solidity = if (ah > 0) ap / ah else NaN, elongation = if (len > 0) 1 - wid / len else 0, width = wid,
       length = len, orientation = orient %% 180,
       fractal_dimension = if (ah > 0 && ah != 1) 2 * log(ph / 4) / log(ah) else NaN,
       fractal_dimension_polygon = if (ap > 0 && ap != 1) 2 * log(pp / 4) / log(ap) else NaN,
       roundness = if (ph > 0) 4 * pi * ah / ph^2 else NaN, compactness = if (pp > 0) 4 * pi * ap / pp^2 else NaN)
}

#' @rdname ConvexHull
#' @export
Delaunay <- function(points) {
  P <- as.matrix(points) * 1
  n <- nrow(P)
  cx <- mean(range(P[, 1]))
  cy <- mean(range(P[, 2]))
  d <- max(diff(range(P[, 1])), diff(range(P[, 2])), 1) * 20
  V <- rbind(P, c(cx - d, cy - d), c(cx + d, cy - d), c(cx, cy + d))
  tris <- matrix(c(n + 1, n + 2, n + 3), 1)
  cc <- list(.hg_circum(V[n + 1, ], V[n + 2, ], V[n + 3, ]))
  for (i in seq_len(n)) {
    p <- V[i, ]
    bad <- vapply(cc, function(c) !is.null(c) && sqrt((c[1] - p[1])^2 + (c[2] - p[2])^2) < c[3], TRUE)
    E <- do.call(rbind, lapply(which(bad), function(k) {
      t <- tris[k, ]
      rbind(sort(t[1:2]), sort(t[2:3]), sort(t[c(1, 3)]))
    }))
    key <- paste(E[, 1], E[, 2])
    once <- E[!(key %in% key[duplicated(key)]), , drop = FALSE]
    tris <- tris[!bad, , drop = FALSE]
    cc <- cc[!bad]
    for (r in seq_len(nrow(once))) {
      t <- sort(c(once[r, ], i))
      tris <- rbind(tris, t)
      cc[[length(cc) + 1]] <- .hg_circum(V[t[1], ], V[t[2], ], V[t[3], ])
    }
  }
  tris <- tris[apply(tris, 1, max) <= n, , drop = FALSE]
  unname(tris[order(tris[, 1], tris[, 2], tris[, 3]), , drop = FALSE])
}

#' @rdname ConvexHull
#' @export
AlphaShape <- function(points, radius) {
  P <- as.matrix(points) * 1
  Tr <- Delaunay(P)
  keep <- vapply(seq_len(nrow(Tr)), function(k) {
    c <- .hg_circum(P[Tr[k, 1], ], P[Tr[k, 2], ], P[Tr[k, 3], ])
    !is.null(c) && c[3] <= radius
  }, TRUE)
  K <- Tr[keep, , drop = FALSE]
  E <- do.call(rbind, lapply(seq_len(nrow(K)), function(k) rbind(K[k, 1:2], K[k, 2:3], K[k, c(1, 3)])))
  if (is.null(E)) E <- matrix(integer(0), 0, 2)
  key <- paste(E[, 1], E[, 2])
  B <- E[!(key %in% key[duplicated(key)]), , drop = FALSE]
  B <- B[order(B[, 1], B[, 2]), , drop = FALSE]
  area <- sum(vapply(seq_len(nrow(K)), function(k) abs(.hg_area(P[K[k, ], , drop = FALSE])), 0))
  list(triangles = K, area = area, edges = B,
       perimeter = sum(sqrt(rowSums((P[B[, 1], , drop = FALSE] - P[B[, 2], , drop = FALSE])^2))))
}

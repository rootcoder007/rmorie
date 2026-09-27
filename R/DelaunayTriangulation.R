.dt_circum <- function(a, b, c) {
  bx <- b[1] - a[1]
  by <- b[2] - a[2]
  cx <- c[1] - a[1]
  cy <- c[2] - a[2]
  d <- 2 * (bx * cy - by * cx)
  if (d == 0) return(list(c = NULL, r2 = Inf))
  b2 <- bx * bx + by * by
  c2 <- cx * cx + cy * cy
  ux <- (cy * b2 - by * c2) / d
  uy <- (bx * c2 - cx * b2) / d
  list(c = c(a[1] + ux, a[2] + uy), r2 = ux * ux + uy * uy)
}

.dt_hull_area <- function(P) {
  h <- grDevices::chull(P)
  H <- P[h, , drop = FALSE]
  x2 <- c(H[-1, 1], H[1, 1])
  y2 <- c(H[-1, 2], H[1, 2])
  abs(0.5 * sum(H[, 1] * y2 - x2 * H[, 2]))
}

#' Delaunay triangulation by Bowyer-Watson insertion
#'
#' Each point in turn removes the triangles whose circumcircle contains it
#' and re-links the cavity boundary to it (Bowyer 1981; Watson 1981).
#' Coordinates are centred and scaled before insertion and the enclosing
#' super-triangle is 1000 times the extent of the points. Per triangle:
#' area, circumcentre, circumradius R, angles, the radius-edge ratio
#' \eqn{R / l_{min}} (Shewchuk 2002) and the shape quality
#' \eqn{4\sqrt{3} A / \sum l^2}. The empty-circle property is checked
#' directly and the triangles' total area is compared with the convex
#' hull's.
#'
#' @param points Distinct points (n x 2), n >= 3, not all collinear.
#' @return List with \code{triangles} (1-based, counter-clockwise),
#'   \code{edges}, \code{neighbours}, \code{adjacency}, \code{areas},
#'   \code{circumcentres}, \code{circumradii}, \code{angles},
#'   \code{min_angle}, \code{radius_edge}, \code{quality},
#'   \code{empty_circle_violations}, \code{hull_area_gap}.
#' @references Bowyer, A. (1981). Computing Dirichlet tessellations.
#'   Computer Journal 24, 162-166.
#'
#'   Watson, D. F. (1981). Computing the n-dimensional Delaunay tessellation
#'   with application to Voronoi polytopes. Computer Journal 24, 167-172.
#'
#'   Shewchuk, J. R. (2002). Delaunay refinement algorithms for triangular
#'   mesh generation. Computational Geometry 22, 21-74.
#' @examples
#' DelaunayTriangulation(rbind(c(0, 0), c(1, 0), c(0, 1), c(1, 1), c(0.5, 0.4)))$triangles
#' @export
DelaunayTriangulation <- function(points) {
  P <- as.matrix(points)
  storage.mode(P) <- "double"
  n <- nrow(P)
  if (n < 3) stop("need at least three points", call. = FALSE)
  m <- colMeans(P)
  sc <- max(abs(sweep(P, 2, m)))
  if (sc == 0) sc <- 1
  allp <- rbind(sweep(P, 2, m) / sc, c(-3000, -3000), c(3000, -3000), c(0, 3000))
  tris <- list(c(n + 1L, n + 2L, n + 3L))
  circ <- list(.dt_circum(allp[n + 1, ], allp[n + 2, ], allp[n + 3, ]))
  for (pi in seq_len(n)) {
    p <- allp[pi, ]
    isbad <- vapply(circ, function(cc) !is.null(cc$c) && sum((p - cc$c)^2) < cc$r2 * (1 + 1e-12), logical(1))
    bad <- tris[isbad]
    keys <- unlist(lapply(bad, function(t) {
      e <- rbind(t[1:2], t[2:3], t[c(3, 1)])
      paste(pmin(e[, 1], e[, 2]), pmax(e[, 1], e[, 2]))
    }))
    cnt <- table(keys)
    tris <- tris[!isbad]
    circ <- circ[!isbad]
    for (t in bad) {
      e <- rbind(t[1:2], t[2:3], t[c(3, 1)])
      for (k in 1:3) {
        if (cnt[[paste(min(e[k, ]), max(e[k, ]))]] == 1) {
          nt <- c(e[k, ], pi)
          tris[[length(tris) + 1L]] <- nt
          circ[[length(circ) + 1L]] <- .dt_circum(allp[nt[1], ], allp[nt[2], ], allp[nt[3], ])
        }
      }
    }
  }
  keep <- vapply(tris, function(t) max(t) <= n, logical(1))
  Tm <- do.call(rbind, tris[keep])
  for (k in seq_len(nrow(Tm))) {
    a <- P[Tm[k, 1], ]
    b <- P[Tm[k, 2], ]
    c <- P[Tm[k, 3], ]
    if ((b[1] - a[1]) * (c[2] - a[2]) - (b[2] - a[2]) * (c[1] - a[1]) < 0) Tm[k, ] <- Tm[k, c(1, 3, 2)]
  }
  Tm <- Tm[order(Tm[, 1], Tm[, 2], Tm[, 3]), , drop = FALSE]
  nt <- nrow(Tm)
  areas <- rad <- mins <- redge <- qual <- numeric(nt)
  cent <- matrix(0, nt, 2)
  angles <- matrix(0, nt, 3)
  for (k in seq_len(nt)) {
    a <- P[Tm[k, 1], ]
    b <- P[Tm[k, 2], ]
    c <- P[Tm[k, 3], ]
    A <- 0.5 * ((b[1] - a[1]) * (c[2] - a[2]) - (b[2] - a[2]) * (c[1] - a[1]))
    cc <- .dt_circum(a, b, c)
    la <- sqrt(sum((b - c)^2))
    lb <- sqrt(sum((a - c)^2))
    lc <- sqrt(sum((a - b)^2))
    g1 <- acos(max(-1, min(1, (lb^2 + lc^2 - la^2) / (2 * lb * lc)))) * 180 / pi
    g2 <- acos(max(-1, min(1, (la^2 + lc^2 - lb^2) / (2 * la * lc)))) * 180 / pi
    angles[k, ] <- c(g1, g2, 180 - g1 - g2)
    areas[k] <- A
    cent[k, ] <- cc$c
    rad[k] <- sqrt(cc$r2)
    mins[k] <- min(angles[k, ])
    redge[k] <- rad[k] / min(la, lb, lc)
    qual[k] <- 4 * sqrt(3) * A / (la^2 + lb^2 + lc^2)
  }
  E <- rbind(Tm[, 1:2], Tm[, 2:3], Tm[, c(3, 1)])
  E <- unique(cbind(pmin(E[, 1], E[, 2]), pmax(E[, 1], E[, 2])))
  E <- E[order(E[, 1], E[, 2]), , drop = FALSE]
  adj <- matrix(0L, n, n)
  adj[E] <- 1L
  adj[E[, 2:1]] <- 1L
  viol <- 0L
  for (k in seq_len(nt)) {
    d <- sqrt(colSums((t(P) - cent[k, ])^2))
    d[Tm[k, ]] <- Inf
    viol <- viol + sum(d < rad[k] * (1 - 1e-10))
  }
  list(triangles = Tm, edges = E, neighbours = lapply(seq_len(n), function(i) which(adj[i, ] == 1L)),
       adjacency = adj, areas = areas, circumcentres = cent, circumradii = rad, angles = angles,
       min_angle = mins, radius_edge = redge, quality = qual, empty_circle_violations = viol,
       hull_area_gap = .dt_hull_area(P) - sum(areas))
}

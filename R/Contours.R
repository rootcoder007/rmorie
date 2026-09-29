# SPDX-License-Identifier: AGPL-3.0-or-later
# Contour operations on a gridded field. Identical to the Python arm morie.fn.contours.

.ct_clip_band <- function(pts, lo, hi) {
  # pts: matrix with columns x, y, value; Sutherland-Hodgman to lo <= value < hi
  clip <- function(points, keep, edge) {
    out <- NULL
    n <- nrow(points)
    for (k in seq_len(n)) {
      a <- points[k, ]
      b <- points[if (k == n) 1 else k + 1, ]
      ina <- keep(a[3])
      inb <- keep(b[3])
      if (ina) out <- rbind(out, a)
      if (ina != inb) {
        t <- (edge - a[3]) / (b[3] - a[3])
        out <- rbind(out, c(a[1:2] + t * (b[1:2] - a[1:2]), edge))
      }
    }
    out
  }
  out <- clip(pts, function(v) v >= lo, lo)
  if (is.null(out)) return(NULL)
  clip(out, function(v) v < hi, hi)
}

.ct_area <- function(poly) {
  n <- nrow(poly)
  nx <- c(2:n, 1)
  abs(sum(poly[, 1] * poly[nx, 2] - poly[nx, 1] * poly[, 2])) / 2
}

.ct_cell_polys <- function(z, xs, ys, lo, hi) {
  polys <- list()
  for (i in seq_len(length(ys) - 1)) for (j in seq_len(length(xs) - 1)) {
    p <- rbind(c(xs[j], ys[i]), c(xs[j + 1], ys[i]), c(xs[j + 1], ys[i + 1]), c(xs[j], ys[i + 1]))
    v <- c(z[i, j], z[i, j + 1], z[i + 1, j + 1], z[i + 1, j])
    for (tri in list(c(1, 2, 3), c(1, 3, 4))) {
      poly <- .ct_clip_band(cbind(p[tri, , drop = FALSE], v[tri]), lo, hi)
      if (!is.null(poly) && nrow(poly) >= 3) polys[[length(polys) + 1]] <- unname(poly)
    }
  }
  polys
}

#' Contours: isobands, clipping, band quantities, labels, smoothing
#'
#' \code{z[i, j]} is the value at \code{(xs[j], ys[i])}; the contour lines
#' themselves come from \code{IsoLines}. \code{ContourFill}: isobands between consecutive levels; each cell
#' is split into two triangles (the field is linear on a triangle) and each
#' triangle is clipped to \code{levels[k] <= z < levels[k + 1]}; areas are exact
#' for the piecewise-linear interpolant. \code{ContourBands}: the isobands with
#' a grey shade per band. \code{ContourClip}: polyline pieces inside a convex
#' counter-clockwise polygon (Sutherland and Hodgman 1974). \code{ContourQuantity}:
#' area and integral of the field where \code{lower <= z < upper} (each clipped
#' polygon is fanned into triangles; the integral of a linear field over a
#' triangle is its area times the mean of the three vertex values).
#' \code{ContourLabels}: label at the midpoint of the straightest run of each
#' polyline (smallest neighbouring turning angles, ties to the longest segment)
#' with the text angle along it. \code{ContourSmooth}: uniform B-spline through
#' the vertices as control points (de Boor recurrence; clamped knots for open
#' curves, wrapped control points for closed ones).
#'
#' @param z Numeric matrix of field values.
#' @param xs,ys Grid coordinates of the columns and rows of \code{z}.
#' @param levels Contour levels (band edges for the fill functions).
#' @param shades Optional shade per band (default grey levels 0.9 to 0.2).
#' @param lines List of polylines, each a two-column matrix of vertices.
#' @param polygon Convex clip polygon, two-column matrix, counter-clockwise.
#' @param lower,upper Band limits for \code{ContourQuantity}.
#' @param min_length Polylines shorter than this get no label.
#' @param line One polyline (two-column matrix) to smooth.
#' @param degree B-spline degree.
#' @param samples Number of evaluation points.
#' @param closed Treat the polyline as closed.
#' @return A list; see each function.
#' @references Sutherland, I. E. and Hodgman, G. W. (1974). Reentrant polygon clipping.
#'   Communications of the ACM 17, 32-42.
#'
#'   de Boor, C. (1978). A Practical Guide to Splines. Springer.
#' @examples
#' z <- rbind(c(0, 1), c(1, 2))
#' ContourFill(z, c(0, 1), c(0, 1), c(0, 1, 2.5))$areas
#' ContourQuantity(z, c(0, 1), c(0, 1), 0, 3)$integral
#' @export
ContourFill <- function(z, xs, ys, levels) {
  z <- unname(as.matrix(z)) * 1
  bands <- list()
  areas <- numeric(0)
  for (k in seq_len(length(levels) - 1)) {
    polys <- .ct_cell_polys(z, xs, ys, levels[k], levels[k + 1])
    bands[[k]] <- lapply(polys, function(p) p[, 1:2, drop = FALSE])
    areas <- c(areas, sum(vapply(polys, function(p) .ct_area(p[, 1:2, drop = FALSE]), 0)))
  }
  list(bands = bands, areas = areas, levels = as.numeric(levels))
}

#' @rdname ContourFill
#' @export
ContourBands <- function(z, xs, ys, levels, shades = NULL) {
  f <- ContourFill(z, xs, ys, levels)
  nb <- length(levels) - 1
  if (is.null(shades)) shades <- 0.9 - 0.7 * (seq_len(nb) - 1) / max(nb - 1, 1)
  list(bands = f$bands, areas = f$areas, shades = as.numeric(shades), levels = f$levels)
}

.ct_clip_line <- function(line, clip) {
  n <- nrow(clip)
  nx <- c(2:n, 1)
  inside <- function(p) all((clip[nx, 1] - clip[, 1]) * (p[2] - clip[, 2]) - (clip[nx, 2] - clip[, 2]) * (p[1] - clip[, 1]) >= -1e-12)
  pieces <- list()
  cur <- NULL
  for (k in seq_len(nrow(line) - 1)) {
    a <- line[k, ]
    b <- line[k + 1, ]
    ts <- c(0, 1)
    for (m in seq_len(n)) {
      cc <- clip[m, ]
      d <- clip[nx[m], ]
      den <- (b[1] - a[1]) * (d[2] - cc[2]) - (b[2] - a[2]) * (d[1] - cc[1])
      if (abs(den) > 1e-15) {
        t <- ((cc[1] - a[1]) * (d[2] - cc[2]) - (cc[2] - a[2]) * (d[1] - cc[1])) / den
        if (t > 0 && t < 1) ts <- c(ts, t)
      }
    }
    ts <- sort(ts)
    for (q in seq_len(length(ts) - 1)) {
      pa <- a + ts[q] * (b - a)
      pb <- a + ts[q + 1] * (b - a)
      if (inside((pa + pb) / 2)) {
        if (is.null(cur)) cur <- rbind(pa)
        cur <- rbind(cur, pb)
      } else if (!is.null(cur)) {
        pieces[[length(pieces) + 1]] <- unname(cur)
        cur <- NULL
      }
    }
  }
  if (!is.null(cur)) pieces[[length(pieces) + 1]] <- unname(cur)
  pieces
}

#' @rdname ContourFill
#' @export
ContourClip <- function(lines, polygon) {
  poly <- unname(as.matrix(polygon)) * 1
  out <- list()
  for (line in lines) out <- c(out, .ct_clip_line(unname(as.matrix(line)) * 1, poly))
  list(lines = out, n_pieces = length(out))
}

#' @rdname ContourFill
#' @export
ContourQuantity <- function(z, xs, ys, lower, upper) {
  z <- unname(as.matrix(z)) * 1
  polys <- .ct_cell_polys(z, xs, ys, lower, upper)
  area <- 0
  integral <- 0
  for (p in polys) {
    for (k in 2:(nrow(p) - 1)) {
      fan <- p[c(1, k, k + 1), , drop = FALSE]
      a <- .ct_area(fan[, 1:2, drop = FALSE])
      area <- area + a
      integral <- integral + a * sum(fan[, 3]) / 3
    }
  }
  list(area = area, integral = integral, mean = if (area > 0) integral / area else NA_real_)
}

#' @rdname ContourFill
#' @export
ContourLabels <- function(lines, min_length = 0) {
  pos <- NULL
  ang <- numeric(0)
  wrap <- function(d) atan2(sin(d), cos(d))
  for (line in lines) {
    pts <- unname(as.matrix(line)) * 1
    if (nrow(pts) < 2) next
    len <- sum(sqrt(rowSums((pts[-1, , drop = FALSE] - pts[-nrow(pts), , drop = FALSE])^2)))
    if (len < min_length) next
    best <- NULL
    key <- NULL
    for (k in seq_len(nrow(pts) - 1)) {
      a <- pts[k, ]
      b <- pts[k + 1, ]
      seg <- atan2(b[2] - a[2], b[1] - a[1])
      turn <- 0
      if (k > 1) turn <- turn + abs(wrap(seg - atan2(a[2] - pts[k - 1, 2], a[1] - pts[k - 1, 1])))
      if (k + 2 <= nrow(pts)) turn <- turn + abs(wrap(atan2(pts[k + 2, 2] - b[2], pts[k + 2, 1] - b[1]) - seg))
      cand <- c(turn, -sqrt(sum((b - a)^2)))
      if (is.null(key) || cand[1] < key[1] || (cand[1] == key[1] && cand[2] < key[2])) {
        key <- cand
        best <- c(k, seg)
      }
    }
    k <- best[1]
    pos <- rbind(pos, (pts[k, ] + pts[k + 1, ]) / 2)
    ang <- c(ang, best[2])
  }
  list(positions = unname(pos), angles = ang)
}

#' @rdname ContourFill
#' @export
ContourSmooth <- function(line, degree = 3, samples = 50, closed = FALSE) {
  P <- unname(as.matrix(line)) * 1
  if (closed) P <- rbind(P, P[seq_len(degree), , drop = FALSE])
  n <- nrow(P)
  if (closed) {
    knots <- 0:(n + degree)
    lo <- knots[degree + 1]
    hi <- knots[n + 1]
  } else {
    knots <- c(rep(0, degree + 1), seq_len(n - degree - 1), rep(n - degree, degree + 1))
    lo <- 0
    hi <- n - degree
  }
  deboor <- function(u) {
    k <- degree
    while (k < n - 1 && !(knots[k + 1] <= u && u < knots[k + 2])) k <- k + 1
    if (u >= hi) k <- n - 1
    d <- P[(0:degree) + k - degree + 1, , drop = FALSE]
    for (r in seq_len(degree)) {
      for (j in degree:r) {
        i <- j + k - degree
        den <- knots[i + degree - r + 2] - knots[i + 1]
        a <- if (den == 0) 0 else (u - knots[i + 1]) / den
        d[j + 1, ] <- (1 - a) * d[j, ] + a * d[j + 1, ]
      }
    }
    d[degree + 1, ]
  }
  us <- if (samples > 1) lo + (hi - lo) * (seq_len(samples) - 1) / (samples - 1) else lo
  pts <- t(vapply(us, function(u) deboor(min(u, hi)), numeric(2)))
  list(points = unname(pts), parameters = us)
}

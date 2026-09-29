#' Vector and raster geoprocessing
#'
#' \code{PolygonBoolean}: intersection (overlay, clip), union and difference
#' (erase) of two simple polygons by the Greiner-Hormann algorithm.
#' \code{PolygonArea}: signed shoelace area. \code{FilledContourBands}:
#' exact isoband polygons and areas of the piecewise-linear surface.
#' \code{ElevationProfile}: bilinear DEM profile along a polyline.
#' \code{ProximityBands}: multiple-ring buffer bands on a grid. Identical to
#' the Python arm \code{morie.fn.geomops}.
#'
#' @param subject,clip Two-column matrices of polygon vertices.
#' @param op One of "intersection", "union", "difference".
#' @param ring Two-column matrix of a ring.
#' @param x,y Grid coordinates (ascending).
#' @param z Matrix of values, rows following y.
#' @param path Two-column matrix of polyline vertices.
#' @param step Sampling step.
#' @param breaks Band distance breaks.
#' @param points Two-column matrix of feature points (or NULL).
#' @param lines List of two-column polyline matrices (or NULL).
#' @return List (number for \code{PolygonArea}).
#' @references Greiner, G. and Hormann, K. (1998). Efficient clipping of
#'   arbitrary polygons. ACM Transactions on Graphics 17, 71-83.
#'
#'   Watson, D. F. (1992). Contouring: A Guide to the Analysis and Display of
#'   Spatial Data. Pergamon.
#' @examples
#' A <- rbind(c(0, 0), c(4, 0), c(4, 3), c(0, 3))
#' B <- rbind(c(2, 1), c(6, 2), c(5, 5))
#' PolygonBoolean(A, B, "union")$area
#' FilledContourBands(c(0, 1), c(0, 1), rbind(c(0, 1), c(1, 2)), c(0, 1, 2))$areas
#' @export
PolygonBoolean <- function(subject, clip, op = "intersection") {
  S <- as.matrix(subject) + 0
  C <- as.matrix(clip) + 0
  if (PolygonArea(S) < 0) S <- S[rev(seq_len(nrow(S))), , drop = FALSE]
  if (PolygonArea(C) < 0) C <- C[rev(seq_len(nrow(C))), , drop = FALSE]
  if (!op %in% c("intersection", "union", "difference")) stop("op must be intersection, union or difference")
  ns <- nrow(S)
  nc <- nrow(C)
  I <- matrix(0, 0, 6)
  for (i in seq_len(ns)) {
    for (j in seq_len(nc)) {
      r <- .go_segint(S[i, ], S[if (i == ns) 1 else i + 1, ], C[j, ], C[if (j == nc) 1 else j + 1, ])
      if (!is.null(r)) I <- rbind(I, c(i, j, r))
    }
  }
  if (nrow(I) == 0) {
    s_in_c <- .go_inside(S[1, ], C)
    c_in_s <- .go_inside(C[1, ], S)
    rings <- if (op == "intersection") {
      if (s_in_c) list(S) else if (c_in_s) list(C) else list()
    } else if (op == "union") {
      if (s_in_c) list(C) else if (c_in_s) list(S) else list(S, C)
    } else {
      if (s_in_c) list() else if (c_in_s) list(S, C[rev(seq_len(nrow(C))), , drop = FALSE]) else list(S)
    }
    return(list(rings = rings, area = .go_ss(vapply(rings, PolygonArea, 0))))
  }
  build <- function(P, col, acol) {
    L <- matrix(0, 0, 4)
    for (k in seq_len(nrow(P))) {
      L <- rbind(L, c(P[k, ], 0, 0))
      hit <- which(I[, col] == k)
      hit <- hit[order(I[hit, acol])]
      for (h in hit) L <- rbind(L, c(I[h, 5], I[h, 6], 1, h))
    }
    L
  }
  SL <- build(S, 1, 3)
  CL <- build(C, 2, 4)
  ent <- list(numeric(nrow(SL)), numeric(nrow(CL)))
  lists <- list(SL, CL)
  others <- list(C, S)
  flips <- c(op %in% c("union", "difference"), op == "union")
  for (w in 1:2) {
    L <- lists[[w]]
    e <- !.go_inside(L[1, 1:2], others[[w]])
    if (flips[w]) e <- !e
    for (k in seq_len(nrow(L))) {
      if (L[k, 3] == 1) {
        ent[[w]][k] <- e
        e <- !e
      }
    }
  }
  posS <- integer(nrow(I))
  posC <- integer(nrow(I))
  for (k in seq_len(nrow(SL))) if (SL[k, 3] == 1) posS[SL[k, 4]] <- k
  for (k in seq_len(nrow(CL))) if (CL[k, 3] == 1) posC[CL[k, 4]] <- k
  visited <- rep(FALSE, nrow(I))
  rings <- list()
  for (st in seq_len(nrow(SL))) {
    if (SL[st, 3] != 1 || visited[SL[st, 4]]) next
    ring <- matrix(0, 0, 2)
    w <- 1
    k <- st
    repeat {
      L <- lists[[w]]
      m <- nrow(L)
      visited[L[k, 4]] <- TRUE
      ring <- rbind(ring, L[k, 1:2])
      if (ent[[w]][k]) {
        k <- if (k == m) 1 else k + 1
        while (L[k, 3] != 1) {
          ring <- rbind(ring, L[k, 1:2])
          k <- if (k == m) 1 else k + 1
        }
      } else {
        k <- if (k == 1) m else k - 1
        while (L[k, 3] != 1) {
          ring <- rbind(ring, L[k, 1:2])
          k <- if (k == 1) m else k - 1
        }
      }
      id <- L[k, 4]
      w <- 3 - w
      k <- if (w == 1) posS[id] else posC[id]
      if (visited[id]) break
    }
    rings[[length(rings) + 1]] <- unname(ring)
  }
  rings <- lapply(rings, function(r) if (PolygonArea(r) > 0) r else r[rev(seq_len(nrow(r))), , drop = FALSE])
  out <- rings
  for (i in seq_along(rings)) {
    r <- rings[[i]]
    t <- NULL
    for (e in seq_len(nrow(r))) {
      q <- r[if (e == nrow(r)) 1 else e + 1, ]
      t <- (r[e, ] + q) / 2
      clear <- TRUE
      for (j in seq_along(rings)) {
        if (j == i) next
        o <- rings[[j]]
        dmin <- min(vapply(seq_len(nrow(o)), function(k) .go_segdist(t, o[k, ], o[if (k == nrow(o)) 1 else k + 1, ]), 0))
        if (dmin <= 1e-9) clear <- FALSE
      }
      if (clear) break
    }
    depth <- sum(vapply(seq_along(rings), function(j) j != i && .go_inside(t, rings[[j]]), TRUE))
    if (depth %% 2 == 1) out[[i]] <- r[rev(seq_len(nrow(r))), , drop = FALSE]
  }
  list(rings = out, area = .go_ss(vapply(out, PolygonArea, 0)))
}

.go_ss <- function(v) {
  s <- 0
  for (a in v) s <- s + a
  s
}

.go_inside <- function(p, poly) {
  n <- nrow(poly)
  cc <- FALSE
  for (i in seq_len(n)) {
    j <- if (i == n) 1 else i + 1
    if ((poly[i, 2] > p[2]) != (poly[j, 2] > p[2])) {
      if (p[1] < poly[i, 1] + (p[2] - poly[i, 2]) * (poly[j, 1] - poly[i, 1]) / (poly[j, 2] - poly[i, 2])) cc <- !cc
    }
  }
  cc
}

.go_segint <- function(p1, p2, q1, q2) {
  d <- (p2[1] - p1[1]) * (q2[2] - q1[2]) - (p2[2] - p1[2]) * (q2[1] - q1[1])
  if (d == 0) return(NULL)
  a <- ((q1[1] - p1[1]) * (q2[2] - q1[2]) - (q1[2] - p1[2]) * (q2[1] - q1[1])) / d
  b <- ((q1[1] - p1[1]) * (p2[2] - p1[2]) - (q1[2] - p1[2]) * (p2[1] - p1[1])) / d
  if (a > 0 && a < 1 && b > 0 && b < 1) c(a, b, p1[1] + a * (p2[1] - p1[1]), p1[2] + a * (p2[2] - p1[2])) else NULL
}

#' @rdname PolygonBoolean
#' @export
PolygonArea <- function(ring) {
  R <- as.matrix(ring)
  n <- nrow(R)
  if (n == 0) return(0)
  s <- 0
  for (i in seq_len(n)) {
    j <- if (i == n) 1 else i + 1
    s <- s + R[i, 1] * R[j, 2] - R[j, 1] * R[i, 2]
  }
  0.5 * s
}

.go_cliplin <- function(P, v, level, above) {
  out <- matrix(0, 0, 2)
  ov <- numeric(0)
  n <- nrow(P)
  for (i in seq_len(n)) {
    j <- if (i == n) 1 else i + 1
    ina <- if (above) v[i] >= level else v[i] <= level
    inb <- if (above) v[j] >= level else v[j] <= level
    if (ina) {
      out <- rbind(out, P[i, ])
      ov <- c(ov, v[i])
    }
    if (ina != inb) {
      t <- (level - v[i]) / (v[j] - v[i])
      out <- rbind(out, P[i, ] + t * (P[j, ] - P[i, ]))
      ov <- c(ov, level)
    }
  }
  list(P = unname(out), v = ov)
}

.go_bilin <- function(x, y, z, px, py) {
  if (px < x[1] || px > x[length(x)] || py < y[1] || py > y[length(y)]) stop("the path leaves the grid")
  i <- if (px < x[length(x)]) max(which(x[-length(x)] <= px)) else length(x) - 1
  j <- if (py < y[length(y)]) max(which(y[-length(y)] <= py)) else length(y) - 1
  tx <- (px - x[i]) / (x[i + 1] - x[i])
  ty <- (py - y[j]) / (y[j + 1] - y[j])
  z[j, i] * (1 - tx) * (1 - ty) + z[j, i + 1] * tx * (1 - ty) + z[j + 1, i] * (1 - tx) * ty + z[j + 1, i + 1] * tx * ty
}

#' @rdname PolygonBoolean
#' @export
ElevationProfile <- function(x, y, z, path, step) {
  z <- as.matrix(z)
  P <- as.matrix(path)
  pts <- matrix(P[1, ], 1)
  dists <- 0
  total <- 0
  for (k in seq_len(nrow(P) - 1)) {
    a <- P[k, ]
    b <- P[k + 1, ]
    seg <- sqrt(sum((b - a)^2))
    t <- step
    while (t < seg - 1e-12) {
      pts <- rbind(pts, a + t / seg * (b - a))
      dists <- c(dists, total + t)
      t <- t + step
    }
    total <- total + seg
    pts <- rbind(pts, b)
    dists <- c(dists, total)
  }
  el <- vapply(seq_len(nrow(pts)), function(k) .go_bilin(x, y, z, pts[k, 1], pts[k, 2]), 0)
  d <- diff(el)
  list(distance = dists, points = unname(pts), elevation = el, ascent = .go_ss(pmax(d, 0)), descent = .go_ss(pmax(-d, 0)))
}

.go_segdist <- function(p, a, b) {
  d <- b - a
  L2 <- sum(d^2)
  t <- if (L2 == 0) 0 else max(0, min(1, sum((p - a) * d) / L2))
  sqrt(sum((p - a - t * d)^2))
}

#' @rdname PolygonBoolean
#' @export
ProximityBands <- function(x, y, breaks, points = NULL, lines = NULL) {
  dx <- if (length(x) > 1) x[2] - x[1] else 1
  dy <- if (length(y) > 1) y[2] - y[1] else 1
  band <- matrix(0L, length(y), length(x))
  dist <- matrix(0, length(y), length(x))
  cnt <- integer(length(breaks) + 1)
  for (j in seq_along(y)) {
    for (i in seq_along(x)) {
      p <- c(x[i], y[j])
      ds <- numeric(0)
      if (!is.null(points)) {
        Pm <- matrix(points, ncol = 2)
        ds <- c(ds, sqrt((Pm[, 1] - p[1])^2 + (Pm[, 2] - p[2])^2))
      }
      for (ln in lines) {
        ln <- as.matrix(ln)
        for (k in seq_len(nrow(ln) - 1)) ds <- c(ds, .go_segdist(p, ln[k, ], ln[k + 1, ]))
      }
      d <- min(ds)
      k <- sum(d >= breaks)
      band[j, i] <- k
      dist[j, i] <- d
      cnt[k + 1] <- cnt[k + 1] + 1L
    }
  }
  list(band = band, distance = dist, cells_per_band = cnt, area_per_band = cnt * dx * dy)
}

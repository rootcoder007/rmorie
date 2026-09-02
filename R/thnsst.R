# SPDX-License-Identifier: AGPL-3.0-or-later
#' Thiessen (Voronoi) polygons
#'
#' Thiessen (1911), Precipitation averages for large areas, Monthly
#' Weather Review 39(7), 1082-1089: each station is assigned the region of
#' all points closer to it than to any other, and the areal average is the
#' area-weighted mean of the station values.  The 1911 volume is in the
#' public domain but was not retrievable here; the construction is quoted
#' in its standard published form.  It is the Voronoi diagram of Dirichlet
#' (1850) and Voronoi (1908) under another name.  Cells are computed
#' exactly as intersections of half-planes with a bounding box, and areas
#' by the shoelace formula, so the weights are exact rather than estimated
#' on a grid.
#'
#' @param coords station locations, one row per station.
#' @param bbox clipping box c(x0, y0, x1, y1).
#' @param values station values, for the areal mean.
#' @return list: estimate, areas, weights, cells, areal_mean, bbox, method.
#' @keywords internal
#' @examples
#' Voronoi(matrix(c(0, 0, 1, 0, 0, 1), 3, 2, byrow = TRUE))$areas
#' @export
Voronoi <- function(coords, bbox = NULL, values = NULL) {
  clip <- function(poly, a, b, cc) {
    out <- list()
    n <- length(poly)
    if (n == 0L) return(out)
    for (i in seq_len(n)) {
      p <- poly[[i]]
      q <- poly[[if (i == n) 1L else i + 1L]]
      dp <- a * p[1] + b * p[2] - cc
      dq <- a * q[1] + b * q[2] - cc
      if (dp <= 0) out[[length(out) + 1L]] <- p
      if ((dp < 0 && dq > 0) || (dq < 0 && dp > 0)) {
        t <- dp / (dp - dq)
        out[[length(out) + 1L]] <- c(p[1] + t * (q[1] - p[1]), p[2] + t * (q[2] - p[2]))
      }
    }
    out
  }
  area <- function(poly) {
    s <- 0
    n <- length(poly)
    if (n < 3L) return(0)
    for (i in seq_len(n)) {
      j <- if (i == n) 1L else i + 1L
      s <- s + poly[[i]][1] * poly[[j]][2] - poly[[j]][1] * poly[[i]][2]
    }
    abs(s) / 2
  }
  P <- .s03mat(coords)
  n <- nrow(P)
  if (is.null(bbox)) {
    xs <- P[, 1]
    ys <- P[, 2]
    mx <- max(xs) - min(xs)
    if (mx == 0) mx <- 1
    my <- max(ys) - min(ys)
    if (my == 0) my <- 1
    bbox <- c(min(xs) - 0.5 * mx, min(ys) - 0.5 * my,
              max(xs) + 0.5 * mx, max(ys) + 0.5 * my)
  }
  bb <- as.numeric(bbox)
  x0 <- bb[1]
  y0 <- bb[2]
  x1 <- bb[3]
  y1 <- bb[4]
  areas <- numeric(n)
  cells <- vector("list", n)
  for (i in seq_len(n)) {
    poly <- list(c(x0, y0), c(x1, y0), c(x1, y1), c(x0, y1))
    for (j in seq_len(n)) {
      if (i == j) next
      a <- 2 * (P[j, 1] - P[i, 1])
      b <- 2 * (P[j, 2] - P[i, 2])
      cc <- (P[j, 1]^2 + P[j, 2]^2) - (P[i, 1]^2 + P[i, 2]^2)
      poly <- clip(poly, a, b, cc)
      if (length(poly) == 0L) break
    }
    cells[[i]] <- poly
    areas[i] <- if (length(poly) >= 3L) area(poly) else 0
  }
  tot <- 0
  for (v in areas) tot <- tot + v
  w <- if (tot > 0) areas / tot else rep(0, n)
  am <- NaN
  if (!is.null(values)) {
    z <- .s03vec(values)
    s <- 0
    for (i in seq_len(n)) s <- s + w[i] * z[i]
    am <- s
  }
  list(estimate = tot, areas = areas, weights = w, cells = cells,
       areal_mean = am, bbox = c(x0, y0, x1, y1),
       method = "Thiessen (1911) polygons by half-plane clipping; areas by the shoelace formula")
}

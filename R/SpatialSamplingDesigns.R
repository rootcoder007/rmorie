#' Spatial sampling designs and weights
#'
#' \code{RandomSpatialSample}: uniform random points in a polygon.
#' \code{HexagonalGridSample}: hexagonal grid in a polygon as
#' \code{sp::spsample(type = "hexagonal")}. \code{AdaptiveClusterSample}:
#' Thompson's adaptive cluster sampling with modified Hansen-Hurwitz and
#' Horvitz-Thompson estimators. \code{QuadtreeGrid}: adaptive-resolution
#' quadtree cells. \code{VoronoiDeclusteringWeights} and
#' \code{CellDeclusteringWeights}: declustering weights.
#' \code{SpatialThinning}: randomised thinning to a minimum distance.
#' \code{MapQualityIndices}: ME, MAE, MSE, RMSE, r2 and MEC. Identical to
#' the Python arm \code{morie.fn.spsampling}.
#'
#' @param n Sample size.
#' @param polygon Two-column matrix of polygon vertices.
#' @param seed Philox seed.
#' @param cellsize Hexagon spacing.
#' @param offset Lattice offset (fractions of a cell).
#' @param y Population grid (matrix).
#' @param initial Two-column matrix of initial cells (0-based row, col), or NULL.
#' @param threshold Condition value.
#' @param points Two-column matrix of points.
#' @param bbox Box (xmin, ymin, xmax, ymax).
#' @param capacity,max_depth Quadtree leaf capacity and depth limit.
#' @param cell_size,origin Declustering cell size and grid origin.
#' @param min_dist,reps Thinning distance and passes.
#' @param z,zhat Observations and predictions.
#' @param pi,N Inclusion probabilities and population size.
#' @return Matrix or list.
#' @references Thompson, S. K. (1990). Adaptive cluster sampling. JASA 85,
#'   1050-1059.
#'
#'   Aiello-Lammens, M. E. et al. (2015). spThin: an R package for spatial
#'   thinning of species occurrence records. Ecography 38, 541-545.
#'
#'   Deutsch, C. V. (1989). DECLUS: a Fortran 77 program for determining
#'   optimum spatial declustering weights. Computers and Geosciences 15,
#'   325-332.
#'
#'   Brus, D. J. (2022). Spatial Sampling with R. CRC Press.
#' @examples
#' sq <- rbind(c(0, 0), c(10, 0), c(10, 10), c(0, 10))
#' nrow(HexagonalGridSample(sq, 2))
#' CellDeclusteringWeights(rbind(c(0.1, 0.1), c(0.2, 0.2), c(1.5, 0.5)), 1)$weights
#' MapQualityIndices(c(1, 2, 3, 4), c(1.5, 1.5, 3.5, 3.5))$MEC
#' @export
RandomSpatialSample <- function(n, polygon, seed = 1) {
  poly <- as.matrix(polygon)
  x0 <- min(poly[, 1])
  x1 <- max(poly[, 1])
  y0 <- min(poly[, 2])
  y1 <- max(poly[, 2])
  out <- matrix(0, 0, 2)
  block <- 0
  while (nrow(out) < n) {
    u <- .morie_random_uniform(2 * max(64, 4 * n), seed = seed, stream = block)
    block <- block + 1
    for (k in seq(1, length(u), by = 2)) {
      x <- x0 + (x1 - x0) * u[k]
      yy <- y0 + (y1 - y0) * u[k + 1]
      if (.ss_inside(x, yy, poly)) {
        out <- rbind(out, c(x, yy))
        if (nrow(out) == n) break
      }
    }
  }
  out
}

.ss_inside <- function(x, y, poly) {
  n <- nrow(poly)
  cc <- FALSE
  for (i in seq_len(n)) {
    j <- if (i == n) 1 else i + 1
    x1 <- poly[i, 1]
    y1 <- poly[i, 2]
    x2 <- poly[j, 1]
    y2 <- poly[j, 2]
    if ((y1 > y) != (y2 > y)) {
      xc <- x1 + (y - y1) * (x2 - x1) / (y2 - y1)
      if (x < xc) cc <- !cc else if (x == xc) return(TRUE)
    }
  }
  cc
}

.ss_ss <- function(v) {
  s <- 0
  for (a in v) s <- s + a
  s
}

.ss_rseq <- function(a, b, by) a + (0:floor((b - a) / by + 1e-10)) * by

#' @rdname RandomSpatialSample
#' @export
HexagonalGridSample <- function(polygon, cellsize, offset = c(0.5, 0.5)) {
  .morie_arg(polygon, "m")
  poly <- as.matrix(polygon)
  ll <- c(min(poly[, 1]), min(poly[, 2]))
  ur <- c(max(poly[, 1]), max(poly[, 2]))
  dx <- cellsize
  dy <- sqrt(3) * dx / 2
  x <- .ss_rseq(ll[1], ur[1] - dx / 2, dx)
  y <- .ss_rseq(ll[2], ur[2], dy)
  Y <- rep(y, each = length(x))
  X <- rep(c(x, x + dx / 2), length.out = length(Y))
  X <- X + (ur[1] - max(X)) / 2 + offset[1] * dx
  Y <- Y + (ur[2] - max(Y)) / 2 + offset[2] * dx * sqrt(3) / 2
  keep <- vapply(seq_along(X), function(i) .ss_inside(X[i], Y[i], poly), TRUE)
  cbind(X[keep], Y[keep])
}

#' @rdname RandomSpatialSample
#' @export
AdaptiveClusterSample <- function(y, initial = NULL, threshold, n = NULL, seed = 1) {
  Y <- as.matrix(y) + 0
  R <- nrow(Y)
  C <- ncol(Y)
  N <- R * C
  val <- function(k) Y[k %/% C + 1, k %% C + 1]
  if (is.null(initial)) {
    if (is.null(n)) stop("give initial cells or n")
    u <- .morie_random_uniform(n, seed = seed)
    pool <- 0:(N - 1)
    s0 <- integer(0)
    for (t in seq_len(n)) {
      j <- min(floor(u[t] * length(pool)), length(pool) - 1) + 1
      s0 <- c(s0, pool[j])
      pool <- pool[-j]
    }
  } else {
    initial <- matrix(initial, ncol = 2)
    s0 <- initial[, 1] * C + initial[, 2]
  }
  n0 <- length(s0)
  if (length(unique(s0)) != n0 || n0 < 2) stop("the initial sample must hold at least two distinct cells")
  meets <- vapply(0:(N - 1), function(k) val(k) >= threshold, TRUE)
  nbrs <- function(k) {
    r <- k %/% C
    c <- k %% C
    cand <- rbind(c(r - 1, c), c(r + 1, c), c(r, c - 1), c(r, c + 1))
    ok <- cand[, 1] >= 0 & cand[, 1] < R & cand[, 2] >= 0 & cand[, 2] < C
    cand[ok, 1] * C + cand[ok, 2]
  }
  net <- rep(-1L, N)
  nid <- 0L
  for (k in 0:(N - 1)) {
    if (net[k + 1] >= 0) next
    net[k + 1] <- nid
    if (meets[k + 1]) {
      stack <- k
      while (length(stack)) {
        q <- stack[length(stack)]
        stack <- stack[-length(stack)]
        for (v in nbrs(q)) {
          if (meets[v + 1] && net[v + 1] < 0) {
            net[v + 1] <- nid
            stack <- c(stack, v)
          }
        }
      }
    }
    nid <- nid + 1L
  }
  members <- split(0:(N - 1), net)
  mem <- function(k) members[[as.character(net[k + 1])]]
  sampled <- integer(0)
  for (k in s0) {
    if (meets[k + 1]) for (q in mem(k)) sampled <- c(sampled, q, nbrs(q))
    sampled <- c(sampled, k)
  }
  sampled <- sort(unique(sampled))
  w <- vapply(s0, function(k) .ss_ss(vapply(mem(k), val, 0)) / length(mem(k)), 0)
  mhh <- .ss_ss(w) / n0
  vhh <- (N - n0) / (N * n0 * (n0 - 1)) * .ss_ss((w - mhh)^2)
  tot <- 0
  for (g in sort(unique(net[s0 + 1]))) {
    mg <- members[[as.character(g)]]
    x <- length(mg)
    alpha <- 1 - exp(lchoose(N - x, n0) - lchoose(N, n0))
    tot <- tot + .ss_ss(vapply(mg, val, 0)) / alpha
  }
  list(initial = cbind(s0 %/% C, s0 %% C), final = cbind(sampled %/% C, sampled %% C), final_size = length(sampled),
       mean_hh = mhh, var_hh = vhh, mean_ht = tot / N)
}

#' @rdname RandomSpatialSample
#' @export
QuadtreeGrid <- function(points, bbox, capacity = 4L, max_depth = 8L) {
  P <- as.matrix(points)
  out <- list()
  rec <- function(x0, y0, x1, y1, idx, depth) {
    if (length(idx) <= capacity || depth >= max_depth) {
      out[[length(out) + 1]] <<- c(x0, y0, x1, y1, depth, length(idx))
      return(invisible(NULL))
    }
    xm <- 0.5 * (x0 + x1)
    ym <- 0.5 * (y0 + y1)
    kids <- rbind(c(x0, y0, xm, ym), c(xm, y0, x1, ym), c(x0, ym, xm, y1), c(xm, ym, x1, y1))
    for (r in 1:4) {
      a0 <- kids[r, 1]
      b0 <- kids[r, 2]
      a1 <- kids[r, 3]
      b1 <- kids[r, 4]
      sub <- idx[vapply(idx, function(i) {
        px <- P[i, 1]
        py <- P[i, 2]
        (if (a0 > x0) px >= a0 else px >= x0) && (if (a1 < x1) px < a1 else px <= x1) &&
          (if (b0 > y0) py >= b0 else py >= y0) && (if (b1 < y1) py < b1 else py <= y1)
      }, TRUE)]
      rec(a0, b0, a1, b1, sub, depth + 1)
    }
  }
  rec(bbox[1], bbox[2], bbox[3], bbox[4], seq_len(nrow(P)), 0)
  list(cells = do.call(rbind, out))
}

.ss_clip <- function(poly, a, b, c) {
  out <- matrix(0, 0, 2)
  n <- nrow(poly)
  if (n == 0) return(out)
  for (i in seq_len(n)) {
    p <- poly[i, ]
    q <- poly[if (i == n) 1 else i + 1, ]
    fp <- a * p[1] + b * p[2] - c
    fq <- a * q[1] + b * q[2] - c
    if (fp <= 0) out <- rbind(out, p)
    if ((fp < 0 && fq > 0) || (fq < 0 && fp > 0)) {
      t <- fp / (fp - fq)
      out <- rbind(out, p + t * (q - p))
    }
  }
  unname(out)
}

.ss_area <- function(poly) {
  s <- 0
  n <- nrow(poly)
  for (i in seq_len(n)) {
    j <- if (i == n) 1 else i + 1
    s <- s + poly[i, 1] * poly[j, 2] - poly[j, 1] * poly[i, 2]
  }
  abs(s) / 2
}

#' @rdname RandomSpatialSample
#' @export
VoronoiDeclusteringWeights <- function(points, bbox) {
  P <- as.matrix(points)
  n <- nrow(P)
  areas <- numeric(n)
  for (i in seq_len(n)) {
    cell <- rbind(c(bbox[1], bbox[2]), c(bbox[3], bbox[2]), c(bbox[3], bbox[4]), c(bbox[1], bbox[4]))
    for (j in seq_len(n)) {
      if (j == i || all(P[j, ] == P[i, ])) next
      a <- P[j, 1] - P[i, 1]
      b <- P[j, 2] - P[i, 2]
      cc <- 0.5 * (P[j, 1]^2 + P[j, 2]^2 - P[i, 1]^2 - P[i, 2]^2)
      cell <- .ss_clip(cell, a, b, cc)
      if (nrow(cell) == 0) break
    }
    areas[i] <- if (nrow(cell) >= 3) .ss_area(cell) else 0
  }
  list(weights = areas / .ss_ss(areas), areas = areas)
}

#' @rdname RandomSpatialSample
#' @export
CellDeclusteringWeights <- function(points, cell_size, origin = c(0, 0)) {
  P <- as.matrix(points)
  key <- paste(floor((P[, 1] - origin[1]) / cell_size), floor((P[, 2] - origin[2]) / cell_size))
  cnt <- table(key)
  L <- length(cnt)
  list(weights = as.numeric(1 / (L * cnt[key])), occupied_cells = L)
}

#' @rdname RandomSpatialSample
#' @export
SpatialThinning <- function(points, min_dist, reps = 10L, seed = 1) {
  P <- as.matrix(points)
  n <- nrow(P)
  best <- NULL
  for (rep in seq_len(reps) - 1) {
    u <- .morie_random_uniform(n, seed = seed, stream = rep)
    kept <- integer(0)
    for (i in order(u, seq_len(n))) {
      ok <- TRUE
      for (j in kept) if (sqrt(sum((P[i, ] - P[j, ])^2)) < min_dist) ok <- FALSE
      if (ok) kept <- c(kept, i)
    }
    if (is.null(best) || length(kept) > length(best)) best <- kept
  }
  list(kept = sort(best))
}

#' @rdname RandomSpatialSample
#' @export
MapQualityIndices <- function(z, zhat, pi = NULL, N = NULL) {
  z <- as.numeric(z)
  h <- as.numeric(zhat)
  n <- length(z)
  e <- h - z
  if (is.null(pi)) {
    me <- .ss_ss(e) / n
    mae <- .ss_ss(abs(e)) / n
    mse <- .ss_ss(e * e) / n
  } else {
    if (is.null(N)) stop("N is required with pi")
    me <- .ss_ss(e / pi) / N
    mae <- .ss_ss(abs(e) / pi) / N
    mse <- .ss_ss(e * e / pi) / N
  }
  zb <- .ss_ss(z) / n
  hb <- .ss_ss(h) / n
  sxy <- .ss_ss((z - zb) * (h - hb))
  sxx <- .ss_ss((z - zb)^2)
  syy <- .ss_ss((h - hb)^2)
  list(ME = me, MAE = mae, MSE = mse, RMSE = sqrt(mse), r2 = sxy * sxy / (sxx * syy), MEC = 1 - .ss_ss(e * e) / sxx)
}

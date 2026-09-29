# SPDX-License-Identifier: AGPL-3.0-or-later
# 3-D Delaunay tetrahedralisation and Voronoi cells, and Ruppert's Delaunay refinement.
# Identical to the Python arm morie.fn.mesh3d.

.m3_circumsphere <- function(P) {
  d <- ncol(P)
  A <- 2 * sweep(P[-1, , drop = FALSE], 2, P[1, ])
  b <- rowSums(P[-1, , drop = FALSE]^2) - sum(P[1, ]^2)
  cc <- tryCatch(solve(A, b), error = function(e) NULL)
  if (is.null(cc)) {
    return(list(c = NULL, r2 = Inf))
  }
  list(c = as.numeric(cc), r2 = sum((cc - P[1, ])^2))
}

.m3_bowyer_watson <- function(pts, d) {
  n <- nrow(pts)
  lo <- apply(pts, 2, min)
  hi <- apply(pts, 2, max)
  span <- max(hi - lo)
  if (span == 0) span <- 1
  mid <- (lo + hi) / 2
  big <- 50 * span
  sup <- if (d == 2) {
    rbind(c(mid[1] - 2 * big, mid[2] - big), c(mid[1] + 2 * big, mid[2] - big), c(mid[1], mid[2] + 2 * big))
  } else {
    rbind(mid - big, mid + c(3, -1, -1) * big, mid + c(-1, 3, -1) * big, mid + c(-1, -1, 3) * big)
  }
  allp <- rbind(pts, sup)
  s0 <- n + seq_len(d + 1)
  simp <- list(s0)
  cache <- list(.m3_circumsphere(allp[s0, , drop = FALSE]))
  for (i in seq_len(n)) {
    p <- allp[i, ]
    bad <- vapply(cache, function(cs) !is.null(cs$c) && sum((p - cs$c)^2) < cs$r2 * (1 - 1e-12), logical(1))
    fkeys <- character(0)
    flist <- list()
    for (s in simp[bad]) {
      for (k in seq_len(d + 1)) {
        f <- sort(s[-k])
        key <- paste(f, collapse = ",")
        fkeys <- c(fkeys, key)
        flist[[key]] <- f
      }
    }
    cnt <- table(fkeys)
    simp <- simp[!bad]
    cache <- cache[!bad]
    for (key in sort(names(cnt)[cnt == 1])) {
      s <- sort(c(flist[[key]], i))
      simp[[length(simp) + 1]] <- s
      cache[[length(cache) + 1]] <- .m3_circumsphere(allp[s, , drop = FALSE])
    }
  }
  keep <- vapply(simp, function(s) all(s <= n), logical(1))
  M <- do.call(rbind, simp[keep])
  M[do.call(order, as.data.frame(M)), , drop = FALSE]
}

.m3_polygon_area <- function(P, ax) {
  cen <- colMeans(P)
  tmp <- if (abs(ax[1]) < 0.9) c(1, 0, 0) else c(0, 1, 0)
  cr <- function(a, b) c(a[2] * b[3] - a[3] * b[2], a[3] * b[1] - a[1] * b[3], a[1] * b[2] - a[2] * b[1])
  u <- cr(ax, tmp)
  u <- u / sqrt(sum(u^2))
  w <- cr(ax, u)
  D <- sweep(P, 2, cen)
  Q <- D[order(atan2(D %*% w, D %*% u)), , drop = FALSE]
  m <- nrow(Q)
  sum(vapply(seq_len(m), function(k) 0.5 * sqrt(sum(cr(Q[k, ], Q[k %% m + 1, ])^2)), numeric(1)))
}

#' Meshes: 3-D Delaunay tetrahedralisation, 3-D Voronoi cells, Ruppert refinement
#'
#' \code{Delaunay3D}: Bowyer-Watson insertion into a super-tetrahedron;
#' tetrahedra whose circumspheres contain the new point are removed and the
#' cavity is re-triangulated from its boundary faces. Returns sorted 1-based
#' index rows and the circumcentres.
#' \code{Voronoi3D}: the dual; cell vertices are circumcentres of the
#' incident tetrahedra, hull points have unbounded cells (volume Inf), and a
#' bounded cell has volume \code{sum_j A_ij |x_i - x_j| / 6} over its faces.
#' \code{RuppertRefine}: splits encroached convex hull segments at midpoints
#' and inserts circumcentres of skinny triangles (circumradius to shortest
#' edge above \code{1 / (2 sin(min_angle))}) until every angle is at least
#' \code{min_angle} degrees or \code{max_points} is reached.
#'
#' @param points Matrix of points (n x 3, or n x 2 for RuppertRefine).
#' @param min_angle Target minimum angle in degrees (at most about 20.7).
#' @param max_points Maximum number of vertices.
#' @return A list.
#' @references Bowyer, A. (1981). Computing Dirichlet tessellations. Computer
#'   J. 24, 162-166. Watson, D. F. (1981). Computer J. 24, 167-172. Okabe, A.,
#'   Boots, B., Sugihara, K. and Chiu, S. N. (2000). Spatial Tessellations,
#'   2nd ed. Ruppert, J. (1995). A Delaunay refinement algorithm for quality
#'   2-dimensional mesh generation. J. Algorithms 18, 548-585. Shewchuk, J. R.
#'   (2002). Computational Geometry 22, 21-74.
#' @examples
#' Delaunay3D(rbind(c(0, 0, 0), c(1, 0, 0), c(0, 1, 0), c(0, 0, 1), c(1, 1, 1.2)))$tetrahedra
#' RuppertRefine(rbind(c(0, 0), c(4, 0), c(4, 1), c(0, 1)))$min_angle
#' @export
Delaunay3D <- function(points) {
  pts <- unname(as.matrix(points)) * 1
  tets <- .m3_bowyer_watson(pts, 3)
  cc <- t(apply(tets, 1, function(s) .m3_circumsphere(pts[s, , drop = FALSE])$c))
  list(tetrahedra = tets, circumcenters = cc)
}

#' @rdname Delaunay3D
#' @export
Voronoi3D <- function(points) {
  pts <- unname(as.matrix(points)) * 1
  n <- nrow(pts)
  d <- Delaunay3D(pts)
  tets <- d$tetrahedra
  cc <- d$circumcenters
  faces <- do.call(rbind, lapply(1:4, function(k) tets[, -k, drop = FALSE]))
  fk <- apply(faces, 1, paste, collapse = ",")
  cnt <- table(fk)
  hull <- unique(as.integer(unlist(strsplit(names(cnt)[cnt == 1], ","))))
  bounded <- !(seq_len(n) %in% hull)
  verts <- lapply(seq_len(n), function(i) cc[rowSums(tets == i) > 0, , drop = FALSE])
  vol <- rep(Inf, n)
  for (i in which(bounded)) {
    inc <- rowSums(tets == i) > 0
    nb <- sort(setdiff(unique(as.integer(tets[inc, ])), i))
    total <- 0
    for (j in nb) {
      P <- cc[inc & rowSums(tets == j) > 0, , drop = FALSE]
      if (nrow(P) < 3) next
      dij <- sqrt(sum((pts[i, ] - pts[j, ])^2))
      total <- total + .m3_polygon_area(P, (pts[j, ] - pts[i, ]) / dij) * dij / 6
    }
    vol[i] <- total
  }
  list(vertices = verts, bounded = bounded, volumes = vol)
}

#' @rdname Delaunay3D
#' @export
RuppertRefine <- function(points, min_angle = 20, max_points = 500) {
  P <- unname(as.matrix(points)) * 1
  idx <- order(P[, 1], P[, 2])
  crs <- function(o, a, b) (P[a, 1] - P[o, 1]) * (P[b, 2] - P[o, 2]) - (P[a, 2] - P[o, 2]) * (P[b, 1] - P[o, 1])
  chain <- function(ix) {
    h <- integer(0)
    for (i in ix) {
      while (length(h) >= 2 && crs(h[length(h) - 1], h[length(h)], i) <= 0) h <- h[-length(h)]
      h <- c(h, i)
    }
    h[-length(h)]
  }
  hull <- c(chain(idx), chain(rev(idx)))
  m <- length(hull)
  segs <- cbind(hull, hull[seq_len(m) %% m + 1])
  B <- 1 / (2 * sin(min_angle * pi / 180))
  encroached <- function(s, q) {
    a <- P[s[1], ]
    b <- P[s[2], ]
    sum((q - (a + b) / 2)^2) < sum((a - b)^2) / 4 * (1 - 1e-12)
  }
  split <- function(k) {
    a <- segs[k, 1]
    b <- segs[k, 2]
    P <<- rbind(P, (P[a, ] + P[b, ]) / 2)
    mm <- nrow(P)
    segs <<- rbind(segs[seq_len(k - 1), , drop = FALSE], c(a, mm), c(mm, b), segs[-seq_len(k), , drop = FALSE])
  }
  first_enc <- function(test) {
    for (k in seq_len(nrow(segs))) if (test(segs[k, ])) return(k)
    NULL
  }
  e2 <- function(t, a, b) sum((P[t[a], ] - P[t[b], ])^2)
  while (nrow(P) < max_points) {
    tris <- .m3_bowyer_watson(P, 2)
    enc <- first_enc(function(s) any(vapply(setdiff(seq_len(nrow(P)), s), function(q) encroached(s, P[q, ]), logical(1))))
    if (!is.null(enc)) {
      split(enc)
      next
    }
    worst <- FALSE
    wr <- B
    wc <- NULL
    for (r in seq_len(nrow(tris))) {
      t <- tris[r, ]
      cs <- .m3_circumsphere(P[t, , drop = FALSE])
      ratio <- sqrt(cs$r2 / min(e2(t, 1, 2), e2(t, 2, 3), e2(t, 1, 3)))
      if (ratio > wr + 1e-12) {
        worst <- TRUE
        wr <- ratio
        wc <- cs$c
      }
    }
    if (!worst) break
    enc <- first_enc(function(s) encroached(s, wc))
    if (!is.null(enc)) split(enc) else P <- rbind(P, wc)
  }
  tris <- .m3_bowyer_watson(P, 2)
  ang <- function(t) {
    vapply(0:2, function(k) {
      a <- P[t[k + 1], ]
      v1 <- P[t[(k + 1) %% 3 + 1], ] - a
      v2 <- P[t[(k + 2) %% 3 + 1], ] - a
      cs <- sum(v1 * v2) / sqrt(sum(v1^2) * sum(v2^2))
      acos(max(-1, min(1, cs))) * 180 / pi
    }, numeric(1))
  }
  mn <- min(apply(tris, 1, function(t) min(ang(t))))
  list(points = unname(P), triangles = tris, segments = unname(segs), min_angle = mn)
}

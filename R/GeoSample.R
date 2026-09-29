# SPDX-License-Identifier: AGPL-3.0-or-later
# Geodesy and spatial sampling.
# Identical to the Python arm morie.fn.geosample.

#' Geodesy and sampling: rhumb lines, ECEF and Helmert datum transformation, spatial CV folds, stratified samples, nested grids
#'
#' \code{RhumbLine}: loxodrome distance \code{R sqrt(dphi^2 + q^2 dlambda^2)}
#' and bearing \code{atan2(dlambda, dpsi)} the short way round (as
#' geosphere::distRhumb; \code{method = "geosphere"} copies bearingRhumb's
#' double wrap across the antimeridian).
#' \code{GeodeticToEcef} and \code{EcefToGeodetic}: ellipsoidal conversions
#' (IOGP guidance note 7-2). \code{HelmertTransform}: seven-parameter
#' similarity transformation (position-vector or coordinate-frame).
#' \code{BlockCvFolds}: blocks of an \code{nx x ny} grid shuffled by a Philox
#' permutation and dealt to \code{k} folds. \code{KmeansCvFolds}: folds from
#' Lloyd's k-means with k-means++ seeding. \code{TemporalStratifiedSample}:
#' proportional allocation with largest remainders and Philox simple random
#' samples per period. \code{NestedGrid}: quadtree cell indices per level.
#' Indices and folds are 0-based, as the Python arm.
#'
#' @param lat1,lon1,lat2,lon2 End points (degrees).
#' @param radius Sphere radius (m).
#' @param method "shortest" or "geosphere".
#' @param lat,lon,h Geodetic latitude, longitude (degrees) and height.
#' @param ellipsoid Name ("WGS84", "GRS80", "WGS72", "Clarke1866", "Airy1830", "Intl1924") or c(a, f).
#' @param X,Y,Z ECEF coordinates.
#' @param tol Iteration tolerance.
#' @param xyz ECEF coordinate vector.
#' @param tx,ty,tz Translations (m).
#' @param rx,ry,rz Rotations (arc-seconds).
#' @param ds_ppm Scale change (ppm).
#' @param convention "position_vector" or "coordinate_frame".
#' @param coords Coordinate matrix.
#' @param nx,ny Blocks along x and y.
#' @param k Number of folds or clusters.
#' @param seed Philox seed.
#' @param max_iter Lloyd iterations.
#' @param times Observation times.
#' @param n_sample Sample size.
#' @param n_strata Number of time strata.
#' @param levels Number of quadtree levels.
#' @param bbox Bounding box c(x0, x1, y0, y1), or NULL.
#' @return A list or numeric vector.
#' @references IOGP (2019). Geomatics Guidance Note 7-2. Roberts, D. R. et al.
#'   (2017). Ecography 40, 913-929. Brenning, A. (2012). IGARSS 2012,
#'   5372-5375. Cochran, W. G. (1977). Sampling Techniques. Samet, H. (1984).
#'   ACM Computing Surveys 16, 187-260.
#' @examples
#' RhumbLine(0, 0, 0, 1)
#' HelmertTransform(c(3657660.66, 255768.55, 5201382.11), 0, 0, 4.5, 0, 0, 0.554, 0.219)
#' @export
RhumbLine <- function(lat1, lon1, lat2, lon2, radius = 6378137, method = "shortest") {
  rad <- pi / 180
  p1 <- lat1 * rad
  p2 <- lat2 * rad
  dphi <- p2 - p1
  dl0 <- (lon2 - lon1) * rad
  dl <- dl0
  if (abs(dl) > pi) dl <- if (dl > 0) -(2 * pi - dl) else 2 * pi + dl
  db <- dl
  if (method == "geosphere" && abs(dl0) > pi && dl0 > 0) db <- 2 * pi + dl
  dpsi <- log(tan(pi / 4 + p2 / 2) / tan(pi / 4 + p1 / 2))
  q <- if (abs(dpsi) > 1e-12) dphi / dpsi else cos(p1)
  list(distance = sqrt(dphi^2 + q^2 * dl^2) * radius, bearing = (atan2(db, dpsi) / rad) %% 360)
}

.gs_ell <- function(e) {
  tab <- list(WGS84 = c(6378137, 1 / 298.257223563), GRS80 = c(6378137, 1 / 298.257222101), WGS72 = c(6378135, 1 / 298.26),
              Clarke1866 = c(6378206.4, 1 / 294.9786982), Airy1830 = c(6377563.396, 1 / 299.3249646),
              Intl1924 = c(6378388, 1 / 297))
  if (is.character(e)) tab[[e]] else e
}

#' @rdname RhumbLine
#' @export
GeodeticToEcef <- function(lat, lon, h, ellipsoid = "WGS84") {
  af <- .gs_ell(ellipsoid)
  e2 <- af[2] * (2 - af[2])
  p <- lat * pi / 180
  lm <- lon * pi / 180
  N <- af[1] / sqrt(1 - e2 * sin(p)^2)
  c((N + h) * cos(p) * cos(lm), (N + h) * cos(p) * sin(lm), (N * (1 - e2) + h) * sin(p))
}

#' @rdname RhumbLine
#' @export
EcefToGeodetic <- function(X, Y, Z, ellipsoid = "WGS84", tol = 1e-14) {
  af <- .gs_ell(ellipsoid)
  a <- af[1]
  e2 <- af[2] * (2 - af[2])
  p <- sqrt(X^2 + Y^2)
  lon <- atan2(Y, X)
  phi <- atan2(Z, p * (1 - e2))
  for (it in seq_len(50)) {
    N <- a / sqrt(1 - e2 * sin(phi)^2)
    nw <- atan2(Z + e2 * N * sin(phi), p)
    if (abs(nw - phi) <= tol) {
      phi <- nw
      break
    }
    phi <- nw
  }
  N <- a / sqrt(1 - e2 * sin(phi)^2)
  h <- if (abs(cos(phi)) > 1e-12) p / cos(phi) - N else abs(Z) - a * sqrt(1 - e2)
  c(phi * 180 / pi, lon * 180 / pi, h)
}

#' @rdname RhumbLine
#' @export
HelmertTransform <- function(xyz, tx, ty, tz, rx, ry, rz, ds_ppm, convention = "position_vector") {
  s <- 1 + ds_ppm * 1e-6
  k <- pi / (180 * 3600)
  a <- rx * k
  b <- ry * k
  cc <- rz * k
  if (convention == "coordinate_frame") {
    a <- -a
    b <- -b
    cc <- -cc
  }
  R <- rbind(c(1, -cc, b), c(cc, 1, -a), c(-b, a, 1))
  c(tx, ty, tz) + s * as.numeric(R %*% as.numeric(xyz))
}

.gs_shuffle <- function(v, u) {
  if (length(v) > 1) for (i in (length(v) - 1):1) {
    j <- floor(u[i + 1] * (i + 1))
    tmp <- v[i + 1]
    v[i + 1] <- v[j + 1]
    v[j + 1] <- tmp
  }
  v
}

#' @rdname RhumbLine
#' @export
BlockCvFolds <- function(coords, nx, ny, k, seed = 0) {
  P <- matrix(as.numeric(unlist(coords)), ncol = 2, byrow = is.list(coords))
  cell <- function(v, lo, hi, m) if (hi <= lo) rep(0, length(v)) else pmin(floor((v - lo) / (hi - lo) * m), m - 1)
  block <- cell(P[, 1], min(P[, 1]), max(P[, 1]), nx) + nx * cell(P[, 2], min(P[, 2]), max(P[, 2]), ny)
  used <- sort(unique(block))
  perm <- .gs_shuffle(used, .morie_random_uniform(length(used), seed = seed, stream = 0))
  fold <- (seq_along(perm) - 1) %% k
  list(fold = fold[match(block, perm)], block = block)
}

#' @rdname RhumbLine
#' @export
KmeansCvFolds <- function(coords, k, max_iter = 100, seed = 0) {
  P <- matrix(as.numeric(unlist(coords)), ncol = 2, byrow = is.list(coords))
  n <- nrow(P)
  u <- .morie_random_uniform(k, seed = seed, stream = 0)
  cent <- P[min(floor(u[1] * n), n - 1) + 1, , drop = FALSE]
  if (k > 1) for (c in 2:k) {
    d2 <- vapply(seq_len(n), function(i) min((P[i, 1] - cent[, 1])^2 + (P[i, 2] - cent[, 2])^2), 0)
    acc <- cumsum(d2)
    pick <- which(acc >= u[c] * sum(d2))[1]
    if (is.na(pick)) pick <- n
    cent <- rbind(cent, P[pick, ])
  }
  lab <- rep(0L, n)
  for (it in seq_len(max_iter)) {
    nw <- vapply(seq_len(n), function(i) which.min((P[i, 1] - cent[, 1])^2 + (P[i, 2] - cent[, 2])^2) - 1L, 0L)
    for (j in seq_len(k)) if (any(nw == j - 1)) cent[j, ] <- c(sum(P[nw == j - 1, 1]), sum(P[nw == j - 1, 2])) / sum(nw == j - 1)
    if (identical(nw, lab)) break
    lab <- nw
  }
  list(fold = match(lab, unique(lab)) - 1L, centers = unname(cent))
}

#' @rdname RhumbLine
#' @export
TemporalStratifiedSample <- function(times, n_sample, n_strata = 4, seed = 0) {
  ts <- as.numeric(times)
  N <- length(ts)
  lo <- min(ts)
  hi <- max(ts)
  st <- if (hi > lo) pmin(floor((ts - lo) / (hi - lo) * n_strata), n_strata - 1) else rep(0, N)
  sizes <- vapply(0:(n_strata - 1), function(h) sum(st == h), 0)
  raw <- n_sample * sizes / N
  alloc <- floor(raw)
  rem <- order(-(raw - alloc), 0:(n_strata - 1))
  extra <- n_sample - sum(alloc)
  if (extra > 0) alloc[rem[seq_len(extra)]] <- alloc[rem[seq_len(extra)]] + 1
  chosen <- weights <- numeric(0)
  for (h in 0:(n_strata - 1)) {
    idx <- which(st == h) - 1
    if (!length(idx)) next
    pool <- .gs_shuffle(idx, .morie_random_uniform(length(idx), seed = seed, stream = h))
    pick <- sort(pool[seq_len(alloc[h + 1])])
    chosen <- c(chosen, pick)
    weights <- c(weights, rep(sizes[h + 1] / alloc[h + 1], length(pick)))
  }
  list(index = chosen, weight = weights, allocation = alloc, stratum_sizes = sizes)
}

#' @rdname RhumbLine
#' @export
NestedGrid <- function(coords, levels, bbox = NULL) {
  P <- matrix(as.numeric(unlist(coords)), ncol = 2, byrow = is.list(coords))
  if (is.null(bbox)) bbox <- c(range(P[, 1]), range(P[, 2]))
  cells <- counts <- vector("list", levels + 1)
  for (lev in 0:levels) {
    m <- 2^lev
    ix <- if (bbox[2] > bbox[1]) pmin(floor((P[, 1] - bbox[1]) / (bbox[2] - bbox[1]) * m), m - 1) else 0 * P[, 1]
    iy <- if (bbox[4] > bbox[3]) pmin(floor((P[, 2] - bbox[3]) / (bbox[4] - bbox[3]) * m), m - 1) else 0 * P[, 2]
    ids <- ix + m * iy
    cells[[lev + 1]] <- ids
    counts[[lev + 1]] <- vapply(0:(m * m - 1), function(cc) sum(ids == cc), 0)
  }
  list(cells = cells, counts = counts)
}

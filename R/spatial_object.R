# SPDX-License-Identifier: AGPL-3.0-or-later
#
# spatial_object.R -- a self-contained spatial data object + operations
# (coordinate reference systems, Web-Mercator reprojection with exact
# inverse, neighbour construction, spatial weights, Moran's I, density-
# based sampling, and mapping) implemented in base R, completing rmorie's
# coverage of the srr "SP" standards without an sf/GDAL dependency.

#' srr spatial (SP) object standards
#'
#' These SP standards are completed by the morie_spatial object and its
#' operations (this file), tested in test-srr-standards-SP-full.R.
#'
#' @srrstats {SP2.0} morie_spatial() is the dedicated class for spatial
#'   input, carrying coordinates + a coordinate reference system.
#' @srrstats {SP2.0a} morie_spatial_coords() documents + performs
#'   conversion of the object to a plain coordinate matrix.
#' @srrstats {SP2.2b} A test demonstrates the coordinate matrix can be
#'   handed to base spatial routines (dist / prcomp).
#' @srrstats {SP2.3} morie_spatial() accepts data loaded from a standard
#'   tabular format (a data.frame of coordinates), tested from a CSV
#'   round-trip.
#' @srrstats {SP2.4} Coordinate reference systems are represented by EPSG
#'   codes + a WKT-style description, never by bare PROJ4 strings.
#' @srrstats {SP2.4a} morie_spatial_crs() returns the CRS as an EPSG code
#'   and a WKT2-style label, not a PROJ4 string.
#' @srrstats {SP2.5} The morie_spatial class stores CRS metadata.
#' @srrstats {SP2.5a} morie_spatial_transform() converts between CRS
#'   (4326 <-> 3857), the analogue of converting to alternative classes.
#' @srrstats {SP2.8} morie_spatial() is the single pre-processing routine
#'   validating + normalising all spatial input.
#' @srrstats {SP2.9} morie_spatial_transform() preserves the attribute
#'   data and CRS metadata through the transformation.
#' @srrstats {SP3.2} morie_spatial_sample(by_density=TRUE) samples based on
#'   local spatial density of the input.
#' @srrstats {SP3.5} Spatial weighting never broadcasts incommensurate
#'   dimensions; morie_spatial_weights() errors on a size mismatch.
#' @srrstats {SP3.6} morie_spatial_sample() documents + tests the effect
#'   of density- vs uniform sampling.
#' @srrstats {SP4.0a} morie_spatial_transform() returns an object of the
#'   same (morie_spatial) class as its input.
#' @srrstats {SP4.1} Coordinate units (degrees vs metres) are carried on
#'   the object and updated by reprojection.
#' @srrstats {SP5.0} plot.morie_spatial() is the default map method.
#' @srrstats {SP5.1} The map places longitude/easting on x and
#'   latitude/northing on y.
#' @srrstats {SP5.2} Map axis labels include the coordinate units.
#' @srrstats {SP5.3} morie_spatial_leaflet_spec() returns a specification
#'   for an interactive (html) map.
#' @srrstats {SP6.0} A test confirms reprojection followed by its inverse
#'   recovers the original coordinates.
#' @srrstats {SP6.1} Functions are tested on both Cartesian (planar) and
#'   curvilinear (lon/lat) data.
#' @srrstats {SP6.1a} Distance on lon/lat uses the great-circle (Haversine)
#'   metric; a test contrasts it with planar distance.
#' @srrstats {SP6.1b} Moran's I is invariant to the coordinate system used
#'   for neighbour construction; a test confirms this.
#' @srrstats {SP6.2} A test includes extreme coordinates (near the poles /
#'   date line).
#' @srrstats {SP6.3} A test exercises every neighbour definition
#'   (knn / distance / rook / queen).
#' @srrstats {SP6.4} A test exercises different spatial-weighting schemes
#'   (binary vs row-standardised).
#' @srrstats {SP6.5} A test exercises spatial clustering
#'   (morie_spatial_cluster) on the coordinates.
#' @srrstats {SP6.6} A test confirms spatial weighting does not broadcast
#'   incommensurate inputs (the SP3.5 guard).
#' @noRd
NULL

#' Construct a spatial data object
#'
#' @param data A data.frame containing the coordinate columns and any
#'   attribute variables.
#' @param coords Length-2 character vector naming the x/longitude and
#'   y/latitude columns.
#' @param crs EPSG code of the coordinate reference system (4326 = WGS84
#'   lon/lat degrees; 3857 = Web Mercator metres).
#' @return A `morie_spatial` object.
#' @examples
#' d <- data.frame(lon = c(-79, -80), lat = c(43, 44), v = c(1, 2))
#' morie_spatial(d, c("lon", "lat"))
#' @export
morie_spatial <- function(data, coords = c("lon", "lat"), crs = 4326) {
  data <- .morie_check_data(data, required = coords, arg = "data")
  xy <- as.matrix(data[coords]); storage.mode(xy) <- "double"
  if (anyNA(xy)) stop("coordinates contain missing values", call. = FALSE)
  out <- list(coords = xy, data = data[setdiff(names(data), coords)],
              coord_names = coords, crs = crs,
              units = if (crs == 4326) "degrees" else "metres",
              n = nrow(xy))
  class(out) <- c("morie_spatial", "morie_rich_result", "list")
  out
}

#' Coordinate matrix of a spatial object
#' @param x A `morie_spatial`.
#' @return A two-column numeric coordinate matrix.
#' @examples
#' morie_spatial_coords(morie_spatial(
#'   data.frame(lon = 1:3, lat = 1:3)))
#' @export
morie_spatial_coords <- function(x) {
  stopifnot(inherits(x, "morie_spatial")); x$coords
}

#' CRS of a spatial object (EPSG + WKT2-style label)
#' @param x A `morie_spatial`.
#' @return A list with `epsg` and a `wkt` description (never a PROJ4 string).
#' @examples
#' morie_spatial_crs(morie_spatial(data.frame(lon = 1, lat = 1)))
#' @export
morie_spatial_crs <- function(x) {
  stopifnot(inherits(x, "morie_spatial"))
  wkt <- switch(as.character(x$crs),
    "4326" = "GEOGCRS[\"WGS 84\", EPSG:4326]",
    "3857" = "PROJCRS[\"WGS 84 / Pseudo-Mercator\", EPSG:3857]",
    sprintf("EPSG:%s", x$crs))
  list(epsg = x$crs, wkt = wkt)
}

#' Reproject a spatial object between CRS (4326 <-> 3857)
#'
#' Implements the Web-Mercator forward and inverse transforms exactly, so
#' a round trip recovers the original coordinates. Attribute data and (the
#' updated) CRS metadata are preserved.
#'
#' @param x A `morie_spatial`.
#' @param to_crs Target EPSG (4326 or 3857).
#' @return A `morie_spatial` in the target CRS.
#' @examples
#' s <- morie_spatial(data.frame(lon = -79, lat = 43))
#' morie_spatial_transform(s, 3857)
#' @export
morie_spatial_transform <- function(x, to_crs) {
  stopifnot(inherits(x, "morie_spatial"))
  R <- 6378137
  xy <- x$coords
  if (x$crs == to_crs) return(x)
  if (x$crs == 4326 && to_crs == 3857) {
    out_xy <- cbind(R * xy[, 1] * pi / 180,
                    R * log(tan(pi / 4 + (xy[, 2] * pi / 180) / 2)))
  } else if (x$crs == 3857 && to_crs == 4326) {
    out_xy <- cbind((xy[, 1] / R) * 180 / pi,
                    (2 * atan(exp(xy[, 2] / R)) - pi / 2) * 180 / pi)
  } else {
    stop("only 4326 <-> 3857 reprojection is supported", call. = FALSE)
  }
  colnames(out_xy) <- x$coord_names
  x$coords <- out_xy; x$crs <- to_crs
  x$units <- if (to_crs == 4326) "degrees" else "metres"
  x                                                    # same class (SP4.0a)
}

#' Internal helper: Sp Haversine
#' @noRd
.sp_haversine <- function(xy) {
  # great-circle distance matrix for lon/lat degrees (metres)
  R <- 6371000; rad <- xy * pi / 180
  n <- nrow(xy); d <- matrix(0, n, n)
  for (i in seq_len(n)) {
    dlon <- rad[, 1] - rad[i, 1]; dlat <- rad[, 2] - rad[i, 2]
    a <- sin(dlat / 2)^2 + cos(rad[i, 2]) * cos(rad[, 2]) * sin(dlon / 2)^2
    d[i, ] <- 2 * R * asin(pmin(1, sqrt(a)))
  }
  d
}

#' Distance matrix for a spatial object
#' @param x A `morie_spatial`.
#' @return A numeric distance matrix (great-circle for lon/lat, Euclidean
#'   for projected coordinates).
#' @examples
#' morie_spatial_distance(morie_spatial(data.frame(lon = c(0, 1), lat = c(0, 0))))
#' @export
morie_spatial_distance <- function(x) {
  stopifnot(inherits(x, "morie_spatial"))
  if (x$crs == 4326) .sp_haversine(x$coords)          # SP6.1a curvilinear
  else as.matrix(stats::dist(x$coords))               # SP6.1 Cartesian
}

#' Construct spatial neighbours
#' @param x A `morie_spatial`.
#' @param type "knn", "distance", "rook", or "queen". Rook/queen treat the
#'   points as a regular grid (contiguity by shared edge / edge+corner).
#' @param k Number of neighbours (knn).
#' @param d Distance band (distance).
#' @return A binary n-by-n neighbour matrix.
#' @examples
#' g <- expand.grid(lon = 1:3, lat = 1:3)
#' morie_spatial_neighbors(morie_spatial(g), type = "queen")
#' @export
morie_spatial_neighbors <- function(x, type = c("knn", "distance",
                                                "rook", "queen"),
                                    k = 4L, d = NULL) {
  type <- match.arg(type)
  dm <- morie_spatial_distance(x)
  n <- nrow(dm); W <- matrix(0L, n, n)
  if (type == "knn") {
    for (i in seq_len(n)) {
      nn <- order(dm[i, ])[2:(k + 1)]; W[i, nn] <- 1L
    }
  } else if (type == "distance") {
    if (is.null(d)) d <- stats::median(dm[dm > 0])
    W[dm > 0 & dm <= d] <- 1L
  } else {
    xy <- x$coords
    for (i in seq_len(n)) for (j in seq_len(n)) {
      if (i == j) next
      dx <- abs(xy[i, 1] - xy[j, 1]); dy <- abs(xy[i, 2] - xy[j, 2])
      ux <- min(diff(sort(unique(xy[, 1])))); uy <- min(diff(sort(unique(xy[, 2]))))
      adj_rook <- (dx <= ux + 1e-9 & dy < 1e-9) | (dy <= uy + 1e-9 & dx < 1e-9)
      adj_queen <- dx <= ux + 1e-9 & dy <= uy + 1e-9
      if (type == "rook" && adj_rook) W[i, j] <- 1L
      if (type == "queen" && adj_queen) W[i, j] <- 1L
    }
  }
  W
}

#' Spatial weights from a neighbour matrix
#' @param neighbors A binary neighbour matrix.
#' @param style "W" (row-standardised) or "B" (binary).
#' @return A weights matrix.
#' @examples
#' morie_spatial_weights(matrix(c(0,1,1,0), 2))
#' @export
morie_spatial_weights <- function(neighbors, style = c("W", "B")) {
  style <- match.arg(style)
  W <- as.matrix(neighbors)
  if (nrow(W) != ncol(W)) {                            # SP3.5 / SP6.6 guard
    stop("neighbour matrix must be square; refusing to broadcast",
         call. = FALSE)
  }
  if (style == "B") return(W)
  rs <- rowSums(W); rs[rs == 0] <- 1
  W / rs
}

#' Global Moran's I on a spatial object
#' @param x A `morie_spatial`.
#' @param var Name of the attribute variable, or a numeric vector.
#' @param neighbors Optional neighbour matrix (default knn, k = 4).
#' @return A list with Moran's `I` and the expected value under no
#'   autocorrelation.
#' @examples
#' g <- expand.grid(lon = 1:5, lat = 1:5)
#' g$v <- g$lon + g$lat
#' morie_spatial_moran(morie_spatial(g), "v")
#' @export
morie_spatial_moran <- function(x, var, neighbors = NULL) {
  stopifnot(inherits(x, "morie_spatial"))
  z <- if (is.character(var)) x$data[[var]] else as.numeric(var)
  if (is.null(neighbors)) neighbors <- morie_spatial_neighbors(x, "knn", k = 4L)
  Wt <- morie_spatial_weights(neighbors, "W")
  n <- length(z); zc <- z - mean(z)
  num <- sum(Wt * outer(zc, zc)); den <- sum(zc^2)
  Wsum <- sum(Wt)
  list(I = (n / Wsum) * (num / den), expected = -1 / (n - 1))
}

#' Sample points from a spatial object, optionally by local density
#' @param x A `morie_spatial`.
#' @param size Number of points to sample.
#' @param by_density If TRUE, sampling probability is proportional to
#'   local point density (denser areas more likely); if FALSE, uniform.
#' @param seed RNG seed.
#' @return Integer row indices of the sampled points.
#' @examples
#' morie_spatial_sample(morie_spatial(expand.grid(lon = 1:5, lat = 1:5)),
#'                      size = 5)
#' @export
morie_spatial_sample <- function(x, size, by_density = TRUE, seed = 42L) {
  stopifnot(inherits(x, "morie_spatial"))
  set.seed(seed)
  if (!by_density) return(sample.int(x$n, size))
  dm <- morie_spatial_distance(x)
  bw <- stats::median(dm[dm > 0])
  dens <- rowSums(exp(-(dm / bw)^2))                   # local density (SP3.2)
  sample.int(x$n, size, prob = dens / sum(dens))
}

#' Spatial clustering of the coordinates
#' @param x A `morie_spatial`.
#' @param k Number of clusters.
#' @return Integer cluster labels (via morie_cluster on coordinates).
#' @examples
#' morie_spatial_cluster(morie_spatial(expand.grid(lon = 1:6, lat = 1:6)), k = 3)
#' @export
morie_spatial_cluster <- function(x, k = 3L) {
  stopifnot(inherits(x, "morie_spatial"))
  cl <- morie_cluster(as.data.frame(x$coords), k = k)
  unname(cl$assignments)
}

#' Specification for an interactive (leaflet) map
#' @param x A `morie_spatial`.
#' @return A list describing an interactive html map (tiles, centre, zoom).
#' @examples
#' morie_spatial_leaflet_spec(morie_spatial(data.frame(lon = -79, lat = 43)))
#' @export
morie_spatial_leaflet_spec <- function(x) {
  stopifnot(inherits(x, "morie_spatial"))
  wgs <- if (x$crs != 4326) morie_spatial_transform(x, 4326) else x
  list(provider = "OpenStreetMap", interactive = TRUE,
       center = colMeans(wgs$coords), zoom = 8L, points = wgs$coords)
}

#' @param x A `morie_spatial`.
#' @param ... Passed to [plot()].
#' @return `NULL`, invisibly. Longitude/easting on x, latitude/northing on
#'   y, with units in the axis labels.
#' @export
plot.morie_spatial <- function(x, ...) {
  plot(x$coords[, 1], x$coords[, 2],
       xlab = sprintf("%s (%s)", x$coord_names[1], x$units),
       ylab = sprintf("%s (%s)", x$coord_names[2], x$units),
       pch = 19, ...)
  invisible(NULL)
}

#' @param x A `morie_spatial`.
#' @param ... Unused.
#' @return `x`, invisibly.
#' @export
print.morie_spatial <- function(x, ...) {
  cat(sprintf("<morie_spatial> n=%d  EPSG:%s (%s)\n", x$n, x$crs, x$units))
  invisible(x)
}

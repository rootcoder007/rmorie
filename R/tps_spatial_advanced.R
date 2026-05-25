# SPDX-License-Identifier: AGPL-3.0-or-later

#' Heavyweight spatial statistics for TPS data
#'
#' R parity of \code{morie.tps_spatial_advanced}. Builds on
#' \code{\link{tps_spatial}} (global Moran's I, LISA, KDE) with:
#'
#' \itemize{
#'   \item \code{\link{morie_tps_ripley_k}}: Ripley's K function for
#'     point-pattern clustering at multiple radii.
#'   \item \code{\link{morie_tps_getis_ord_g_star}}: local
#'     Getis-Ord Gi* hot/cold-spot z-scores.
#'   \item \code{\link{morie_tps_dbscan_clusters}}: density-based
#'     clusters on lat/long (via \pkg{dbscan}, optional).
#'   \item \code{\link{morie_tps_polygon_morans_i}}: polygon-aware
#'     Moran's I from an \pkg{sf} object's actual polygon centroids
#'     (instead of the centroid-only k-NN approximation in
#'     \code{\link{morie_tps_morans_i_neighbourhood}}).
#'   \item \code{\link{morie_tps_bivariate_moran}}: bivariate Moran's I
#'     between two attributes at the same polygons.
#'   \item \code{\link{morie_tps_moran_sweep_heatmap}}: a (category x
#'     year) sweep of polygon Moran's I.
#' }
#'
#' Polygon functions accept either an \pkg{sf} object (gated with
#' \code{requireNamespace("sf")}) or a plain data.frame carrying
#' precomputed centroid columns. KNN graphs prefer \pkg{FNN}; DBSCAN
#' requires the optional \pkg{dbscan} package; spatial autocorrelation
#' tests can optionally be delegated to \pkg{spdep}.
#'
#' @name tps_spatial_advanced
NULL


# ---------------------------------------------------------------------------
# Internal helpers
# ---------------------------------------------------------------------------

.tps_adv_result <- function(title, call,
                             summary_lines = list(),
                             warnings = character(0),
                             interpretation = "",
                             ...) {
  out <- list(
    title = title,
    call = call,
    summary_lines = summary_lines,
    warnings = warnings,
    interpretation = interpretation,
    ...
  )
  class(out) <- c("morie_tps_spatial_advanced_result",
                  "morie_rich_result", "list")
  out
}


.tps_coords <- function(df, lat_col, lon_col) {
  if (!all(c(lat_col, lon_col) %in% names(df))) {
    return(matrix(numeric(0), 0L, 2L))
  }
  a <- df[, c(lat_col, lon_col), drop = FALSE]
  a <- a[stats::complete.cases(a), , drop = FALSE]
  a <- a[a[[lat_col]] != 0 & a[[lon_col]] != 0, , drop = FALSE]
  as.matrix(a)
}


.tps_haversine_km <- function(lat1, lon1, lat2, lon2) {
  Rk <- 6371
  rad <- pi / 180
  dlat <- (lat2 - lat1) * rad
  dlon <- (lon2 - lon1) * rad
  a <- sin(dlat / 2)^2 + cos(lat1 * rad) * cos(lat2 * rad) * sin(dlon / 2)^2
  2 * Rk * asin(pmin(1, sqrt(a)))
}


.tps_knn_idx <- function(coords, k) {
  n <- nrow(coords)
  k <- min(as.integer(k), n - 1L)
  if (requireNamespace("FNN", quietly = TRUE)) {
    return(FNN::get.knn(coords, k = k)$nn.index)
  }
  idx <- matrix(0L, n, k)
  for (i in seq_len(n)) {
    diff <- sweep(coords, 2L, coords[i, ], "-")
    d <- sqrt(rowSums(diff * diff))
    d[i] <- Inf
    idx[i, ] <- order(d)[seq_len(k)]
  }
  idx
}


# ---------------------------------------------------------------------------
# 1. Ripley's K
# ---------------------------------------------------------------------------

#' Ripley's K function at multiple radii
#'
#' Computes Ripley's K(r) at each user-supplied radius (km), the
#' Besag-centred L(r)-r transformation, and the CSR baseline pi*r^2.
#' Coordinates are projected to km via the small-angle latitude
#' factor; for typical city-scale point patterns this is accurate
#' enough that haversine is unnecessary.
#'
#' @param df Incident-level data.frame.
#' @param ds_name Tag for the result title.
#' @param radii_km Numeric vector of radii in km (default 0.25, 0.5,
#'   1, 2, 3, 5).
#' @param max_n Subsample cap (default 5000) to keep the pairwise
#'   distance matrix tractable.
#' @param lat_col,lon_col WGS84 column names.
#' @return A named list with the per-radius table, intensity, and
#'   bounding-box area.
#' @examples
#' set.seed(2026)
#' df <- data.frame(
#'   LAT_WGS84 = 43.6 + rnorm(80, 0, 0.04),
#'   LONG_WGS84 = -79.4 + rnorm(80, 0, 0.04)
#' )
#' morie_tps_ripley_k(df, radii_km = c(0.5, 1, 2))
#' @export
morie_tps_ripley_k <- function(df,
                                ds_name = "?",
                                radii_km = c(0.25, 0.5, 1, 2, 3, 5),
                                max_n = 5000L,
                                lat_col = "LAT_WGS84",
                                lon_col = "LONG_WGS84") {
  stopifnot(is.data.frame(df), is.numeric(radii_km), length(radii_km) >= 1L)
  call <- sprintf("morie_tps_ripley_k(df=<%dr>)", nrow(df))
  title <- sprintf("Ripley's K -- %s", ds_name)

  coords <- .tps_coords(df, lat_col, lon_col)
  n0 <- nrow(coords)
  if (n0 < 50L) {
    return(.tps_adv_result(
      title, call,
      warnings = sprintf("only %d geocoded", n0),
      interpretation = "No analysis: fewer than 50 geocoded points.",
      n = as.integer(n0)
    ))
  }
  if (n0 > max_n) {
    set.seed(42L)
    coords <- coords[sample.int(n0, max_n), , drop = FALSE]
  }
  n <- nrow(coords)

  lat_mid <- mean(coords[, 1L])
  km_per_deg_lat <- 111
  km_per_deg_lon <- 111 * cos(lat_mid * pi / 180)
  lat_range_km <- (max(coords[, 1L]) - min(coords[, 1L])) * km_per_deg_lat
  lon_range_km <- (max(coords[, 2L]) - min(coords[, 2L])) * km_per_deg_lon
  area_km2 <- max(0.01, lat_range_km * lon_range_km)
  intensity <- n / area_km2

  coords_km <- cbind(coords[, 1L] * km_per_deg_lat,
                     coords[, 2L] * km_per_deg_lon)
  dist_km <- as.matrix(stats::dist(coords_km))
  diag(dist_km) <- Inf

  rows <- vector("list", length(radii_km))
  K_vec <- numeric(length(radii_km))
  L_vec <- numeric(length(radii_km))
  for (i in seq_along(radii_km)) {
    r <- radii_km[i]
    within <- sum(dist_km < r) / n
    K <- within / intensity
    K_csr <- pi * r * r
    L <- sqrt(K / pi) - r
    K_vec[i] <- K
    L_vec[i] <- L
    cmp <- if (K_csr * 1.05 < K) {
      "clustered"
    } else if (K_csr * 0.95 > K) {
      "regular"
    } else {
      "approx CSR"
    }
    rows[[i]] <- data.frame(
      r_km = r, avg_neigh = within, K_r = K,
      K_csr = K_csr, L_minus_r = L, vs_CSR = cmp,
      stringsAsFactors = FALSE
    )
  }
  tbl <- do.call(rbind, rows)

  n_clustered <- sum(tbl$vs_CSR == "clustered")
  interp <- sprintf(
    paste0(
      "Across %d point(s) with intensity %.3f points/km^2, K(r) ",
      "exceeds the Poisson CSR baseline at %d of %d radii ",
      "(=> clustering at those scales). The Besag L(r)-r ranges ",
      "from %+.3f to %+.3f; positive values indicate clustering, ",
      "negative values regularity."
    ),
    n, intensity, n_clustered, length(radii_km),
    min(L_vec), max(L_vec)
  )

  .tps_adv_result(
    title, call,
    summary_lines = list(
      `Points used` = as.integer(n),
      `Bounding-box area (km^2)` = round(area_km2, 2),
      `Intensity (pts/km^2)` = round(intensity, 3),
      `Radii (km)` = paste(radii_km, collapse = ", "),
      `Clustered radii` = as.integer(n_clustered)
    ),
    warnings = character(0),
    interpretation = interp,
    n = as.integer(n),
    intensity = intensity,
    area_km2 = area_km2,
    table = tbl,
    K = K_vec,
    L_minus_r = L_vec,
    radii_km = radii_km
  )
}


# ---------------------------------------------------------------------------
# 2. Getis-Ord Gi*
# ---------------------------------------------------------------------------

#' Local Getis-Ord Gi* statistic per neighbourhood
#'
#' Returns Gi* per neighbourhood (count vector aggregated from the
#' incident data.frame), using a binary k-NN spatial weights matrix
#' with self-inclusion (Gi* convention). z-score interpretation: Gi*
#' > 1.96 = significant hot spot at alpha=0.05; Gi* < -1.96 =
#' significant cold spot.
#'
#' @param df Incident-level data.frame.
#' @param ds_name Tag for the result title.
#' @param hood_col Neighbourhood id column.
#' @param k_neighbours k for the (binary) k-NN weights graph.
#' @param top_n Number of top hot/cold spots to surface.
#' @param lat_col,lon_col WGS84 column names.
#' @return A named list with Gi* per hood, the top hot/cold spot
#'   tables, and tallies of hot/cold spots at alpha=0.05.
#' @examples
#' set.seed(2026)
#' df <- data.frame(
#'   HOOD_158 = sample(letters[1:20], 400, replace = TRUE),
#'   LAT_WGS84 = 43.6 + runif(400, 0, 0.2),
#'   LONG_WGS84 = -79.4 + runif(400, 0, 0.2)
#' )
#' morie_tps_getis_ord_g_star(df)
#' @export
morie_tps_getis_ord_g_star <- function(df,
                                        ds_name = "?",
                                        hood_col = "HOOD_158",
                                        k_neighbours = 5L,
                                        top_n = 20L,
                                        lat_col = "LAT_WGS84",
                                        lon_col = "LONG_WGS84") {
  stopifnot(is.data.frame(df))
  call <- sprintf(
    "morie_tps_getis_ord_g_star(df=<%dr>, hood_col=%s, k=%d)",
    nrow(df), hood_col, as.integer(k_neighbours)
  )
  title <- sprintf("Getis-Ord Gi* -- %s", ds_name)

  if (!(hood_col %in% names(df)) || !all(c(lat_col, lon_col) %in% names(df))) {
    return(.tps_adv_result(
      title, call,
      warnings = sprintf("%s or %s/%s missing", hood_col, lat_col, lon_col),
      interpretation = "No analysis: required columns absent.",
      n = 0L
    ))
  }

  counts <- df[[hood_col]]
  counts <- counts[!is.na(counts)]
  counts <- counts[toupper(as.character(counts)) != "NSA"]
  counts <- sort(table(counts), decreasing = TRUE)

  keep <- stats::complete.cases(df[, c(hood_col, lat_col, lon_col)])
  d <- df[keep, , drop = FALSE]
  cent <- aggregate(d[, c(lat_col, lon_col)], by = list(d[[hood_col]]), mean)
  rownames(cent) <- as.character(cent[[1L]])
  cent <- cent[, c(lat_col, lon_col), drop = FALSE]

  common <- intersect(names(counts), rownames(cent))
  counts <- counts[common]
  cent <- cent[common, , drop = FALSE]
  n <- length(counts)
  if (n < 5L) {
    return(.tps_adv_result(
      title, call,
      warnings = sprintf("only %d valid hoods", n),
      interpretation = "No analysis: fewer than 5 valid neighbourhoods.",
      n = as.integer(n)
    ))
  }

  coords <- as.matrix(cent)
  k <- min(as.integer(k_neighbours), n - 1L)
  idx <- .tps_knn_idx(coords, k = k)
  W <- matrix(0L, n, n)
  for (i in seq_len(n)) W[i, idx[i, ]] <- 1L
  diag(W) <- 1L # Gi* INCLUDES self

  x <- as.numeric(counts)
  x_bar <- mean(x)
  s <- stats::sd(x) * sqrt((n - 1) / n) # population sd (ddof=0)
  if (!is.finite(s) || s <= 0) s <- 1e-9

  Gi <- numeric(n)
  for (i in seq_len(n)) {
    wi <- W[i, ]
    sum_wi <- sum(wi)
    num <- sum(wi * x) - x_bar * sum_wi
    denom_inside <- (n * sum(wi * wi) - sum_wi^2) / max(1L, n - 1L)
    denom <- s * sqrt(max(0, denom_inside))
    Gi[i] <- if (denom > 0) num / denom else 0
  }

  out_tbl <- data.frame(
    hood = names(counts),
    count = as.integer(x),
    z_score = Gi,
    stringsAsFactors = FALSE
  )
  out_tbl <- out_tbl[order(out_tbl$z_score, decreasing = TRUE), , drop = FALSE]

  hotspots <- sum(Gi > 1.96)
  coldspots <- sum(Gi < -1.96)
  top_hot <- utils::head(out_tbl, as.integer(top_n))
  top_cold <- utils::tail(out_tbl, as.integer(top_n))
  top_cold <- top_cold[order(top_cold$z_score), , drop = FALSE]

  interp <- sprintf(
    paste0(
      "Across %d neighbourhood(s) with a binary k=%d-NN weights ",
      "matrix (self-inclusive), Gi* identifies %d hot spot(s) ",
      "(Gi* > 1.96) and %d cold spot(s) (Gi* < -1.96) at alpha=0.05. ",
      "Max Gi* = %+.2f at '%s'; min Gi* = %+.2f at '%s'."
    ),
    n, k + 1L, hotspots, coldspots,
    max(Gi), out_tbl$hood[1L],
    min(Gi), out_tbl$hood[nrow(out_tbl)]
  )

  .tps_adv_result(
    title, call,
    summary_lines = list(
      `Spatial unit` = hood_col,
      `Neighbourhoods` = as.integer(n),
      `k-NN (incl. self)` = as.integer(k + 1L),
      `Hotspots (Gi* > 1.96)` = as.integer(hotspots),
      `Cold spots (Gi* < -1.96)` = as.integer(coldspots),
      `Max Gi*` = max(Gi),
      `Min Gi*` = min(Gi)
    ),
    warnings = character(0),
    interpretation = interp,
    n = as.integer(n),
    table = out_tbl,
    top_hot = top_hot,
    top_cold = top_cold,
    Gi = Gi,
    n_hotspots = hotspots,
    n_coldspots = coldspots
  )
}


# ---------------------------------------------------------------------------
# 3. DBSCAN density clusters
# ---------------------------------------------------------------------------

#' DBSCAN density clusters on lat/long
#'
#' Requires the optional \pkg{dbscan} package. Coordinates are
#' projected to km via the small-angle latitude factor so \code{eps_km}
#' is interpretable as a kilometre-scale radius.
#'
#' @param df Incident-level data.frame.
#' @param ds_name Tag for the result title.
#' @param eps_km Neighbourhood radius in km.
#' @param min_samples DBSCAN \code{minPts} parameter.
#' @param max_n Subsample cap to keep DBSCAN tractable.
#' @param lat_col,lon_col WGS84 column names.
#' @return A named list with the per-cluster table, the count of
#'   noise points, and the largest-cluster size.
#' @examples
#' if (requireNamespace("dbscan", quietly = TRUE)) {
#'   set.seed(2026)
#'   df <- data.frame(
#'     LAT_WGS84 = c(rnorm(60, 43.65, 0.005), rnorm(60, 43.70, 0.005)),
#'     LONG_WGS84 = c(rnorm(60, -79.40, 0.005), rnorm(60, -79.38, 0.005))
#'   )
#'   morie_tps_dbscan_clusters(df, eps_km = 0.5, min_samples = 5L)
#' }
#' @export
morie_tps_dbscan_clusters <- function(df,
                                       ds_name = "?",
                                       eps_km = 0.25,
                                       min_samples = 30L,
                                       max_n = 30000L,
                                       lat_col = "LAT_WGS84",
                                       lon_col = "LONG_WGS84") {
  stopifnot(is.data.frame(df))
  call <- sprintf(
    "morie_tps_dbscan_clusters(df=<%dr>, eps_km=%.4g, min_samples=%d)",
    nrow(df), eps_km, as.integer(min_samples)
  )
  title <- sprintf("DBSCAN density clusters -- %s", ds_name)

  if (!requireNamespace("dbscan", quietly = TRUE)) {
    return(.tps_adv_result(
      title, call,
      warnings = "optional package 'dbscan' not installed",
      interpretation = paste(
        "No analysis: the optional 'dbscan' package is required for",
        "this callable. Install with install.packages('dbscan')."
      ),
      n = 0L
    ))
  }

  coords <- .tps_coords(df, lat_col, lon_col)
  n0 <- nrow(coords)
  if (n0 < 50L) {
    return(.tps_adv_result(
      title, call,
      warnings = sprintf("only %d geocoded", n0),
      interpretation = "No analysis: fewer than 50 geocoded points.",
      n = as.integer(n0)
    ))
  }
  if (n0 > max_n) {
    set.seed(42L)
    coords <- coords[sample.int(n0, max_n), , drop = FALSE]
  }
  n <- nrow(coords)

  lat_mid <- mean(coords[, 1L])
  km_per_deg_lat <- 111
  km_per_deg_lon <- 111 * cos(lat_mid * pi / 180)
  coords_km <- cbind(coords[, 1L] * km_per_deg_lat,
                     coords[, 2L] * km_per_deg_lon)

  db <- dbscan::dbscan(coords_km, eps = eps_km, minPts = as.integer(min_samples))
  labels <- db$cluster
  n_clusters <- length(setdiff(unique(labels), 0L))
  n_noise <- sum(labels == 0L)

  rows <- list()
  for (cl in sort(setdiff(unique(labels), 0L))) {
    mask <- labels == cl
    cl_coords <- coords[mask, , drop = FALSE]
    rows[[length(rows) + 1L]] <- data.frame(
      cluster_id = as.integer(cl),
      size = as.integer(sum(mask)),
      centroid_lat = round(mean(cl_coords[, 1L]), 5),
      centroid_lon = round(mean(cl_coords[, 2L]), 5),
      stringsAsFactors = FALSE
    )
  }
  tbl <- if (length(rows) > 0L) do.call(rbind, rows) else data.frame(
    cluster_id = integer(0), size = integer(0),
    centroid_lat = numeric(0), centroid_lon = numeric(0)
  )
  if (nrow(tbl) > 0L) {
    tbl <- tbl[order(tbl$size, decreasing = TRUE), , drop = FALSE]
  }
  largest <- if (nrow(tbl) > 0L) tbl$size[1L] else 0L

  interp <- sprintf(
    paste0(
      "DBSCAN with eps=%.3g km, minPts=%d on %d point(s) found %d ",
      "cluster(s); %d point(s) classified as noise (no nearby ",
      "cluster). Largest cluster has %d member(s)."
    ),
    eps_km, as.integer(min_samples), n, n_clusters, n_noise, largest
  )

  .tps_adv_result(
    title, call,
    summary_lines = list(
      `Points clustered` = as.integer(n),
      `eps (km)` = eps_km,
      `min_samples` = as.integer(min_samples),
      `Clusters discovered` = as.integer(n_clusters),
      `Noise points` = as.integer(n_noise),
      `Largest cluster size` = as.integer(largest)
    ),
    warnings = character(0),
    interpretation = interp,
    n = as.integer(n),
    n_clusters = as.integer(n_clusters),
    n_noise = as.integer(n_noise),
    largest_cluster = as.integer(largest),
    eps_km = eps_km,
    min_samples = as.integer(min_samples),
    table = tbl,
    labels = labels
  )
}


# ---------------------------------------------------------------------------
# 4. Polygon-based Moran's I
# ---------------------------------------------------------------------------

.tps_polygon_centroids <- function(polygons) {
  # `polygons` is an sf object with a geometry column. Use sf if available
  if (requireNamespace("sf", quietly = TRUE) && inherits(polygons, "sf")) {
    g <- sf::st_geometry(polygons)
    cent <- suppressWarnings(sf::st_centroid(g))
    cm <- sf::st_coordinates(cent)
    return(cbind(lon = cm[, 1L], lat = cm[, 2L]))
  }
  NULL
}


#' Polygon-aware Moran's I on a value column
#'
#' Accepts an \pkg{sf} object (recommended) carrying neighbourhood
#' polygons and a numeric value column, computes polygon centroids
#' via \code{sf::st_centroid}, then runs Moran's I with a k-NN row-
#' standardised weights matrix on those centroids. Falls back to a
#' \code{data.frame} carrying precomputed centroid columns when
#' \pkg{sf} is unavailable.
#'
#' @param polygons An \pkg{sf} object, or a data.frame with centroid
#'   columns.
#' @param value_col Column to test for spatial autocorrelation.
#' @param ds_name Tag for the result title.
#' @param k_neighbours k for the k-NN weights graph.
#' @param centroid_lat_col,centroid_lon_col Names of the centroid
#'   columns when \code{polygons} is a plain data.frame.
#' @return A named list with \code{moran_I}, \code{z_score},
#'   \code{p_value}, \code{n}.
#' @examples
#' set.seed(2026)
#' polys <- data.frame(
#'   HOOD_ID = letters[1:16],
#'   lat = rep(43.6 + (0:3) * 0.02, 4),
#'   lon = rep(-79.4 + (0:3) * 0.02, each = 4),
#'   ASSAULT_RATE_2024 = rpois(16, 30)
#' )
#' morie_tps_polygon_morans_i(polys, value_col = "ASSAULT_RATE_2024",
#'   centroid_lat_col = "lat", centroid_lon_col = "lon")
#' @export
morie_tps_polygon_morans_i <- function(polygons,
                                        value_col,
                                        ds_name = "NeighbourhoodCrimeRates",
                                        k_neighbours = 5L,
                                        centroid_lat_col = "lat",
                                        centroid_lon_col = "lon") {
  stopifnot(is.character(value_col), length(value_col) == 1L)
  call <- sprintf(
    "morie_tps_polygon_morans_i(value_col=%s, k=%d)",
    value_col, as.integer(k_neighbours)
  )
  title <- sprintf("Polygon Moran's I -- %s (%s)", ds_name, value_col)

  cents <- .tps_polygon_centroids(polygons)
  df_attr <- if (inherits(polygons, "sf")) {
    # sf object: drop geometry to extract attribute table
    sf::st_drop_geometry(polygons)
  } else {
    polygons
  }

  if (!is.data.frame(df_attr)) {
    return(.tps_adv_result(
      title, call,
      warnings = "polygons must be an sf object or a data.frame",
      interpretation = "No analysis: polygons input not recognised.",
      n = 0L
    ))
  }
  if (!(value_col %in% names(df_attr))) {
    return(.tps_adv_result(
      title, call,
      warnings = sprintf("%s not in attribute table", value_col),
      interpretation = sprintf(
        "No analysis: value column '%s' is absent from the polygons input.",
        value_col
      ),
      n = 0L
    ))
  }

  if (is.null(cents)) {
    if (!all(c(centroid_lat_col, centroid_lon_col) %in% names(df_attr))) {
      return(.tps_adv_result(
        title, call,
        warnings = "sf not installed and no precomputed centroid columns supplied",
        interpretation = paste(
          "No analysis: install the optional 'sf' package, or pass a",
          "data.frame with precomputed centroid columns named by",
          "centroid_lat_col / centroid_lon_col."
        ),
        n = 0L
      ))
    }
    cents <- cbind(lon = as.numeric(df_attr[[centroid_lon_col]]),
                   lat = as.numeric(df_attr[[centroid_lat_col]]))
  }

  vals <- suppressWarnings(as.numeric(df_attr[[value_col]]))
  keep <- is.finite(cents[, 1L]) & is.finite(cents[, 2L]) & is.finite(vals)
  cents <- cents[keep, , drop = FALSE]
  vals <- vals[keep]
  n <- nrow(cents)
  if (n < 5L) {
    return(.tps_adv_result(
      title, call,
      warnings = sprintf("only %d usable centroid+value pairs", n),
      interpretation = "No analysis: fewer than 5 usable polygons.",
      n = as.integer(n)
    ))
  }

  k <- min(as.integer(k_neighbours), n - 1L)
  idx <- .tps_knn_idx(cents, k = k)
  W <- matrix(0, n, n)
  for (i in seq_len(n)) W[i, idx[i, ]] <- 1
  rsum <- rowSums(W)
  rsum[rsum == 0] <- 1
  W <- W / rsum

  x <- vals
  z <- x - mean(x)
  S0 <- sum(W)
  if (S0 == 0 || sum(z * z) == 0) {
    return(.tps_adv_result(
      title, call,
      warnings = "S0 or variance is zero",
      interpretation = "No analysis: zero spatial weights or zero variance.",
      n = as.integer(n)
    ))
  }
  I_val <- (n / S0) * as.numeric(t(z) %*% W %*% z) / sum(z * z)
  expected_I <- -1 / (n - 1)

  W_sym <- (W + t(W)) / 2
  S1 <- 2 * sum(W_sym^2)
  S2 <- sum((colSums(W) + rowSums(W))^2)
  var_I <- (n * (n - 2) * S1 - 2 * n * S2 + 6 * S0^2) /
    ((n - 1) * (n + 1) * (n - 2) * S0^2 + 1e-300)
  z_I <- if (is.finite(var_I) && var_I > 0) (I_val - expected_I) / sqrt(var_I) else NA_real_
  p <- if (is.finite(z_I)) 2 * stats::pnorm(-abs(z_I)) else NA_real_

  interp <- sprintf(
    paste0(
      "Polygon Moran's I on '%s' across %d polygon(s) with a k=%d-NN ",
      "weights matrix: I = %+.3f (z = %+.2f, two-sided p = %.4g). ",
      "Positive I means neighbouring polygons share similar values ",
      "(spatial autocorrelation in the underlying rate)."
    ),
    value_col, n, k, I_val,
    if (is.finite(z_I)) z_I else NA_real_,
    if (is.finite(p)) p else NA_real_
  )

  .tps_adv_result(
    title, call,
    summary_lines = list(
      `Variable` = value_col,
      `Polygons` = as.integer(n),
      `k-NN` = as.integer(k),
      `Moran's I` = I_val,
      `Expected I` = expected_I,
      `Var(I)` = var_I,
      `z-score` = z_I,
      `p-value (two-sided)` = p
    ),
    warnings = character(0),
    interpretation = interp,
    n = as.integer(n),
    moran_I = I_val,
    expected_I = expected_I,
    var_I = var_I,
    z_score = z_I,
    p_value = p,
    value_col = value_col
  )
}


# ---------------------------------------------------------------------------
# 5. Bivariate Moran's I
# ---------------------------------------------------------------------------

#' Bivariate Moran's I between two attributes at the same polygons
#'
#' Generalises \code{\link{morie_tps_polygon_morans_i}} to two
#' attributes: measures the cross-correlation between attribute X at
#' location i and attribute Y at neighbouring locations j.
#'
#' \deqn{I_{xy} = \frac{n}{S_0}\,\frac{\sum_i \sum_j w_{ij}\, z^x_i\, z^y_j}{\sqrt{\sum_i (z^x_i)^2 \cdot \sum_i (z^y_i)^2}}}{I_xy = (n)/(S_0) frac{sum_i sum_j w_ij z^x_i z^y_j}{sqrt(sum_i (z^x_i)^2 * sum_i (z^y_i)^2)}}
#'
#' Polygon centroids and k-NN weights are constructed exactly as in
#' \code{\link{morie_tps_polygon_morans_i}}; distances use the
#' haversine formula for parity with the Python source.
#'
#' @param polygons An \pkg{sf} object, or a data.frame with centroid
#'   columns.
#' @param x_col,y_col The two attributes (column names).
#' @param ds_name Tag for the result title.
#' @param k_neighbours k for the k-NN weights graph.
#' @param centroid_lat_col,centroid_lon_col Names of centroid columns
#'   when \code{polygons} is a plain data.frame.
#' @return A named list with \code{I_xy}, \code{n}, \code{x_col},
#'   \code{y_col}.
#' @examples
#' set.seed(2026)
#' polys <- data.frame(
#'   HOOD_ID = letters[1:16],
#'   lat = rep(43.6 + (0:3) * 0.02, 4),
#'   lon = rep(-79.4 + (0:3) * 0.02, each = 4),
#'   ASSAULT_RATE_2024 = rpois(16, 30),
#'   HOMICIDE_RATE_2024 = rpois(16, 2)
#' )
#' morie_tps_bivariate_moran(polys,
#'   x_col = "ASSAULT_RATE_2024",
#'   y_col = "HOMICIDE_RATE_2024",
#'   centroid_lat_col = "lat", centroid_lon_col = "lon")
#' @export
morie_tps_bivariate_moran <- function(polygons,
                                       x_col,
                                       y_col,
                                       ds_name = "NeighbourhoodCrimeRates",
                                       k_neighbours = 5L,
                                       centroid_lat_col = "lat",
                                       centroid_lon_col = "lon") {
  stopifnot(is.character(x_col), is.character(y_col))
  call <- sprintf(
    "morie_tps_bivariate_moran(x=%s, y=%s, k=%d)",
    x_col, y_col, as.integer(k_neighbours)
  )
  title <- sprintf("Bivariate Moran's I -- %s vs %s", x_col, y_col)

  cents <- .tps_polygon_centroids(polygons)
  df_attr <- if (inherits(polygons, "sf")) sf::st_drop_geometry(polygons) else polygons
  if (!is.data.frame(df_attr)) {
    return(.tps_adv_result(
      title, call,
      warnings = "polygons must be an sf object or a data.frame",
      interpretation = "No analysis: polygons input not recognised.",
      n = 0L
    ))
  }
  missing <- setdiff(c(x_col, y_col), names(df_attr))
  if (length(missing) > 0L) {
    return(.tps_adv_result(
      title, call,
      warnings = sprintf("missing column(s): %s", paste(missing, collapse = ", ")),
      interpretation = "No analysis: required attribute columns absent.",
      n = 0L
    ))
  }

  if (is.null(cents)) {
    if (!all(c(centroid_lat_col, centroid_lon_col) %in% names(df_attr))) {
      return(.tps_adv_result(
        title, call,
        warnings = "sf not installed and no precomputed centroid columns supplied",
        interpretation = paste(
          "No analysis: install the optional 'sf' package, or pass a",
          "data.frame with precomputed centroid columns named by",
          "centroid_lat_col / centroid_lon_col."
        ),
        n = 0L
      ))
    }
    cents <- cbind(lon = as.numeric(df_attr[[centroid_lon_col]]),
                   lat = as.numeric(df_attr[[centroid_lat_col]]))
  }

  x_arr <- suppressWarnings(as.numeric(df_attr[[x_col]]))
  y_arr <- suppressWarnings(as.numeric(df_attr[[y_col]]))
  keep <- is.finite(cents[, 1L]) & is.finite(cents[, 2L]) &
    is.finite(x_arr) & is.finite(y_arr)
  cents <- cents[keep, , drop = FALSE]
  x_arr <- x_arr[keep]
  y_arr <- y_arr[keep]
  n <- nrow(cents)
  if (n < 5L) {
    return(.tps_adv_result(
      title, call,
      warnings = sprintf("only %d valid (centroid, x, y) triples; need >= 5", n),
      interpretation = "No analysis: fewer than 5 usable polygons.",
      n = as.integer(n)
    ))
  }

  zx <- (x_arr - mean(x_arr)) / (stats::sd(x_arr) * sqrt((n - 1) / n) + 1e-300)
  zy <- (y_arr - mean(y_arr)) / (stats::sd(y_arr) * sqrt((n - 1) / n) + 1e-300)

  k <- min(as.integer(k_neighbours), n - 1L)
  W <- matrix(0, n, n)
  for (i in seq_len(n)) {
    d <- .tps_haversine_km(cents[i, 2L], cents[i, 1L],
                            cents[, 2L], cents[, 1L])
    d[i] <- Inf
    nn <- order(d)[seq_len(k)]
    W[i, nn] <- 1
  }
  rs <- rowSums(W)
  rs[rs == 0] <- 1
  Wn <- W / rs
  S0 <- sum(Wn)
  cross <- sum(Wn * outer(zx, zy))
  norm <- sqrt(sum(zx^2) * sum(zy^2))
  I_xy <- if (norm > 0) (n / S0) * cross / norm else NA_real_

  interp <- sprintf(
    paste0(
      "Bivariate Moran's I_xy between '%s' and '%s' across %d ",
      "polygons (k=%d-NN haversine weights): I_xy = %+.3f. ",
      "Positive => high X at i tends to occur near high Y at ",
      "neighbours j; negative => spatial mismatch between the ",
      "two attributes."
    ),
    x_col, y_col, n, k, I_xy
  )

  .tps_adv_result(
    title, call,
    summary_lines = list(
      `X column` = x_col,
      `Y column` = y_col,
      `Polygons` = as.integer(n),
      `k-NN` = as.integer(k),
      `Bivariate I_xy` = I_xy
    ),
    warnings = character(0),
    interpretation = interp,
    n = as.integer(n),
    I_xy = I_xy,
    x_col = x_col,
    y_col = y_col
  )
}


# ---------------------------------------------------------------------------
# 6. Moran sweep heatmap (category x year)
# ---------------------------------------------------------------------------

#' Sweep polygon Moran's I across (category x year)
#'
#' Loops \code{\link{morie_tps_polygon_morans_i}} over a grid of
#' value-column prefixes and years, returning the resulting matrix
#' of Moran's I values for downstream visualisation as a heatmap.
#'
#' Column names are constructed as \code{paste0(prefix, "_", year)}.
#'
#' @param polygons An \pkg{sf} object or data.frame with centroid
#'   columns and per-year value columns.
#' @param category_prefixes Character vector of column prefixes.
#'   Defaults to the 9 published TPS rate categories.
#' @param years Integer vector of years. Defaults to 2014:2024.
#' @param k_neighbours k for the k-NN weights graph passed down.
#' @param ds_name Tag for the result title.
#' @param centroid_lat_col,centroid_lon_col Centroid column names
#'   forwarded to \code{morie_tps_polygon_morans_i}.
#' @return A named list with the (category x year) Moran's I matrix.
#' @examples
#' set.seed(2026)
#' polys <- data.frame(
#'   HOOD_ID = letters[1:16],
#'   lat = rep(43.6 + (0:3) * 0.02, 4),
#'   lon = rep(-79.4 + (0:3) * 0.02, each = 4),
#'   ASSAULT_RATE_2023 = rpois(16, 30),
#'   ASSAULT_RATE_2024 = rpois(16, 32),
#'   HOMICIDE_RATE_2023 = rpois(16, 2),
#'   HOMICIDE_RATE_2024 = rpois(16, 2)
#' )
#' morie_tps_moran_sweep_heatmap(polys,
#'   category_prefixes = c("ASSAULT_RATE", "HOMICIDE_RATE"),
#'   years = c(2023L, 2024L),
#'   centroid_lat_col = "lat", centroid_lon_col = "lon")
#' @export
morie_tps_moran_sweep_heatmap <- function(polygons,
                                           category_prefixes = NULL,
                                           years = NULL,
                                           k_neighbours = 5L,
                                           ds_name = "NeighbourhoodCrimeRates",
                                           centroid_lat_col = "lat",
                                           centroid_lon_col = "lon") {
  if (is.null(category_prefixes)) {
    category_prefixes <- c(
      "ASSAULT_RATE", "AUTOTHEFT_RATE", "BIKETHEFT_RATE",
      "BREAKENTER_RATE", "HOMICIDE_RATE", "ROBBERY_RATE",
      "SHOOTING_RATE", "THEFTFROMMV_RATE", "THEFTOVER_RATE"
    )
  }
  if (is.null(years)) years <- 2014L:2024L
  category_prefixes <- as.character(category_prefixes)
  years <- as.integer(years)

  call <- sprintf(
    "morie_tps_moran_sweep_heatmap(cats=%d, years=%d)",
    length(category_prefixes), length(years)
  )
  title <- sprintf(
    "Moran's I sweep -- %d categories x %d years",
    length(category_prefixes), length(years)
  )

  M <- matrix(NA_real_,
              nrow = length(category_prefixes),
              ncol = length(years),
              dimnames = list(category_prefixes, as.character(years)))
  for (i in seq_along(category_prefixes)) {
    for (j in seq_along(years)) {
      col <- sprintf("%s_%d", category_prefixes[i], years[j])
      res <- tryCatch(
        morie_tps_polygon_morans_i(
          polygons,
          value_col = col,
          ds_name = ds_name,
          k_neighbours = k_neighbours,
          centroid_lat_col = centroid_lat_col,
          centroid_lon_col = centroid_lon_col
        ),
        error = function(e) NULL
      )
      mi <- if (!is.null(res)) res$moran_I else NULL
      if (!is.null(mi) && length(mi) == 1L && isTRUE(is.finite(mi))) {
        M[i, j] <- mi
      }
    }
  }

  has_any <- any(is.finite(M))
  interp <- sprintf(
    paste0(
      "Swept polygon Moran's I across %d category prefix(es) and ",
      "%d year(s) (%d-%d). Min I = %s, max I = %s; positive values ",
      "indicate spatial autocorrelation in the corresponding crime ",
      "rate across neighbourhoods."
    ),
    length(category_prefixes), length(years),
    years[1L], years[length(years)],
    if (has_any) sprintf("%+.3f", min(M, na.rm = TRUE)) else "n/a",
    if (has_any) sprintf("%+.3f", max(M, na.rm = TRUE)) else "n/a"
  )

  .tps_adv_result(
    title, call,
    summary_lines = list(
      `Categories` = as.integer(length(category_prefixes)),
      `Years` = sprintf("%d-%d", years[1L], years[length(years)]),
      `Min I` = if (has_any) min(M, na.rm = TRUE) else NA_real_,
      `Max I` = if (has_any) max(M, na.rm = TRUE) else NA_real_
    ),
    warnings = character(0),
    interpretation = interp,
    n = as.integer(length(category_prefixes) * length(years)),
    matrix = M,
    category_prefixes = category_prefixes,
    years = years
  )
}


# ---------------------------------------------------------------------------
# Print method
# ---------------------------------------------------------------------------

#' @export
print.morie_tps_spatial_advanced_result <- function(x, ...) {
  cat(x$title, "\
", strrep("=", nchar(x$title)), "\
", sep = "")
  if (!is.null(x$call) && nzchar(x$call)) {
    cat("Call:", x$call, "\
\
", sep = " ")
  }
  if (length(x$summary_lines) > 0L) {
    nms <- names(x$summary_lines)
    label_w <- max(nchar(nms))
    for (i in seq_along(x$summary_lines)) {
      v <- x$summary_lines[[i]]
      if (is.numeric(v) && length(v) == 1L && is.finite(v)) {
        v <- format(v, digits = 5)
      }
      cat(sprintf("  %-*s  %s\
", label_w, nms[i], format(v)))
    }
    cat("\
")
  }
  if (length(x$warnings) > 0L) {
    for (w in x$warnings) cat("Warning:", w, "\
")
    cat("\
")
  }
  if (nzchar(x$interpretation)) {
    cat(x$interpretation, "\
")
  }
  invisible(x)
}

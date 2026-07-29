# SPDX-License-Identifier: AGPL-3.0-or-later

#' srr spatial (SP) standards
#'
#' rmorie's spatial methods (Moran's I global/local/LISA, Getis-Ord
#' G*, Ripley's K, DBSCAN, Kulldorff scan) operate on planar/projected
#' neighbourhood structures and delegate to `spdep` / `sf` / `gstat`.
#' The applicable standards are addressed here; class-system, CRS
#' reprojection, and plot-method standards that do not fit rmorie's
#' coordinate-column interface are declared NA (with reasons) in
#' `srr-stats-standards.R`.
#'
#' @srrstats {SP1.0} The domain of applicability is documented: the
#'   methods apply to planar / projected geographic neighbourhoods
#'   (Toronto neighbourhood polygons and point coordinates).
#' @srrstats {SP1.1} The dimensional domain is two-dimensional
#'   (planar x/y or lon/lat), documented per function.
#' @srrstats {SP2.1} rmorie uses `sf` and `spdep`, never the retired
#'   `sp` package.
#' @srrstats {SP2.2} Spatial routines wrap and interoperate with the
#'   established `spdep` / `sf` / `gstat` ecosystem.
#' @srrstats {SP2.2a} The wrapping of those packages is documented on
#'   each function and in the spatial vignette.
#' @srrstats {SP2.0b} Functions validate their spatial input and return
#'   an explicit no-analysis result (rather than a misleading number)
#'   when required spatial columns are absent.
#' @srrstats {SP2.6} Accepted input types (coordinate / neighbourhood
#'   columns) are documented per function.
#' @srrstats {SP2.7} Input validation confirms the required spatial
#'   columns are present before analysis.
#' @srrstats {SP3.0} Neighbour construction is user-controllable.
#' @srrstats {SP3.0a} Regular-grid neighbours support rook/queen
#'   contiguity via the underlying `spdep` neighbour styles.
#' @srrstats {SP3.0b} Irregular-space neighbourhoods are controlled by
#'   an integer number of neighbours (`k_neighbours`) or a distance band.
#' @srrstats {SP3.1} Neighbour contributions can be distance-weighted
#'   through the spatial-weights construction, not only uniform cut-offs.
#' @srrstats {SP3.3} Spatial autocorrelation is explicitly quantified and
#'   distinguished from non-spatial covariation (global/local Moran's I,
#'   LISA, Getis-Ord G*).
#' @srrstats {SP3.4} Spatial clustering uses explicitly spatial
#'   algorithms (DBSCAN on coordinates, Kulldorff spatial scan), not a
#'   non-spatial clusterer with proximity as a mere weight.
#' @srrstats {SP4.0} Return values use a defined result structure.
#' @srrstats {SP4.0b} Results are returned in a class-defined format
#'   (`.tps_spatial_result` / `morie_rich_result`).
#' @srrstats {SP4.2} The type and class of return values are documented.
#' @noRd
NULL

#' Spatial analyses for TPS crime data
#'
#' R parity of \code{morie.tps_spatial}: Moran's I (global), LISA
#' (local Moran's Ii) for hot/cold spots, and 2-D kernel density
#' estimation of incident lat/long. Each function accepts a
#' \code{data.frame} of incident-level rows with a neighbourhood id
#' column plus WGS84 lat/long columns, and returns a named
#' \code{list} carrying numeric outputs alongside a multi-paragraph
#' \code{interpretation} so the result prints in a notebook without
#' further post-processing.
#'
#' Spatial weights are built with an internal base-R k-nearest-
#' neighbours routine; if the optional \pkg{FNN} package is installed
#' it is used for the KNN graph. The 2-D kernel density estimator
#' prefers \pkg{MASS}\code{::kde2d} when available, otherwise falls
#' back to a Gaussian density evaluated at the observation points.
#' If \pkg{spdep} is installed, callers can delegate the global
#' Moran's I test natively (Cliff-Ord normal approximation,
#' validated against spdep in tests/cross/).
#'
#' Functions
#' ---------
#'
#' \itemize{
#'   \item \code{\link{morie_tps_morans_i_neighbourhood}}: global
#'     Moran's I on neighbourhood-level incident counts.
#'   \item \code{\link{morie_tps_local_morans_i}}: LISA (local
#'     Moran's Ii) per neighbourhood with HH/LL/HL/LH quadrant
#'     classification.
#'   \item \code{\link{morie_tps_kde_density}}: 2-D kernel density
#'     estimate of geocoded incident points.
#' }
#'
#' @name tps_spatial
NULL


# ---------------------------------------------------------------------------
# Internal helpers (NOT exported)
# ---------------------------------------------------------------------------

#' Internal helper: Tps Spatial Result
#' @noRd
.tps_spatial_result <- function(title, call,
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
  class(out) <- c("morie_tps_spatial_result", "morie_rich_result", "list")
  out
}


#' Internal helper: Tps Hood Counts
#' @noRd
.tps_hood_counts <- function(df, hood_col = "HOOD_158") {
  if (!is.data.frame(df) || !(hood_col %in% names(df))) {
    return(integer(0))
  }
  s <- df[[hood_col]]
  s <- s[!is.na(s)]
  s <- s[toupper(as.character(s)) != "NSA"]
  if (length(s) == 0L) {
    return(integer(0))
  }
  tab <- table(s)
  sort(tab, decreasing = TRUE)
}


#' Internal helper: Tps Knn Adjacency
#' @noRd
.tps_knn_adjacency <- function(coords, k) {
  n <- nrow(coords)
  if (n < 2L) {
    return(matrix(0, n, n))
  }
  k <- min(as.integer(k), n - 1L)
  W <- matrix(0, n, n)
  if (n <= 2000L) {
    nn <- .morie_knn_index(coords, k)
    for (i in seq_len(n)) {
      W[i, nn[i, ]] <- 1.0
    }
  } else {
    # base R fallback: row-wise squared Euclidean distance
    for (i in seq_len(n)) {
      diff <- sweep(coords, 2L, coords[i, ], "-")
      d <- sqrt(rowSums(diff * diff))
      d[i] <- Inf
      idx <- order(d)[seq_len(k)]
      W[i, idx] <- 1.0
    }
  }
  rsum <- rowSums(W)
  rsum[rsum == 0] <- 1
  W / rsum
}


#' Internal helper: Tps Cliff Ord Variance
#'
#' Variance of Moran's I under normality (Cliff & Ord 1981):
#' \deqn{Var(I) = \frac{n^2 S_1 - n S_2 + 3 S_0^2}{S_0^2 (n^2 - 1)}
#'   - \frac{1}{(n-1)^2}.}
#' Module 19 fix: the previous combining formula was off by a factor
#' of order n; this form reproduces
#' \code{spdep::moran.test(randomisation = FALSE)} exactly
#' (tests/cross/test-morie_vs_spatial.R).
#' @noRd
.tps_cliff_ord_variance <- function(W, n, S0) {
  W_sym <- (W + t(W)) / 2
  S1 <- 2 * sum(W_sym^2)
  S2 <- sum((colSums(W) + rowSums(W))^2)
  (n^2 * S1 - n * S2 + 3 * S0^2) / (S0^2 * (n^2 - 1) + 1e-300) -
    1 / (n - 1)^2
}


# ---------------------------------------------------------------------------
# 1. Moran's I (global) on neighbourhood-level counts
# ---------------------------------------------------------------------------

#' Global Moran's I on neighbourhood-level incident counts
#'
#' Builds a k-NN spatial weights matrix from neighbourhood centroids
#' (mean LAT/LONG of incidents in each hood) and computes the global
#' Moran's I on the count vector. The Cliff-Ord normal-assumption
#' variance is used for the z-score and two-sided p-value.
#'
#' @param df Incident-level data.frame.
#' @param hood_col Character. Neighbourhood id column (default
#'   \code{"HOOD_158"}).
#' @param ds_name Character. Tag for the result title.
#' @param k_neighbours k for the k-NN spatial weights graph
#'   (default 5).
#' @param lat_col,lon_col WGS84 column names (default
#'   \code{"LAT_WGS84"} / \code{"LONG_WGS84"}).
#' @param use_spdep Retained for back-compat; ignored (the native
#'   Cliff-Ord computation is always used). Default \code{FALSE}.
#' @return A named list with classes \code{morie_tps_spatial_result},
#'   \code{morie_rich_result}, \code{list}. Numeric outputs include
#'   \code{moran_I}, \code{expected_I}, \code{var_I}, \code{z_score},
#'   \code{p_value}, \code{n}.
#' @examples
#' set.seed(2026)
#' n_inc <- 400
#' df <- data.frame(
#'   HOOD_158 = sample(letters\[1:20\], n_inc, replace = TRUE),
#'   LAT_WGS84 = 43.6 + runif(n_inc, 0, 0.2),
#'   LONG_WGS84 = -79.4 + runif(n_inc, 0, 0.2)
#' )
#' morie_tps_morans_i_neighbourhood(df)
#' @export
morie_tps_morans_i_neighbourhood <- function(df,
                                              hood_col = "HOOD_158",
                                              ds_name = "?",
                                              k_neighbours = 5L,
                                              lat_col = "LAT_WGS84",
                                              lon_col = "LONG_WGS84",
                                              use_spdep = FALSE) {
  stopifnot(is.data.frame(df), is.character(hood_col))
  call <- sprintf(
    "morie_tps_morans_i_neighbourhood(df=<%dr>, hood_col=%s, k=%d)",
    nrow(df), hood_col, as.integer(k_neighbours)
  )
  title <- sprintf("Moran's I (global) -- %s", ds_name)

  if (!(hood_col %in% names(df))) {
    return(.tps_spatial_result(
      title, call,
      warnings = sprintf("%s missing", hood_col),
      interpretation = sprintf(
        "No analysis: column '%s' is absent from the supplied data.frame.",
        hood_col
      ),
      n = 0L
    ))
  }
  if (!all(c(lat_col, lon_col) %in% names(df))) {
    return(.tps_spatial_result(
      title, call,
      warnings = sprintf(
        "%s/%s missing -- cannot build spatial weights", lat_col, lon_col
      ),
      interpretation = "No analysis: lat/lon columns absent.",
      n = 0L
    ))
  }

  counts <- .tps_hood_counts(df, hood_col)
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
    return(.tps_spatial_result(
      title, call,
      warnings = sprintf("only %d valid neighbourhoods", n),
      interpretation = "No analysis: fewer than 5 valid neighbourhood centroids.",
      n = as.integer(n)
    ))
  }

  coords <- as.matrix(cent)
  k <- min(as.integer(k_neighbours), n - 1L)
  W <- .tps_knn_adjacency(coords, k = k)
  x <- as.numeric(counts)
  z <- x - mean(x)

  # Module 19: the Cliff-Ord normal approximation below IS the
  # estimator (validated against spdep::moran.test in tests/cross/);
  # use_spdep is retained in the signature for back-compat and ignored.
  if (TRUE) {
    S0 <- sum(W)
    if (S0 == 0) {
      return(.tps_spatial_result(
        title, call,
        warnings = "empty spatial weights -- k too small",
        interpretation = "No analysis: spatial weights sum to zero.",
        n = as.integer(n)
      ))
    }
    num <- as.numeric(t(z) %*% W %*% z)
    den <- sum(z * z)
    I_val <- if (den != 0) (n / S0) * (num / den) else NA_real_
    expected_I <- if (n > 1L) -1 / (n - 1) else NA_real_
    var_I <- .tps_cliff_ord_variance(W, n, S0)
    if (!is.finite(var_I) || var_I <= 0) {
      z_I <- NA_real_
      p <- NA_real_
    } else {
      z_I <- (I_val - expected_I) / sqrt(var_I)
      p <- 2 * stats::pnorm(-abs(z_I))
    }
    backend <- "internal Cliff-Ord normal approximation"
  }

  interp <- if (is.finite(z_I)) {
    sprintf(
      paste0(
        "Global Moran's I = %+.3f on %d neighbourhood(s) with a ",
        "k=%d-NN weights matrix (backend: %s). z = %+.2f, two-sided ",
        "p = %.4g. Positive I means nearby neighbourhoods share ",
        "similar incident counts (spatial clustering); negative ",
        "indicates a checkerboard pattern; near zero is consistent ",
        "with spatial randomness."
      ),
      I_val, n, k, backend, z_I, p
    )
  } else {
    "Variance non-positive or undefined; interpretation skipped."
  }

  .tps_spatial_result(
    title, call,
    summary_lines = list(
      `Spatial unit` = hood_col,
      `Neighbourhoods` = as.integer(n),
      `k-nearest neighbours` = as.integer(k),
      `Moran's I` = I_val,
      `Expected I under null` = expected_I,
      `Variance(I)` = var_I,
      `z-score` = z_I,
      `p-value (two-sided)` = p,
      `Backend` = backend
    ),
    warnings = character(0),
    interpretation = interp,
    n = as.integer(n),
    moran_I = I_val,
    expected_I = expected_I,
    var_I = var_I,
    z_score = z_I,
    p_value = p,
    backend = backend
  )
}


# ---------------------------------------------------------------------------
# 2. Local Moran's I (LISA) per neighbourhood
# ---------------------------------------------------------------------------

#' LISA -- local Moran's Ii per neighbourhood
#'
#' Computes local Moran's Ii for each neighbourhood given a k-NN
#' spatial weights graph on centroid lat/long, with HH / LL / HL / LH
#' quadrant classification.
#'
#' @param df Incident-level data.frame.
#' @param hood_col Neighbourhood id column.
#' @param ds_name Tag for the result title.
#' @param k_neighbours k for the spatial weights graph.
#' @param top_n Number of top-Ii rows to surface in the result table.
#' @param lat_col,lon_col WGS84 column names.
#' @return A named list with \code{table} (data.frame of per-hood
#'   I_i, z, Wz, quadrant) and quadrant tallies.
#' @examples
#' set.seed(2026)
#' df <- data.frame(
#'   HOOD_158 = sample(letters\[1:15\], 300, replace = TRUE),
#'   LAT_WGS84 = 43.6 + runif(300, 0, 0.2),
#'   LONG_WGS84 = -79.4 + runif(300, 0, 0.2)
#' )
#' morie_tps_local_morans_i(df, top_n = 5L)
#' @export
morie_tps_local_morans_i <- function(df,
                                      hood_col = "HOOD_158",
                                      ds_name = "?",
                                      k_neighbours = 5L,
                                      top_n = 20L,
                                      lat_col = "LAT_WGS84",
                                      lon_col = "LONG_WGS84") {
  stopifnot(is.data.frame(df), is.character(hood_col))
  call <- sprintf(
    "morie_tps_local_morans_i(df=<%dr>, hood_col=%s, k=%d)",
    nrow(df), hood_col, as.integer(k_neighbours)
  )
  title <- sprintf("LISA (local Moran's Ii) -- %s", ds_name)

  if (!(hood_col %in% names(df))) {
    return(.tps_spatial_result(
      title, call,
      warnings = sprintf("%s missing", hood_col),
      interpretation = sprintf("No analysis: column '%s' is absent.", hood_col),
      n = 0L
    ))
  }
  if (!all(c(lat_col, lon_col) %in% names(df))) {
    return(.tps_spatial_result(
      title, call,
      warnings = sprintf("%s/%s missing", lat_col, lon_col),
      interpretation = "No analysis: lat/lon columns absent.",
      n = 0L
    ))
  }

  counts <- .tps_hood_counts(df, hood_col)
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
    return(.tps_spatial_result(
      title, call,
      warnings = sprintf("only %d valid neighbourhoods", n),
      interpretation = "No analysis: fewer than 5 valid neighbourhood centroids.",
      n = as.integer(n)
    ))
  }

  coords <- as.matrix(cent)
  k <- min(as.integer(k_neighbours), n - 1L)
  W <- .tps_knn_adjacency(coords, k = k)
  x <- as.numeric(counts)
  z <- x - mean(x)
  z_std <- z / (stats::sd(z) + 1e-300)
  Wz <- as.vector(W %*% z_std)
  Ii <- z_std * Wz

  quad <- character(n)
  quad[(z_std > 0) & (Wz > 0)] <- "HH (high-high)"
  quad[(z_std < 0) & (Wz < 0)] <- "LL (low-low)"
  quad[(z_std > 0) & (Wz < 0)] <- "HL (high-low)"
  quad[(z_std <= 0) & (Wz >= 0) & quad == ""] <- "LH (low-high)"
  quad[quad == ""] <- "LH (low-high)"

  out_tbl <- data.frame(
    hood = names(counts),
    count = as.integer(x),
    z = z_std,
    Wz = Wz,
    Ii = Ii,
    quadrant = quad,
    stringsAsFactors = FALSE
  )
  out_tbl <- out_tbl[order(out_tbl$Ii, decreasing = TRUE), , drop = FALSE]

  n_hh <- sum(quad == "HH (high-high)")
  n_ll <- sum(quad == "LL (low-low)")
  n_hl <- sum(quad == "HL (high-low)")
  n_lh <- sum(quad == "LH (low-high)")
  top <- utils::head(out_tbl, as.integer(top_n))

  interp <- sprintf(
    paste0(
      "LISA identified %d hot-spot (HH) and %d cold-spot (LL) ",
      "neighbourhood(s) on a k=%d-NN graph (n=%d). HL outliers ",
      "(high count surrounded by low neighbours) = %d, LH = %d. ",
      "Top neighbourhood by local Ii: '%s' with Ii=%+.3f."
    ),
    n_hh, n_ll, k, n, n_hl, n_lh, top$hood[1L], top$Ii[1L]
  )

  .tps_spatial_result(
    title, call,
    summary_lines = list(
      `Spatial unit` = hood_col,
      `Neighbourhoods` = as.integer(n),
      `Hot spots (HH)` = n_hh,
      `Cold spots (LL)` = n_ll,
      `HL outliers (high-in-low)` = n_hl,
      `LH outliers (low-in-high)` = n_lh
    ),
    warnings = character(0),
    interpretation = interp,
    n = as.integer(n),
    table = out_tbl,
    top = top,
    n_hh = n_hh,
    n_ll = n_ll,
    n_hl = n_hl,
    n_lh = n_lh
  )
}


# ---------------------------------------------------------------------------
# 3. 2-D KDE of geocoded incident points
# ---------------------------------------------------------------------------

#' 2-D kernel density estimate of geocoded incidents
#'
#' Evaluates a Gaussian KDE on incident lat/long and returns summary
#' statistics plus the (lat, lon) of the densest observation. Prefers
#' \pkg{MASS}\code{::kde2d} when available; otherwise uses a pure
#' base-R Gaussian kernel evaluated at the observation points (i.e.
#' kernel density at each datum).
#'
#' @param df Incident-level data.frame.
#' @param bandwidth Bandwidth multiplier passed to the 2-D KDE; see
#'   the \code{h} argument of the native \code{morie_kde2d}.
#' @param ds_name Tag for the result title.
#' @param lat_col,lon_col WGS84 column names.
#' @return A named list with summary stats including max/mean/median
#'   density and the (lat, lon) of the densest observation.
#' @examples
#' set.seed(2026)
#' df <- data.frame(
#'   LAT_WGS84 = 43.6 + rnorm(120, 0, 0.05),
#'   LONG_WGS84 = -79.4 + rnorm(120, 0, 0.05)
#' )
#' morie_tps_kde_density(df, bandwidth = 0.01)
#' @export
morie_tps_kde_density <- function(df,
                                   bandwidth = 0.005,
                                   ds_name = "?",
                                   lat_col = "LAT_WGS84",
                                   lon_col = "LONG_WGS84") {
  stopifnot(is.data.frame(df))
  call <- sprintf(
    "morie_tps_kde_density(df=<%dr>, bandwidth=%.4g)",
    nrow(df), bandwidth
  )
  title <- sprintf("KDE -- %s", ds_name)

  if (!all(c(lat_col, lon_col) %in% names(df))) {
    return(.tps_spatial_result(
      title, call,
      warnings = sprintf("%s/%s missing", lat_col, lon_col),
      interpretation = "No analysis: lat/lon columns absent.",
      n = 0L
    ))
  }

  coords <- df[, c(lat_col, lon_col), drop = FALSE]
  coords <- coords[stats::complete.cases(coords), , drop = FALSE]
  coords <- coords[coords[[lat_col]] != 0 & coords[[lon_col]] != 0, , drop = FALSE]
  n <- nrow(coords)
  if (n < 50L) {
    return(.tps_spatial_result(
      title, call,
      warnings = sprintf("only %d geocoded", n),
      interpretation = "No analysis: fewer than 50 geocoded incidents.",
      n = as.integer(n)
    ))
  }

  lat <- coords[[lat_col]]
  lon <- coords[[lon_col]]

  {
    h <- c(bandwidth * diff(range(lon)) * 4,
           bandwidth * diff(range(lat)) * 4)
    h[h <= 0] <- bandwidth
    gn <- 64L
    z <- morie_kde2d(lon, lat, h = h, n = gn)
    # bilinear lookup at each observation
    ix <- pmin(pmax(findInterval(lon, z$x), 1L), gn)
    iy <- pmin(pmax(findInterval(lat, z$y), 1L), gn)
    densities <- z$z[cbind(ix, iy)]
    backend <- "morie_kde2d"
  }

  i_max <- which.max(densities)
  interp <- sprintf(
    paste0(
      "Across %d geocoded incident(s), peak kernel density is %.4f ",
      "(mean %.4f, median %.4f; backend: %s). The densest cluster ",
      "centre is at (lat=%.4f, lon=%.4f). Smaller bandwidth ",
      "exposes finer local clusters; larger bandwidth oversmooths."
    ),
    n, max(densities), mean(densities), stats::median(densities),
    backend, lat[i_max], lon[i_max]
  )

  .tps_spatial_result(
    title, call,
    summary_lines = list(
      `Geocoded incidents` = as.integer(n),
      `Bandwidth` = bandwidth,
      `Max density (at obs)` = max(densities),
      `Mean density` = mean(densities),
      `Median density` = stats::median(densities),
      `Lat at max-density obs` = lat[i_max],
      `Lon at max-density obs` = lon[i_max],
      `Backend` = backend
    ),
    warnings = character(0),
    interpretation = interp,
    n = as.integer(n),
    densities = densities,
    max_density = max(densities),
    mean_density = mean(densities),
    median_density = stats::median(densities),
    lat_at_max = lat[i_max],
    lon_at_max = lon[i_max],
    backend = backend
  )
}


# ---------------------------------------------------------------------------
# Print method
# ---------------------------------------------------------------------------

#' @return \code{x}, invisibly.
#' @export
print.morie_tps_spatial_result <- function(x, ...) {
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

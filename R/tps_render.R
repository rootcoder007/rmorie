# SPDX-License-Identifier: AGPL-3.0-or-later
#' Toronto Police Service map rendering (R-side)
#'
#' R-side port of \code{morie.tps_render}.  Carries the two design
#' rules from the Python module (per the author, 2026-05-07):
#'
#' \enumerate{
#'   \item No floating neighbourhood text labels on the map -- hot-spot
#'     identification is delivered via the \code{morie_tps_*} result
#'     tables, not via on-canvas text.
#'   \item Map is rotated approximately 17.5 degrees clockwise in
#'     projected space so Lake Ontario's shoreline sits level
#'     horizontally -- matching the Sigar Li 2022 "Hotspot Policing for
#'     the City of Toronto" poster aesthetic and the Hohl 2024 ALMI
#'     homicide-cluster map.
#' }
#'
#' Plotting back-ends are gated behind \pkg{ggplot2}; without it, the
#' callables fall back to base \code{plot()}.  Heavy panels
#' (kernel-density, LISA, Getis-Ord, Kulldorff scan) that depend on
#' the Python TPS spatial modules are intentionally not ported here:
#' the projection + base choropleth / point-pattern primitives below
#' are enough for the empirical paper's figures.
#'
#' Functions
#' ---------
#'
#' \itemize{
#'   \item \code{\link{morie_tps_project_xy}}: degrees -> rotated planar km.
#'   \item \code{\link{morie_tps_pretty_label}}: \code{ASSAULT_RATE_2024} ->
#'     \code{"Assault rate * 2024"}.
#'   \item \code{\link{morie_tps_district_for_centroid}}: lat/lon ->
#'     pre-1998 borough name.
#'   \item \code{\link{morie_tps_render_choropleth}}: polygon choropleth
#'     (ggplot2 if available, else base R).
#'   \item \code{\link{morie_tps_render_points}}: point-pattern map
#'     (incident dots + optional DBSCAN colouring).
#'   \item \code{\link{morie_tps_render_yearly_grid}}: small-multiples
#'     by year.
#' }
#'
#' @name morie_tps_render
NULL


# ---------------------------------------------------------------------------
# Toronto rotation constants (mirror src/morie/tps_render.py)
# ---------------------------------------------------------------------------

.MORIE_TPS_LAT_C        <- 43.7000
.MORIE_TPS_LON_C        <- -79.4000
.MORIE_TPS_ROT_DEG_CW   <- 17.5
.MORIE_TPS_KM_PER_DEG_LAT <- 110.574
.MORIE_TPS_KM_PER_DEG_LON <- 111.320 * cos(.MORIE_TPS_LAT_C * pi / 180)


# Pre-1998 community-council districts.  Roughly bbox-defined; close
# to the OFFICIAL boundaries within ~1 km.  Order matters for legend
# layout.
.MORIE_TPS_DISTRICTS <- list(
  list(name = "Etobicoke",   colour = "#cfe7c8",
       box = list(lon_max = -79.475)),
  list(name = "North York",  colour = "#fff2b3",
       box = list(lat_min = 43.745, lon_min = -79.475, lon_max = -79.275)),
  list(name = "Scarborough", colour = "#cfe2f3",
       box = list(lon_min = -79.275)),
  list(name = "York",        colour = "#e8b6cf",
       box = list(lat_max = 43.745, lon_min = -79.475, lon_max = -79.430)),
  list(name = "Old Toronto", colour = "#f7c69b",
       box = list(lat_max = 43.745, lon_min = -79.430, lon_max = -79.310)),
  list(name = "East York",   colour = "#d2c2a0",
       box = list(lat_max = 43.745, lat_min = 43.685,
                  lon_min = -79.355, lon_max = -79.300))
)


# Per-category palette (matches the Python tps_render CATEGORY_CMAP).
# Two named values per category: sequential for rate panels, divergent
# for cluster / LISA panels.  Names are descriptive; R-side renderers
# accept any ColorBrewer name (e.g. via scale_fill_distiller in ggplot2)
# or a colorRampPalette fallback.
.MORIE_TPS_CATEGORY_CMAP <- list(
  Assault                          = c(seq = "YlOrRd",  div = "RdBu"),
  AutoTheft                        = c(seq = "YlGnBu",  div = "RdBu"),
  BicycleTheft                     = c(seq = "BuGn",    div = "PuOr"),
  BreakandEnter                    = c(seq = "Greens",  div = "RdBu"),
  Homicides                        = c(seq = "YlOrBr",  div = "RdBu"),
  Robbery                          = c(seq = "Purples", div = "PiYG"),
  ShootingAndFirearmDiscarges      = c(seq = "Reds",    div = "RdBu"),
  TheftFromMovingVehicle           = c(seq = "PuBu",    div = "RdBu"),
  TheftOver                        = c(seq = "OrRd",    div = "RdBu"),
  HateCrimes                       = c(seq = "RdPu",    div = "RdBu"),
  IntimatePartnerAndFamilyViolence = c(seq = "RdPu",    div = "RdBu"),
  CommunitySafetyIndicators        = c(seq = "Magma",   div = "RdBu"),
  NeighbourhoodCrimeRates          = c(seq = "YlOrRd",  div = "RdBu")
)


# ---------------------------------------------------------------------------
# 1. Centroid -> district lookup
# ---------------------------------------------------------------------------

#' Return the pre-1998 borough name for a (lat, lon) centroid
#'
#' Toronto amalgamated in 1998; the six former municipalities (Etobicoke,
#' North York, Scarborough, York, Old Toronto, East York) are still in
#' common use for district-level reporting and are bbox-defined here.
#'
#' @param lat Numeric latitude (WGS84).
#' @param lon Numeric longitude (WGS84).
#' @return A character scalar.  Defaults to \code{"Old Toronto"} when no
#'   bbox matches.
#' @export
morie_tps_district_for_centroid <- function(lat, lon) {
  for (d in .MORIE_TPS_DISTRICTS) {
    ok <- TRUE
    box <- d$box
    if (!is.null(box$lon_max) && lon > box$lon_max) ok <- FALSE
    if (!is.null(box$lon_min) && lon < box$lon_min) ok <- FALSE
    if (!is.null(box$lat_max) && lat > box$lat_max) ok <- FALSE
    if (!is.null(box$lat_min) && lat < box$lat_min) ok <- FALSE
    if (ok) return(d$name)
  }
  "Old Toronto"
}


# ---------------------------------------------------------------------------
# 2. Pretty label
# ---------------------------------------------------------------------------

#' Convert a SQL-style column name to a prose label
#'
#' \code{ASSAULT_RATE_2024} -> \code{"Assault rate * 2024"}.  Strips
#' underscores and casing so plot titles look like prose.
#'
#' @param s Character scalar (column name).
#' @return Character scalar (display label).
#' @export
morie_tps_pretty_label <- function(s) {
  parts <- strsplit(s, "_", fixed = TRUE)[[1]]
  out <- vapply(parts, function(p) {
    if (grepl("^[0-9]{4}$", p)) p else tolower(p)
  }, character(1), USE.NAMES = FALSE)
  if (length(out) == 0L) return(s)
  out[1] <- paste0(toupper(substr(out[1], 1, 1)),
                   substr(out[1], 2, nchar(out[1])))
  last <- out[length(out)]
  if (length(out) > 1L && grepl("^[0-9]{4}$", last)) {
    head_str <- trimws(paste(out[-length(out)], collapse = " "))
    paste0(head_str, " * ", last)
  } else {
    paste(out, collapse = " ")
  }
}


# ---------------------------------------------------------------------------
# 3. Projection
# ---------------------------------------------------------------------------

#' Project (lat, lon) to rotated planar kilometres
#'
#' Equirectangular projection centred at (lat_c, lon_c), then rotated
#' \code{rot_deg_cw} degrees clockwise (default 17.5).  Returns
#' kilometres east-of-centre (after rotation) and kilometres
#' north-of-centre (after rotation).
#'
#' Clockwise convention: positive \code{rot_deg_cw} rotates the map
#' so a line that previously sloped up-right slopes less (or
#' down-right).
#'
#' @param lat Numeric vector of latitudes (WGS84 degrees).
#' @param lon Numeric vector of longitudes (WGS84 degrees).
#' @param rot_deg_cw Clockwise rotation in degrees (default 17.5).
#' @param lat_c,lon_c Centre-point of the projection (default downtown
#'   Toronto: 43.70 N, 79.40 W).
#' @return A named list with numeric vectors \code{x} (km east of centre)
#'   and \code{y} (km north of centre).
#' @export
morie_tps_project_xy <- function(lat, lon,
                                  rot_deg_cw = .MORIE_TPS_ROT_DEG_CW,
                                  lat_c = .MORIE_TPS_LAT_C,
                                  lon_c = .MORIE_TPS_LON_C) {
  lat <- as.numeric(lat)
  lon <- as.numeric(lon)
  stopifnot(length(lat) == length(lon))
  km_lon <- 111.320 * cos(lat_c * pi / 180)
  x <- (lon - lon_c) * km_lon
  y <- (lat - lat_c) * .MORIE_TPS_KM_PER_DEG_LAT
  a <- rot_deg_cw * pi / 180
  cs <- cos(a)
  sn <- sin(a)
  list(
    x = x * cs + y * sn,
    y = -x * sn + y * cs
  )
}


# ---------------------------------------------------------------------------
# Internal: ggplot2-or-base dispatcher
# ---------------------------------------------------------------------------

.tps_has_ggplot2 <- function() {
  requireNamespace("ggplot2", quietly = TRUE)
}

.tps_save_plot <- function(p_or_recordedplot, outfile, fig_w, fig_h,
                            use_gg) {
  outfile <- path.expand(outfile)
  dir.create(dirname(outfile), showWarnings = FALSE, recursive = TRUE)
  ext <- tolower(tools::file_ext(outfile))
  if (use_gg) {
    ggplot2::ggsave(outfile, plot = p_or_recordedplot,
                    width = fig_w, height = fig_h, dpi = 140)
  } else {
    # base-R fallback: re-draw into a device of the right type
    dev_fn <- switch(ext,
      png  = grDevices::png,
      jpg  = grDevices::jpeg,
      jpeg = grDevices::jpeg,
      pdf  = grDevices::pdf,
      svg  = grDevices::svg,
      grDevices::png
    )
    if (identical(dev_fn, grDevices::pdf) ||
        identical(dev_fn, grDevices::svg)) {
      dev_fn(outfile, width = fig_w, height = fig_h)
    } else {
      dev_fn(outfile, width = fig_w * 96, height = fig_h * 96)
    }
    grDevices::replayPlot(p_or_recordedplot)
    grDevices::dev.off()
  }
  outfile
}


# ---------------------------------------------------------------------------
# 4. Choropleth
# ---------------------------------------------------------------------------

#' Polygon choropleth map for Toronto (158 wards)
#'
#' Renders a sequential-colour choropleth from a polygon data.frame
#' carrying one row per neighbourhood with a list-column of WGS84
#' (lon, lat) rings (\code{geometry}) and a numeric \code{rate_col}.
#' This signature deliberately matches what
#' \code{rmorie::morie_fetch("https://.../NeighbourhoodCrimeRates...",
#' format = "geojson")} returns once unrolled.
#'
#' When \pkg{ggplot2} is available the render uses
#' \code{geom_polygon} + \code{scale_fill_distiller}; otherwise the
#' base R \code{polygon()} primitive is used.
#'
#' @param polys A \code{data.frame} or \code{tibble} with columns
#'   \code{geometry} (list-column of N x 2 lon/lat matrices) and the
#'   metric column named in \code{rate_col}.  An optional \code{HOOD_ID}
#'   or \code{AREA_ID} column drives the show-IDs labels.
#' @param rate_col Name of the metric column.  Default \code{"ASSAULT_RATE_2024"}.
#' @param title Plot title; defaults to a Hohl-2024-style auto-label.
#' @param cmap Sequential colour palette name (default \code{"YlOrRd"}).
#' @param outfile Path to write the image (\code{.png}, \code{.pdf},
#'   \code{.svg}).  When \code{NULL}, the function returns the
#'   plot object without writing.
#' @param fig_w,fig_h Figure size in inches.
#' @param show_ids When \code{TRUE} (default), draw small numeric
#'   polygon-ID labels at each ward centroid.
#' @param border_color Polygon edge colour.
#' @param border_lw Polygon edge linewidth.
#' @return A \code{ggplot} object (when ggplot2 is loaded) or
#'   \code{invisible(NULL)} for the base-R fallback; the file path is
#'   returned invisibly when \code{outfile} is supplied.
#' @export
morie_tps_render_choropleth <- function(polys,
                                          rate_col = "ASSAULT_RATE_2024",
                                          title = NULL,
                                          cmap = "YlOrRd",
                                          outfile = NULL,
                                          fig_w = 12, fig_h = 7,
                                          show_ids = TRUE,
                                          border_color = "#1a1a1a",
                                          border_lw = 0.7) {
  stopifnot(is.data.frame(polys), "geometry" %in% names(polys),
            rate_col %in% names(polys))
  use_gg <- .tps_has_ggplot2()
  if (is.null(title)) {
    title <- sprintf("Toronto * %s * 158 wards",
                     morie_tps_pretty_label(rate_col))
  }

  # Flatten geometry list-column into a long projected data.frame for
  # ggplot2; for base R we keep a per-ring list of (x, y) matrices.
  long_df <- list()
  base_rings <- list()
  for (i in seq_len(nrow(polys))) {
    geom <- polys$geometry[[i]]
    if (is.null(geom)) next
    rings <- if (is.list(geom[[1]][[1]])) geom else list(geom)
    for (ri in seq_along(rings)) {
      ring <- rings[[ri]]
      m <- do.call(rbind, lapply(ring, function(p) c(p[[1]], p[[2]])))
      if (is.null(m) || nrow(m) < 3L) next
      lons <- m[, 1]
      lats <- m[, 2]
      if (min(lats) < 43.55 || max(lats) > 43.90 ||
          min(lons) < -79.65 || max(lons) > -79.10) next
      pp <- morie_tps_project_xy(lats, lons)
      ring_id <- paste0(i, "_", ri)
      base_rings[[ring_id]] <- list(
        x = pp$x, y = pp$y,
        val = suppressWarnings(as.numeric(polys[[rate_col]][i])),
        id = polys$HOOD_ID[i] %||% polys$AREA_ID[i] %||% NA
      )
      long_df[[ring_id]] <- data.frame(
        x = pp$x, y = pp$y,
        ring = ring_id, poly = i,
        val = suppressWarnings(as.numeric(polys[[rate_col]][i])),
        stringsAsFactors = FALSE
      )
    }
  }

  if (use_gg) {
    dfl <- do.call(rbind, long_df)
    p <- ggplot2::ggplot(dfl,
                          ggplot2::aes(x = .data$x, y = .data$y,
                                       group = .data$ring,
                                       fill = .data$val)) +
      ggplot2::geom_polygon(colour = border_color, linewidth = border_lw) +
      ggplot2::scale_fill_distiller(palette = cmap, direction = 1,
                                    name = morie_tps_pretty_label(rate_col)) +
      ggplot2::coord_equal() +
      ggplot2::labs(title = title,
                    x = "east of centre (km)",
                    y = "north of centre (km)") +
      ggplot2::theme_minimal()
    if (show_ids) {
      centroids <- do.call(rbind, lapply(base_rings, function(r) {
        if (is.null(r$id) || is.na(r$id)) return(NULL)
        data.frame(x = mean(r$x), y = mean(r$y),
                   id = as.integer(r$id))
      }))
      if (!is.null(centroids) && nrow(centroids) > 0L) {
        p <- p + ggplot2::geom_text(
          data = centroids,
          ggplot2::aes(x = .data$x, y = .data$y, label = .data$id),
          inherit.aes = FALSE,
          size = 2.0, fontface = "bold", colour = "#0a0a0a"
        )
      }
    }
    if (!is.null(outfile)) {
      .tps_save_plot(p, outfile, fig_w, fig_h, use_gg = TRUE)
      return(invisible(outfile))
    }
    return(p)
  }

  # Base-R fallback.
  ramp <- grDevices::colorRampPalette(c("#fff7bc", "#fec44f", "#d95f0e"))
  vals <- vapply(base_rings, `[[`, numeric(1), "val")
  finite <- vals[is.finite(vals)]
  vmin <- if (length(finite)) min(finite) else 0
  vmax <- if (length(finite)) max(finite) else 1
  colours <- ramp(64)
  xrng <- range(unlist(lapply(base_rings, `[[`, "x")), na.rm = TRUE)
  yrng <- range(unlist(lapply(base_rings, `[[`, "y")), na.rm = TRUE)
  graphics::plot.new()
  graphics::plot.window(xlim = xrng, ylim = yrng, asp = 1)
  for (r in base_rings) {
    col <- if (is.finite(r$val)) {
      colours[1 + floor(63 * (r$val - vmin) / max(vmax - vmin, 1e-9))]
    } else "#eaeaea"
    graphics::polygon(r$x, r$y, col = col,
                      border = border_color, lwd = border_lw)
    if (show_ids && !is.null(r$id) && !is.na(r$id)) {
      graphics::text(mean(r$x), mean(r$y),
                     labels = as.integer(r$id),
                     cex = 0.5, font = 2)
    }
  }
  graphics::title(main = title,
                  xlab = "east of centre (km)",
                  ylab = "north of centre (km)")
  graphics::axis(1)
  graphics::axis(2)
  rec <- grDevices::recordPlot()
  if (!is.null(outfile)) {
    .tps_save_plot(rec, outfile, fig_w, fig_h, use_gg = FALSE)
    return(invisible(outfile))
  }
  invisible(NULL)
}


# ---------------------------------------------------------------------------
# 5. Point-pattern + optional DBSCAN colouring
# ---------------------------------------------------------------------------

#' Render a TPS point-pattern map (incident dots, optional DBSCAN)
#'
#' Projects (LAT_WGS84, LONG_WGS84) to the rotated Toronto canvas and
#' draws one dot per incident.  When \code{eps_km} and \code{min_samples}
#' are supplied AND the \pkg{dbscan} package is installed, points are
#' coloured by DBSCAN cluster label.
#'
#' @param df A TPS data.frame with columns \code{LAT_WGS84} and
#'   \code{LONG_WGS84}.
#' @param category Optional category label used in the title.
#' @param eps_km DBSCAN neighbourhood radius in km.  When \code{NULL}
#'   no clustering is run.
#' @param min_samples DBSCAN minimum cluster size.
#' @param outfile Path to write the image, or \code{NULL} to return
#'   the plot.
#' @param show_top Cap on how many clusters appear in the legend.
#' @param fig_w,fig_h Figure size.
#' @return A \code{ggplot} (when ggplot2 is available) or
#'   \code{invisible(NULL)} for the base-R path.
#' @export
morie_tps_render_points <- function(df,
                                      category = "Assault",
                                      eps_km = NULL,
                                      min_samples = 20L,
                                      outfile = NULL,
                                      show_top = 12L,
                                      fig_w = 12, fig_h = 7.5) {
  stopifnot(is.data.frame(df),
            all(c("LAT_WGS84", "LONG_WGS84") %in% names(df)))
  use_gg <- .tps_has_ggplot2()

  lat <- suppressWarnings(as.numeric(df$LAT_WGS84))
  lon <- suppressWarnings(as.numeric(df$LONG_WGS84))
  keep <- is.finite(lat) & is.finite(lon) &
          lat >= 43.55 & lat <= 43.90 &
          lon >= -79.65 & lon <= -79.10
  lat <- lat[keep]
  lon <- lon[keep]
  if (length(lat) == 0L) {
    stop(sprintf("%s: no in-bbox LAT/LONG rows", category))
  }
  pp <- morie_tps_project_xy(lat, lon)
  xk <- pp$x
  yk <- pp$y

  labels <- rep(-1L, length(xk))
  n_clusters <- 0L
  if (!is.null(eps_km) && requireNamespace("dbscan", quietly = TRUE)) {
    fit <- dbscan::dbscan(cbind(xk, yk),
                          eps = eps_km, minPts = min_samples)
    labels <- as.integer(fit$cluster) - 1L  # match sklearn -1==noise
    labels[labels == -1L] <- -1L
    n_clusters <- length(unique(labels[labels >= 0L]))
  }
  noise_n <- sum(labels == -1L)

  title <- if (!is.null(eps_km)) {
    sprintf("Toronto %s -- DBSCAN (eps=%gkm, min=%d) -- %d clusters, %s noise",
            category, eps_km, min_samples, n_clusters,
            format(noise_n, big.mark = ","))
  } else {
    sprintf("Toronto %s -- incident point pattern (n=%s)",
            category, format(length(xk), big.mark = ","))
  }

  if (use_gg) {
    df_plot <- data.frame(x = xk, y = yk,
                          cluster = factor(labels))
    p <- ggplot2::ggplot(df_plot,
                          ggplot2::aes(x = .data$x, y = .data$y,
                                       colour = .data$cluster)) +
      ggplot2::geom_point(size = 0.6, alpha = 0.6) +
      ggplot2::coord_equal() +
      ggplot2::labs(title = title,
                    x = "east of centre (km)",
                    y = "north of centre (km)") +
      ggplot2::theme_minimal() +
      ggplot2::theme(legend.position = "bottom")
    if (!is.null(outfile)) {
      .tps_save_plot(p, outfile, fig_w, fig_h, use_gg = TRUE)
      return(invisible(outfile))
    }
    return(p)
  }

  # Base-R fallback
  graphics::plot.new()
  graphics::plot.window(xlim = range(xk), ylim = range(yk), asp = 1)
  if (any(labels == -1L)) {
    graphics::points(xk[labels == -1L], yk[labels == -1L],
                      pch = 16, cex = 0.3, col = "#888")
  }
  cluster_ids <- sort(unique(labels[labels >= 0L]))
  if (length(cluster_ids) > 0L) {
    palette <- grDevices::hcl.colors(length(cluster_ids), "viridis")
    for (i in seq_along(cluster_ids)) {
      m <- labels == cluster_ids[i]
      graphics::points(xk[m], yk[m], pch = 16, cex = 0.45,
                        col = palette[i])
    }
  }
  graphics::title(main = title,
                  xlab = "east of centre (km)",
                  ylab = "north of centre (km)")
  graphics::axis(1)
  graphics::axis(2)
  rec <- grDevices::recordPlot()
  if (!is.null(outfile)) {
    .tps_save_plot(rec, outfile, fig_w, fig_h, use_gg = FALSE)
    return(invisible(outfile))
  }
  invisible(NULL)
}


# ---------------------------------------------------------------------------
# 6. Yearly small-multiples
# ---------------------------------------------------------------------------

#' Small-multiples panel of per-year TPS choropleths
#'
#' Walks \code{polys} once and renders one ggplot facet per year for
#' columns named \code{<prefix>_<year>}.
#'
#' @param polys Polygon data.frame (see
#'   \code{\link{morie_tps_render_choropleth}}).
#' @param prefix Column-name prefix (default \code{"ASSAULT_RATE"}).
#' @param years Integer vector of years (default 2014:2024).
#' @param cmap Sequential palette name (default \code{"Reds"}).
#' @param outfile Optional output path.
#' @param ncols Number of facet columns.
#' @return A \code{ggplot} (when ggplot2 is loaded) or
#'   \code{invisible(NULL)} for the base-R fallback.
#' @export
morie_tps_render_yearly_grid <- function(polys,
                                           prefix = "ASSAULT_RATE",
                                           years = 2014:2024,
                                           cmap = "Reds",
                                           outfile = NULL,
                                           ncols = 4L) {
  stopifnot(is.data.frame(polys), "geometry" %in% names(polys))
  use_gg <- .tps_has_ggplot2()
  cols <- paste0(prefix, "_", years)
  cols <- cols[cols %in% names(polys)]
  if (length(cols) == 0L) {
    stop(sprintf("no %s_<year> columns found", prefix))
  }
  if (!use_gg) {
    stop("ggplot2 is required for morie_tps_render_yearly_grid(); ",
         "install.packages('ggplot2') first.")
  }
  rows_long <- list()
  for (i in seq_len(nrow(polys))) {
    geom <- polys$geometry[[i]]
    if (is.null(geom)) next
    rings <- if (is.list(geom[[1]][[1]])) geom else list(geom)
    for (ri in seq_along(rings)) {
      ring <- rings[[ri]]
      m <- do.call(rbind, lapply(ring, function(p) c(p[[1]], p[[2]])))
      if (is.null(m) || nrow(m) < 3L) next
      lons <- m[, 1]
      lats <- m[, 2]
      pp <- morie_tps_project_xy(lats, lons)
      ring_id <- paste0(i, "_", ri)
      for (col in cols) {
        v <- suppressWarnings(as.numeric(polys[[col]][i]))
        year <- as.integer(sub(paste0(prefix, "_"), "", col))
        rows_long[[length(rows_long) + 1L]] <- data.frame(
          x = pp$x, y = pp$y,
          ring = ring_id, year = year, val = v,
          stringsAsFactors = FALSE
        )
      }
    }
  }
  dfl <- do.call(rbind, rows_long)
  p <- ggplot2::ggplot(dfl,
                        ggplot2::aes(x = .data$x, y = .data$y,
                                     group = .data$ring,
                                     fill = .data$val)) +
    ggplot2::geom_polygon(colour = "white", linewidth = 0.1) +
    ggplot2::scale_fill_distiller(palette = cmap, direction = 1,
                                  name = prefix) +
    ggplot2::coord_equal() +
    ggplot2::facet_wrap(~ .data$year, ncol = ncols) +
    ggplot2::labs(title = sprintf("Toronto %s * yearly small-multiples",
                                  prefix)) +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.title = ggplot2::element_blank(),
                   axis.text  = ggplot2::element_blank(),
                   axis.ticks = ggplot2::element_blank())
  if (!is.null(outfile)) {
    .tps_save_plot(p, outfile,
                   3.5 * ncols, 2.4 * ceiling(length(cols) / ncols),
                   use_gg = TRUE)
    return(invisible(outfile))
  }
  p
}


# `%||%` helper, scoped here to avoid pulling rlang.


# --- APPENDED 2026-05-22 -----------------------------------------------------
# Additional TPS rendering primitives: 4-panel quad composite, DBSCAN
# cluster figure, district proportional-symbol map, SaTScan-style panel,
# plus two internal helpers (compass + scalebar).  All callables prefer
# ggplot2 when available and fall back to base graphics; the SaTScan and
# proportional-symbol panels stub a `stop("NotYetPorted")` body for the
# detailed Kulldorff-likelihood overlays that depend on the Python
# tps_satscan module.
# ----------------------------------------------------------------------------

# Internal: draw a north-arrow compass in plot coordinates.
.tps_draw_compass <- function(x, y, size = 1.5, use_gg = FALSE) {
  if (use_gg && .tps_has_ggplot2()) {
    arrow_df <- data.frame(
      x  = c(x, x),
      y  = c(y - size / 2, y + size / 2),
      xend = c(x, x),
      yend = c(y + size / 2, y + size / 2)
    )
    list(
      ggplot2::annotate("segment",
                        x = x, xend = x,
                        y = y - size / 2, yend = y + size / 2,
                        arrow = grid::arrow(length = grid::unit(0.15, "cm")),
                        colour = "#222222", linewidth = 0.6),
      ggplot2::annotate("text", x = x, y = y + size / 2 + 0.3 * size,
                        label = "N", size = 3, colour = "#222222")
    )
  } else {
    graphics::arrows(x, y - size / 2, x, y + size / 2,
                     length = 0.08, lwd = 1.4, col = "#222222")
    graphics::text(x, y + size / 2 + 0.3 * size, "N",
                   cex = 0.8, col = "#222222")
    invisible(NULL)
  }
}

# Internal: draw a scalebar of `length_km` near (x, y) in km space.
.tps_draw_scalebar <- function(x, y, length_km = 5, use_gg = FALSE) {
  if (use_gg && .tps_has_ggplot2()) {
    list(
      ggplot2::annotate("segment", x = x, xend = x + length_km,
                        y = y, yend = y,
                        colour = "#222222", linewidth = 0.8),
      ggplot2::annotate("text", x = x + length_km / 2, y = y - 0.4,
                        label = sprintf("%g km", length_km),
                        size = 2.8, colour = "#222222")
    )
  } else {
    graphics::segments(x, y, x + length_km, y, lwd = 1.6, col = "#222222")
    graphics::text(x + length_km / 2, y - 0.4,
                   sprintf("%g km", length_km),
                   cex = 0.7, col = "#222222")
    invisible(NULL)
  }
}


#' Four-panel composite of TPS rendering primitives
#'
#' Lays out a 2x2 quad combining a choropleth, point pattern, yearly grid
#' summary, and (when available) a DBSCAN cluster panel.  Falls back to
#' base graphics with \code{par(mfrow = c(2, 2))} when ggplot2 is absent.
#'
#' @param data Named list with elements: ``polys`` (polygons frame),
#'   ``points`` (lat/lon points), ``count_col``, ``year_cols`` (character
#'   vector of column names like ``ASSAULT_RATE_2020:2024``).
#' @param outfile Optional output path; when NULL the rendered object is
#'   returned (ggplot or invisible NULL for base).
#' @param ... Forwarded to the underlying single-panel renderers.
#' @return A patchwork-or-list object (ggplot2 path) or invisible NULL.
#' @export
morie_tps_render_quad <- function(data, outfile = NULL, ...) {
  stopifnot(is.list(data))
  use_gg <- .tps_has_ggplot2() &&
            requireNamespace("patchwork", quietly = TRUE)
  panels <- list()
  if (!is.null(data$polys) && !is.null(data$count_col)) {
    panels$choropleth <- tryCatch(
      morie_tps_render_choropleth(data$polys, rate_col = data$count_col, ...),
      error = function(e) NULL)
  }
  if (!is.null(data$points)) {
    panels$points <- tryCatch(
      morie_tps_render_points(data$points, ...),
      error = function(e) NULL)
  }
  if (!is.null(data$points)) {
    panels$dbscan <- tryCatch(
      morie_tps_render_dbscan(data$points, eps_km = 0.5, min_samples = 8),
      error = function(e) NULL)
  }
  if (!is.null(data$polys) && !is.null(data$year_cols)) {
    panels$grid <- tryCatch(
      morie_tps_render_yearly_grid(data$polys, prefix = "ASSAULT_RATE", ...),
      error = function(e) NULL)
  }
  panels <- Filter(Negate(is.null), panels)
  if (use_gg && length(panels) > 0L &&
      all(vapply(panels, inherits, logical(1), what = "ggplot"))) {
    combined <- Reduce(`+`, panels) +
      patchwork::plot_layout(ncol = 2)
    if (!is.null(outfile)) {
      .tps_save_plot(combined, outfile, 11, 9, use_gg = TRUE)
      return(invisible(outfile))
    }
    return(combined)
  }
  # Base fallback: just print first available panel.
  if (length(panels) > 0L) return(panels[[1]])
  invisible(NULL)
}


#' DBSCAN cluster figure on TPS-projected points
#'
#' Runs DBSCAN on rotated-km coordinates and colours points by cluster
#' label, with noise rendered grey.  Requires the suggested \pkg{dbscan}
#' package; without it a base-graphics single-colour fallback is drawn.
#'
#' @param points_df data.frame with columns ``lat`` / ``lon`` (or ``LAT_WGS84``
#'   / ``LONG_WGS84``).
#' @param eps_km DBSCAN epsilon in kilometres.
#' @param min_samples Minimum samples per cluster.
#' @param outfile Optional output path.
#' @param ... Extra plotting args (size, alpha, palette).
#' @return ggplot object or invisible NULL.
#' @export
morie_tps_render_dbscan <- function(points_df, eps_km = 0.5,
                                    min_samples = 8L,
                                    outfile = NULL, ...) {
  lat <- points_df$lat %||% points_df$LAT_WGS84
  lon <- points_df$lon %||% points_df$LONG_WGS84
  if (is.null(lat) || is.null(lon))
    stop("points_df must have lat/lon (or LAT_WGS84/LONG_WGS84) columns")
  pp <- morie_tps_project_xy(lat, lon)

  labels <- rep(0L, length(lat))
  if (requireNamespace("dbscan", quietly = TRUE)) {
    cl <- dbscan::dbscan(cbind(pp$x, pp$y), eps = eps_km,
                         minPts = as.integer(min_samples))
    labels <- as.integer(cl$cluster)  # 0 = noise
  }
  dfp <- data.frame(x = pp$x, y = pp$y,
                    cluster = factor(labels))
  if (.tps_has_ggplot2()) {
    p <- ggplot2::ggplot(dfp,
                         ggplot2::aes(x = .data$x, y = .data$y,
                                      colour = .data$cluster)) +
      ggplot2::geom_point(size = 0.6, alpha = 0.7) +
      ggplot2::scale_colour_viridis_d(option = "turbo", na.value = "grey60") +
      ggplot2::coord_equal() +
      ggplot2::labs(title = sprintf("DBSCAN (eps=%g km, minPts=%d)",
                                    eps_km, min_samples),
                    colour = "cluster") +
      ggplot2::theme_minimal()
    p <- p + .tps_draw_compass(min(pp$x) + 1, max(pp$y) - 1,
                               size = 2, use_gg = TRUE)
    p <- p + .tps_draw_scalebar(min(pp$x) + 1, min(pp$y) + 1,
                                length_km = 5, use_gg = TRUE)
    if (!is.null(outfile)) {
      .tps_save_plot(p, outfile, 6, 6, use_gg = TRUE)
      return(invisible(outfile))
    }
    return(p)
  }
  graphics::plot(dfp$x, dfp$y, col = as.integer(dfp$cluster) + 1L,
                 pch = 19, cex = 0.4, asp = 1,
                 xlab = "km E", ylab = "km N",
                 main = sprintf("DBSCAN (eps=%g km, minPts=%d)",
                                eps_km, min_samples))
  invisible(NULL)
}


#' District-level proportional-symbol map
#'
#' Renders one centroid-anchored circle per polygon row, sized
#' proportionally to ``count_col``.  Useful for showing per-district
#' incident counts without colour-coding the polygons themselves.
#'
#' @param polys data.frame with one row per polygon, including a
#'   ``centroid_lat`` / ``centroid_lon`` (or ``LAT_WGS84`` / ``LONG_WGS84``)
#'   and the count column.
#' @param count_col Name of the numeric column.
#' @param max_radius_km Largest symbol radius in km.
#' @param outfile Optional output path.
#' @return ggplot object or invisible NULL.
#' @export
morie_tps_render_district_proportional <- function(polys, count_col,
                                                   max_radius_km = 3,
                                                   outfile = NULL) {
  stopifnot(count_col %in% names(polys))
  lat <- polys$centroid_lat %||% polys$LAT_WGS84
  lon <- polys$centroid_lon %||% polys$LONG_WGS84
  if (is.null(lat) || is.null(lon))
    stop("polys must expose centroid_lat/lon (or LAT/LONG_WGS84) columns")
  pp <- morie_tps_project_xy(lat, lon)
  counts <- suppressWarnings(as.numeric(polys[[count_col]]))
  counts[is.na(counts)] <- 0
  scale <- if (max(counts, na.rm = TRUE) > 0)
            max_radius_km / sqrt(max(counts, na.rm = TRUE)) else 0
  dfp <- data.frame(x = pp$x, y = pp$y,
                    radius = sqrt(counts) * scale,
                    count = counts)
  if (.tps_has_ggplot2()) {
    p <- ggplot2::ggplot(dfp,
                         ggplot2::aes(x = .data$x, y = .data$y,
                                      size = .data$count)) +
      ggplot2::geom_point(alpha = 0.55, colour = "#C0392B") +
      ggplot2::scale_size_area(max_size = max_radius_km * 4,
                                name = count_col) +
      ggplot2::coord_equal() +
      ggplot2::labs(title = sprintf("Proportional symbols * %s", count_col)) +
      ggplot2::theme_minimal()
    p <- p + .tps_draw_compass(min(pp$x) + 1, max(pp$y) - 1,
                               size = 2, use_gg = TRUE)
    if (!is.null(outfile)) {
      .tps_save_plot(p, outfile, 6, 6, use_gg = TRUE)
      return(invisible(outfile))
    }
    return(p)
  }
  graphics::symbols(pp$x, pp$y, circles = dfp$radius,
                    inches = FALSE, fg = "#C0392B", bg = "#C0392B40",
                    asp = 1, xlab = "km E", ylab = "km N",
                    main = sprintf("Proportional symbols * %s", count_col))
  invisible(NULL)
}


#' SaTScan-style spatial scan panel
#'
#' Renders Kulldorff-style circular candidate windows on the TPS canvas.
#' Currently a thin layer over centroids + radius circles; the full
#' likelihood-ratio overlay and significance ranking depend on the Python
#' ``morie.tps_satscan`` module and are stubbed.
#'
#' @param clusters data.frame with columns ``lat`` / ``lon`` / ``radius_km``
#'   and optionally ``llr`` (log-likelihood ratio) for shading.
#' @param outfile Optional output path.
#' @return ggplot object or invisible NULL.
#' @export
morie_tps_render_satscan_panel <- function(clusters, outfile = NULL) {
  if (!is.data.frame(clusters) ||
      !all(c("lat", "lon", "radius_km") %in% names(clusters))) {
    stop("clusters must be a data.frame with lat / lon / radius_km columns")
  }
  if (!"llr" %in% names(clusters)) {
    # Likelihood shading requires the Python SaTScan engine.
    clusters$llr <- NA_real_
  }
  # The full Kulldorff windowing + Monte-Carlo replication pipeline isn't
  # ported -- raise so callers don't silently get a degraded figure.
  if (any(is.na(clusters$llr))) {
    # Drop into a stub that documents the gap clearly.
    stop("NotYetPorted")
  }
  pp <- morie_tps_project_xy(clusters$lat, clusters$lon)
  dfp <- data.frame(x = pp$x, y = pp$y,
                    radius = clusters$radius_km,
                    llr = clusters$llr)
  if (.tps_has_ggplot2()) {
    p <- ggplot2::ggplot(dfp,
                         ggplot2::aes(x = .data$x, y = .data$y,
                                      size = .data$radius,
                                      colour = .data$llr)) +
      ggplot2::geom_point(alpha = 0.5) +
      ggplot2::scale_size_area(name = "radius (km)") +
      ggplot2::scale_colour_distiller(palette = "YlOrRd", direction = 1,
                                       name = "LLR") +
      ggplot2::coord_equal() +
      ggplot2::labs(title = "SaTScan candidate clusters") +
      ggplot2::theme_minimal()
    if (!is.null(outfile)) {
      .tps_save_plot(p, outfile, 6, 6, use_gg = TRUE)
      return(invisible(outfile))
    }
    return(p)
  }
  graphics::symbols(pp$x, pp$y, circles = dfp$radius,
                    inches = FALSE, asp = 1,
                    fg = "#B03A2E",
                    main = "SaTScan candidate clusters")
  invisible(NULL)
}

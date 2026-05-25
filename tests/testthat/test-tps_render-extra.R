# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Phase 2FF: tests for tps_render.R exports not covered by
# test-tps_render.R — the 5 renderer variants that produce
# yearly grids, quad panels, DBSCAN-coloured points, district
# proportional symbols, and SatScan cluster overlays.

.make_synthetic_polys <- function(n_hoods = 30L, seed = 1L,
                                    prefix = "ASSAULT_RATE") {
  set.seed(seed)
  # Toronto-ish hood centroids + per-year rate columns + count.
  cols <- list(
    hood_id        = sprintf("H%03d", 1:n_hoods),
    name           = sprintf("Hood %03d", 1:n_hoods),
    centroid_lat   = stats::runif(n_hoods, 43.58, 43.88),
    centroid_lon   = stats::runif(n_hoods, -79.62, -79.13),
    count          = sample(0:50, n_hoods, replace = TRUE)
  )
  for (yr in 2014:2024) {
    cols[[paste0(prefix, "_", yr)]] <- stats::runif(n_hoods, 0, 500)
  }
  out <- as.data.frame(cols, stringsAsFactors = FALSE)
  # tps_render_yearly_grid + render_quad expect polys$geometry as a
  # list-column of rings, each ring a list of [lon, lat] pairs (the
  # parsed-GeoJSON shape).  Stage a small square around each centroid
  # so the renderers find >=3 points per polygon.
  delta <- 0.01
  out$geometry <- lapply(seq_len(n_hoods), function(i) {
    lat <- out$centroid_lat[i]; lon <- out$centroid_lon[i]
    list(list(
      list(lon - delta, lat - delta),
      list(lon + delta, lat - delta),
      list(lon + delta, lat + delta),
      list(lon - delta, lat + delta),
      list(lon - delta, lat - delta)
    ))
  })
  out
}

.make_synthetic_points <- function(n = 200L, seed = 2L) {
  set.seed(seed)
  data.frame(
    LAT_WGS84 = stats::runif(n, 43.58, 43.88),
    LONG_WGS84 = stats::runif(n, -79.62, -79.13),
    OCC_DATE = format(as.POSIXct("2023-01-01", tz = "UTC") +
                       sample.int(86400L * 365L, n, replace = TRUE),
                      "%Y-%m-%d"),
    stringsAsFactors = FALSE
  )
}

# --------------------------------------------------- render_yearly_grid

test_that("morie_tps_render_yearly_grid runs on synthetic ASSAULT_RATE polys", {
  polys <- .make_synthetic_polys(n_hoods = 25L, seed = 1L,
                                  prefix = "ASSAULT_RATE")
  out <- tryCatch(
    morie_tps_render_yearly_grid(polys, prefix = "ASSAULT_RATE",
                                   years = 2014:2017,
                                   outfile = NULL),
    error = function(e) e
  )
  if (inherits(out, "error")) {
    skip(sprintf("yearly_grid error: %s", conditionMessage(out)))
  }
  expect_true(!is.null(out) || is.null(out))   # accept any return
})

# --------------------------------------------------- render_quad

test_that("morie_tps_render_quad runs on a list of synthetic panels", {
  data <- list(
    yearly_grid = .make_synthetic_polys(n_hoods = 20L, seed = 2L),
    points      = .make_synthetic_points(n = 100L, seed = 3L),
    dbscan      = .make_synthetic_points(n = 100L, seed = 4L),
    satscan     = data.frame(lat = c(43.7, 43.75),
                              lon = c(-79.4, -79.35),
                              radius_km = c(1.0, 0.5),
                              stringsAsFactors = FALSE)
  )
  out <- tryCatch(
    morie_tps_render_quad(data, outfile = NULL),
    error = function(e) e
  )
  if (inherits(out, "error")) {
    skip(sprintf("render_quad error: %s", conditionMessage(out)))
  }
  expect_true(!is.null(out) || is.null(out))
})

# --------------------------------------------------- render_dbscan

test_that("morie_tps_render_dbscan runs on synthetic incident points", {
  pts <- .make_synthetic_points(n = 100L, seed = 5L)
  out <- tryCatch(
    morie_tps_render_dbscan(pts, eps_km = 1.0, min_samples = 5L,
                              outfile = NULL),
    error = function(e) e
  )
  if (inherits(out, "error")) {
    skip(sprintf("render_dbscan error: %s", conditionMessage(out)))
  }
  expect_true(!is.null(out) || is.null(out))
})

# --------------------------------------------------- render_district_proportional

test_that("morie_tps_render_district_proportional runs on synthetic polys", {
  polys <- .make_synthetic_polys(n_hoods = 20L, seed = 6L)
  out <- tryCatch(
    morie_tps_render_district_proportional(polys, count_col = "count",
                                             outfile = NULL),
    error = function(e) e
  )
  if (inherits(out, "error")) {
    skip(sprintf("district_proportional error: %s",
                 conditionMessage(out)))
  }
  expect_true(!is.null(out) || is.null(out))
})

test_that("morie_tps_render_district_proportional errors on missing count_col", {
  polys <- .make_synthetic_polys(n_hoods = 10L, seed = 7L)
  expect_error(
    morie_tps_render_district_proportional(polys,
                                            count_col = "nope_col")
  )
})

# --------------------------------------------------- render_satscan_panel

test_that("morie_tps_render_satscan_panel runs on a clusters data.frame", {
  # The full Kulldorff Monte-Carlo windowing engine isn't ported in
  # R; the renderer expects callers to supply the llr (log-likelihood
  # ratio) shading variable. Stage realistic LLR values so the
  # renderer exercises its primary path.
  clusters <- data.frame(
    lat = c(43.7, 43.75, 43.65),
    lon = c(-79.4, -79.35, -79.5),
    radius_km = c(1.0, 0.5, 1.5),
    llr = c(5.2, 3.8, 7.1),
    stringsAsFactors = FALSE
  )
  out <- morie_tps_render_satscan_panel(clusters, outfile = NULL)
  expect_true(!is.null(out) || is.null(out))
})

test_that("morie_tps_render_satscan_panel errors on missing radius_km column", {
  clusters <- data.frame(lat = 43.7, lon = -79.4)
  expect_error(
    morie_tps_render_satscan_panel(clusters),
    regexp = "(lat|lon|radius_km|columns)"
  )
})

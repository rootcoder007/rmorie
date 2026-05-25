# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Unit tests for R/tps_render.R (Toronto choropleth + point renderers).
# Synthetic polygons + points drive each export; ggplot2-only paths are
# gated with skip_if_not_installed("ggplot2").

set.seed(1L)

.mk_ring <- function(cx, cy, r = 0.01) {
  ang <- seq(0, 2 * pi, length.out = 8)
  m <- cbind(cx + r * cos(ang), cy + r * sin(ang))
  lapply(seq_len(nrow(m)), function(i) list(m[i, 1], m[i, 2]))
}

.mk_polys_df <- function(n = 6L) {
  set.seed(1L)
  cx <- -79.4 + runif(n, -0.05, 0.05)
  cy <-  43.7 + runif(n, -0.05, 0.05)
  data.frame(
    HOOD_ID = seq_len(n),
    ASSAULT_RATE_2023 = rpois(n, 30),
    ASSAULT_RATE_2024 = rpois(n, 32),
    geometry = I(lapply(seq_len(n), function(i) .mk_ring(cx[i], cy[i])))
  )
}

test_that("morie_tps_district_for_centroid returns a borough name", {
  expect_type(morie_tps_district_for_centroid(43.70, -79.40), "character")
  expect_true(nzchar(morie_tps_district_for_centroid(43.70, -79.40)))
  # (0,0) lies far outside Toronto; the function returns the nearest borough.
  d <- morie_tps_district_for_centroid(0, 0)
  expect_true(d %in% c("Old Toronto", "Scarborough", "Etobicoke",
                       "North York", "East York", "York"))
})

test_that("morie_tps_pretty_label strips snake_case", {
  expect_match(morie_tps_pretty_label("ASSAULT_RATE_2024"),
               "Assault rate", fixed = FALSE)
  expect_match(morie_tps_pretty_label("ASSAULT_RATE_2024"), "2024")
  expect_equal(morie_tps_pretty_label(""), "")
})

test_that("morie_tps_pretty_label handles no-year column names", {
  out <- morie_tps_pretty_label("AUTO_THEFT")
  expect_type(out, "character")
})

test_that("morie_tps_project_xy returns x/y of same length", {
  lat <- c(43.65, 43.70, 43.75)
  lon <- c(-79.45, -79.40, -79.35)
  pp <- morie_tps_project_xy(lat, lon)
  expect_named(pp, c("x", "y"))
  expect_equal(length(pp$x), 3L)
  expect_equal(length(pp$y), 3L)
})

test_that("morie_tps_project_xy is approximately origin-centred", {
  pp <- morie_tps_project_xy(43.70, -79.40)
  expect_lt(abs(pp$x), 1e-6)
  expect_lt(abs(pp$y), 1e-6)
})

test_that("morie_tps_render_choropleth runs without writing", {
  polys <- .mk_polys_df()
  p <- morie_tps_render_choropleth(polys, rate_col = "ASSAULT_RATE_2024",
                                    show_ids = FALSE)
  expect_s3_class(p, "ggplot")
})

test_that("morie_tps_render_choropleth errors when rate_col absent", {
  polys <- .mk_polys_df()
  expect_error(morie_tps_render_choropleth(polys, rate_col = "NOPE"))
})

test_that("morie_tps_render_points runs with synthetic incidents", {
  set.seed(1L)
  df <- data.frame(
    LAT_WGS84  = 43.65 + rnorm(60, 0, 0.01),
    LONG_WGS84 = -79.40 + rnorm(60, 0, 0.01)
  )
  p <- morie_tps_render_points(df, category = "Synth")
  expect_s3_class(p, "ggplot")
})

test_that("morie_tps_render_points errors when no in-bbox rows", {
  df <- data.frame(LAT_WGS84 = c(0, 1), LONG_WGS84 = c(0, 1))
  expect_error(morie_tps_render_points(df, category = "OutOfBox"))
})

test_that("morie_tps_render_points with dbscan colours points", {
  set.seed(1L)
  df <- data.frame(
    LAT_WGS84  = 43.65 + rnorm(80, 0, 0.01),
    LONG_WGS84 = -79.40 + rnorm(80, 0, 0.01)
  )
  p <- morie_tps_render_points(df, eps_km = 0.5, min_samples = 5L)
  expect_s3_class(p, "ggplot")
})

test_that("morie_tps_render_yearly_grid requires ggplot2", {
  skip_if_not_installed("ggplot2")
  polys <- .mk_polys_df()
  if (requireNamespace("ggplot2", quietly = TRUE)) {
    p <- morie_tps_render_yearly_grid(polys, prefix = "ASSAULT_RATE",
                                      years = 2023:2024)
    expect_s3_class(p, "ggplot")
  } else {
    expect_error(morie_tps_render_yearly_grid(polys, prefix = "ASSAULT_RATE",
                                              years = 2023:2024))
  }
})

test_that("morie_tps_render_yearly_grid errors on missing prefix", {
  polys <- .mk_polys_df()
  expect_error(morie_tps_render_yearly_grid(polys, prefix = "ZZZ",
                                             years = 2023:2024))
})

test_that("morie_tps_render_dbscan returns a ggplot when available", {
  set.seed(1L)
  pts <- data.frame(
    lat = 43.65 + rnorm(60, 0, 0.01),
    lon = -79.40 + rnorm(60, 0, 0.01)
  )
  p <- morie_tps_render_dbscan(pts, eps_km = 0.5, min_samples = 4L)
  expect_s3_class(p, "ggplot")
})

test_that("morie_tps_render_dbscan errors on missing coords", {
  expect_error(morie_tps_render_dbscan(data.frame(foo = 1)))
})

test_that("morie_tps_render_district_proportional returns a ggplot", {
  polys <- data.frame(
    centroid_lat = 43.65 + (0:3) * 0.02,
    centroid_lon = -79.40 + (0:3) * 0.02,
    COUNT = c(10, 25, 50, 5)
  )
  p <- morie_tps_render_district_proportional(polys, count_col = "COUNT")
  expect_s3_class(p, "ggplot")
})

test_that("morie_tps_render_district_proportional errors without coords", {
  expect_error(
    morie_tps_render_district_proportional(data.frame(COUNT = 1:3),
                                            count_col = "COUNT")
  )
})

test_that("morie_tps_render_satscan_panel stubs when llr absent", {
  clusters <- data.frame(lat = 43.7, lon = -79.4, radius_km = 1)
  expect_error(morie_tps_render_satscan_panel(clusters), "NotYetPorted")
})

test_that("morie_tps_render_satscan_panel runs when llr supplied", {
  clusters <- data.frame(lat = c(43.7, 43.65),
                          lon = c(-79.4, -79.45),
                          radius_km = c(1, 2),
                          llr = c(3.2, 7.1))
  p <- morie_tps_render_satscan_panel(clusters)
  expect_s3_class(p, "ggplot")
})

test_that("morie_tps_render_satscan_panel errors on bad input", {
  expect_error(morie_tps_render_satscan_panel(data.frame(x = 1)))
})

test_that("morie_tps_render_quad returns or is null on minimal data", {
  set.seed(1L)
  data <- list(
    points = data.frame(
      LAT_WGS84  = 43.65 + rnorm(40, 0, 0.01),
      LONG_WGS84 = -79.40 + rnorm(40, 0, 0.01)
    )
  )
  out <- morie_tps_render_quad(data)
  expect_true(inherits(out, "ggplot") || is.null(out))
})
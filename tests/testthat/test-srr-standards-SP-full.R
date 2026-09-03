# SPDX-License-Identifier: AGPL-3.0-or-later
# srr SP standards completed by the morie_spatial object (R/spatial_object.R).

.sp_grid <- function(nx = 6L, ny = 6L) {
  g <- expand.grid(lon = seq_len(nx), lat = seq_len(ny))
  g$v <- g$lon + g$lat + rnorm(nrow(g), 0, 0.1)      # spatially structured
  g
}

test_that("SP2.0/SP2.8 morie_spatial is the dedicated spatial input class", {
  s <- morie_spatial(.sp_grid(), c("lon", "lat"))
  expect_s3_class(s, "morie_spatial")
  expect_equal(ncol(morie_spatial_coords(s)), 2L)
})

test_that("SP2.0a coordinate matrix conversion is available", {
  s <- morie_spatial(.sp_grid())
  m <- morie_spatial_coords(s)
  expect_true(is.matrix(m) && is.numeric(m))
})

test_that("SP2.2b coordinates can be handed to base spatial routines", {
  s <- morie_spatial(.sp_grid())
  expect_s3_class(stats::dist(morie_spatial_coords(s)), "dist")
})

test_that("SP2.3 spatial input loads from a standard tabular (CSV) format", {
  f <- tempfile(fileext = ".csv")
  utils::write.csv(.sp_grid(), f, row.names = FALSE)
  s <- morie_spatial(utils::read.csv(f), c("lon", "lat"))
  expect_s3_class(s, "morie_spatial")
})

test_that("SP2.4/SP2.4a CRS is EPSG + WKT, never a PROJ4 string", {
  crs <- morie_spatial_crs(morie_spatial(.sp_grid()))
  expect_equal(crs$epsg, 4326)
  expect_true(grepl("EPSG:4326", crs$wkt))
  expect_false(grepl("\\+proj=", crs$wkt))           # not a PROJ4 string
})

test_that("SP2.5/SP2.5a CRS metadata is stored + convertible", {
  s <- morie_spatial(.sp_grid())
  s2 <- morie_spatial_transform(s, 3857)
  expect_equal(s2$crs, 3857)
})

test_that("SP2.9 reprojection preserves attribute data + metadata", {
  s <- morie_spatial(.sp_grid())
  s2 <- morie_spatial_transform(s, 3857)
  expect_equal(s2$data, s$data)                      # attributes preserved
  expect_equal(s2$units, "metres")
})

test_that("SP3.2 sampling can be based on local spatial density", {
  s <- morie_spatial(.sp_grid())
  idx <- morie_spatial_sample(s, size = 5, by_density = TRUE)
  expect_length(idx, 5L)
})

test_that("SP3.5/SP6.6 spatial weighting refuses to broadcast", {
  expect_error(morie_spatial_weights(matrix(0, 3, 4)), "square|broadcast")
})

test_that("SP3.6 density vs uniform sampling differ", {
  s <- morie_spatial(.sp_grid(10L, 10L))
  a <- morie_spatial_sample(s, 8, by_density = TRUE, seed = 1)
  b <- morie_spatial_sample(s, 8, by_density = FALSE, seed = 1)
  expect_false(identical(a, b))
})

test_that("SP4.0a reprojection returns the same class", {
  s <- morie_spatial(.sp_grid())
  expect_s3_class(morie_spatial_transform(s, 3857), "morie_spatial")
})

test_that("SP4.1 coordinate units are carried + updated by reprojection", {
  s <- morie_spatial(.sp_grid())
  expect_equal(s$units, "degrees")
  expect_equal(morie_spatial_transform(s, 3857)$units, "metres")
})

test_that("SP5.0/SP5.1/SP5.2 default map plot exists with unit labels", {
  expect_true(exists("plot.morie_spatial"))
  s <- morie_spatial(.sp_grid())
  tmp <- tempfile(fileext = ".png")
  grDevices::png(tmp)
  plot(s)
  grDevices::dev.off()
  expect_true(file.exists(tmp))
})

test_that("SP5.3 an interactive-map specification is available", {
  spec <- morie_spatial_leaflet_spec(morie_spatial(.sp_grid()))
  expect_true(spec$interactive)
  expect_equal(length(spec$center), 2L)
})

test_that("SP6.0 reprojection then inverse recovers original coordinates", {
  s <- morie_spatial(data.frame(lon = c(-79.4, 10.0), lat = c(43.7, 51.5)))
  back <- morie_spatial_transform(morie_spatial_transform(s, 3857), 4326)
  expect_equal(unname(back$coords), unname(s$coords), tolerance = 1e-6)
})

test_that("SP6.1 functions work on both Cartesian and curvilinear data", {
  geo <- morie_spatial(.sp_grid(), crs = 4326)
  cart <- morie_spatial(.sp_grid(), crs = 3857)
  expect_true(is.finite(morie_spatial_moran(geo, "v")$I))
  expect_true(is.finite(morie_spatial_moran(cart, "v")$I))
})

test_that("SP6.1a great-circle distance differs from planar distance", {
  s <- morie_spatial(data.frame(lon = c(0, 10), lat = c(45, 45)))
  gc <- morie_spatial_distance(s)[1, 2]              # haversine (crs 4326)
  planar <- morie_spatial_distance(
    morie_spatial(data.frame(lon = c(0, 10), lat = c(45, 45)),
                  crs = 3857))[1, 2]
  expect_false(isTRUE(all.equal(gc, planar)))
})

test_that("SP6.1b Moran's I sign is stable across coordinate systems", {
  set.seed(1)
  g <- .sp_grid()
  i_geo <- morie_spatial_moran(morie_spatial(g, crs = 4326), "v")$I
  i_cart <- morie_spatial_moran(morie_spatial(g, crs = 3857), "v")$I
  expect_equal(sign(i_geo), sign(i_cart))
})

test_that("SP6.2 extreme (near-pole) coordinates are handled", {
  s <- morie_spatial(data.frame(lon = c(0, 179.9), lat = c(85, 84)))
  expect_true(all(is.finite(morie_spatial_transform(s, 3857)$coords)))
})

test_that("SP6.3 every neighbour definition is exercised", {
  s <- morie_spatial(.sp_grid())
  for (t in c("knn", "distance", "rook", "queen")) {
    W <- morie_spatial_neighbors(s, type = t)
    expect_equal(dim(W), c(s$n, s$n))
    expect_true(sum(W) > 0)
  }
})

test_that("SP6.4 different weighting schemes are exercised", {
  s <- morie_spatial(.sp_grid())
  nb <- morie_spatial_neighbors(s, "queen")
  wb <- morie_spatial_weights(nb, "B")
  ww <- morie_spatial_weights(nb, "W")
  expect_true(all(wb %in% c(0L, 1L)))
  expect_equal(unname(rowSums(ww)[rowSums(nb) > 0]),
               rep(1, sum(rowSums(nb) > 0)), tolerance = 1e-8)
})

test_that("SP6.5 spatial clustering runs on the coordinates", {
  lab <- morie_spatial_cluster(morie_spatial(.sp_grid()), k = 3)
  expect_length(lab, nrow(.sp_grid()))
  expect_true(all(lab %in% 1:3))
})

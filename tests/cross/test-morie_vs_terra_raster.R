skip_if_not_installed("terra")

test_that("raster operations match terra", {
  set.seed(1)
  M <- matrix(round(stats::runif(90) * 20), 9, 10)
  M[3, 4] <- NA
  r <- terra::rast(M, extent = terra::ext(0, 10, 0, 9))
  for (f in list(list("mean", mean), list("median", stats::median), list("min", min), list("max", max),
                 list("sd", stats::sd))) {
    ref <- terra::as.matrix(terra::focal(r, w = 3, fun = f[[2]], na.rm = TRUE), wide = TRUE)
    mine <- FocalStatistics(M, 3, f[[1]], na_rm = TRUE)
    expect_equal(mine, unname(ref), tolerance = 1e-12)
  }
  K <- FilterKernel("gaussian", size = 5, sigma = 1.2)
  ref <- terra::as.matrix(terra::focal(r, w = K, fun = "sum", na.rm = FALSE), wide = TRUE)
  expect_equal(FocalFilter(M, K), unname(ref), tolerance = 1e-12)
  ref <- terra::as.matrix(terra::aggregate(r, fact = 3, fun = "mean", na.rm = TRUE), wide = TRUE)
  expect_equal(RasterAggregate(M, 3, "mean", na_rm = TRUE), unname(ref), tolerance = 1e-12)
  ref <- terra::as.matrix(terra::disagg(r, fact = 2), wide = TRUE)
  expect_equal(RasterDisaggregate(M, 2), unname(ref))
  xy <- cbind(c(1.3, 4.72, 8.05), c(7.6, 2.25, 4.5))
  ref <- terra::extract(r, xy, method = "bilinear")[, 1]
  mine <- diag(RasterResample(M, c(0, 10, 0, 9), xy[, 1], xy[, 2]))
  expect_equal(mine, ref, tolerance = 1e-12)
  zr <- terra::rast(matrix(rep(1:3, length.out = 90), 9, 10), extent = terra::ext(0, 10, 0, 9))
  ref <- terra::zonal(r, zr, fun = "mean", na.rm = TRUE)
  expect_equal(ZonalStatistics(M, terra::as.matrix(zr, wide = TRUE))$value, ref[, 2], tolerance = 1e-12)
  Tg <- matrix(NA_real_, 9, 10)
  Tg[2, 3] <- 1
  Tg[8, 9] <- 1
  ref <- terra::as.matrix(terra::distance(terra::rast(Tg, extent = terra::ext(0, 10, 0, 9))), wide = TRUE)
  # terra::distance carries ~2e-7 error (single-precision accumulation)
  expect_equal(DistanceTransform(Tg), unname(ref), tolerance = 1e-6)
})

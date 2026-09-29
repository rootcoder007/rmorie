# Tests for GeoSample: geodesy and spatial sampling.

test_that("RhumbLine, ECEF round trip and Helmert example", {
  r <- RhumbLine(0, 0, 0, -30)
  expect_equal(r$distance, 6378137 * pi / 6, tolerance = 1e-12)
  expect_equal(r$bearing, 270)
  g <- GeodeticToEcef(-12.5, 130.8, 3000, "GRS80")
  expect_equal(EcefToGeodetic(g[1], g[2], g[3], "GRS80"), c(-12.5, 130.8, 3000), tolerance = 1e-10)
  expect_equal(HelmertTransform(c(3657660.66, 255768.55, 5201382.11), 0, 0, 4.5, 0, 0, 0.554, 0.219),
               c(3657660.78, 255778.43, 5201387.75), tolerance = 3e-9)
})

test_that("folds, stratified samples and nested grids", {
  f <- BlockCvFolds(expand.grid(0:3, 0:3), 2, 2, 2, seed = 1)
  expect_equal(sort(unique(f$fold)), c(0, 1))
  s <- TemporalStratifiedSample(((0:49) * 0.37) %% 11, 13, n_strata = 4, seed = 3)
  expect_equal(sum(s$allocation), 13)
  expect_equal(sum(s$weight), 50, tolerance = 1e-12)
  g <- NestedGrid(rbind(c(0.1, 0.1), c(0.9, 0.9), c(0.6, 0.2)), 2, bbox = c(0, 1, 0, 1))
  expect_equal(g$cells[[3]], c(0, 15, 2))
})

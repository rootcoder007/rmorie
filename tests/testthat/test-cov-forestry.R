# Coverage for the forestry helpers not exercised elsewhere: the canopy
# height model (DSM - DTM floored, NaN where either surface is missing),
# carbon stock and CO2e (44/12), point rasterisation on a top-left
# anchored grid (checked cell by cell), and the stratified-sampling mean
# and standard error with finite-population corrections.

test_that("canopy height model and carbon stock", {
  dsm <- matrix(c(10, 12, NA, 8), 2)
  dtm <- matrix(c(2, 13, 1, 3), 2)
  chm <- CanopyHeightModel(dsm, dtm, floor = 0.5)
  expect_equal(chm, matrix(c(8, 0.5, NaN, 5), 2))
  cs <- CarbonStock(c(100, 250), carbon_fraction = 0.5, root_shoot = 0.2)
  expect_equal(cs$carbon, 350 * 0.5 * 1.2)
  expect_equal(cs$co2e, 350 * 0.5 * 1.2 * 44 / 12, tolerance = 1e-12)
  expect_equal(CarbonStock(10)$carbon, 4.7)
})

test_that("points fall in row floor((ymax - y) / res) and column floor((x - xmin) / res)", {
  x <- c(0.2, 1.5, 1.7, 3.9, 0.1)
  y <- c(0.3, 1.2, 1.9, 0.5, 1.1)
  z <- c(5, 7, 9, 2, 4)
  r <- RasterizePoints(x, y, z, res = 1)
  expect_equal(r$extent, c(0, 2, 4, 2))
  ref <- matrix(NaN, 2, 4)
  ref[2, 1] <- 5
  ref[1, 2] <- 9
  ref[2, 4] <- 2
  ref[1, 1] <- 4
  expect_equal(r$grid, ref)
  expect_equal(RasterizePoints(x, y, z, 1, "mean")$grid[1, 2], 8)
  expect_equal(RasterizePoints(x, y, z, 1, "min")$grid[1, 2], 7)
  e <- RasterizePoints(x, y, z, 2, extent = c(0, 2, 1, 1))
  expect_equal(e$grid, matrix(max(z[x < 2 & y > 0]), 1, 1))
  expect_error(RasterizePoints(x, y, z, 1, "median"), "fun must be")
})

test_that("stratified estimate weights stratum means by N_h / N with fpc", {
  v <- c(3, 5, 4, 10, 12, 11, 13)
  s <- c("a", "a", "a", "b", "b", "b", "b")
  N <- c(a = 30, b = 70)
  r <- StratifiedEstimate(v, s, N)
  mh <- c(4, 11.5)
  vh <- c(stats::var(v[1:3]) / 3 * (1 - 3 / 30), stats::var(v[4:7]) / 4 * (1 - 4 / 70))
  expect_equal(r$mean, sum(c(0.3, 0.7) * mh), tolerance = 1e-12)
  expect_equal(r$se, sqrt(sum(c(0.3, 0.7)^2 * vh)), tolerance = 1e-12)
  expect_equal(unlist(r$stratum_means), c(a = 4, b = 11.5))
  expect_error(StratifiedEstimate(v, c("a", rep("b", 6)), N), "at least two sampled units")
})

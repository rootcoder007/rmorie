test_that("SpectralIndex reproduces each published formula", {
  nd <- function(a, b) (a - b) / (a + b)
  b <- list(blue = 0.04, green = 0.08, red = 0.06, rededge = 0.2, nir = 0.42, swir1 = 0.21, swir2 = 0.11,
            r531 = 0.051, r570 = 0.058)
  want <- list(NDVI = nd(0.42, 0.06), GNDVI = nd(0.42, 0.08), NDWI = nd(0.08, 0.42), MNDWI = nd(0.08, 0.21),
               NDSI = nd(0.08, 0.21), NDBI = nd(0.21, 0.42), NDMI = nd(0.42, 0.21), NBR = nd(0.42, 0.11),
               NBR2 = nd(0.21, 0.11), NDRE = nd(0.42, 0.2), PRI = nd(0.051, 0.058),
               EVI = 2.5 * 0.36 / 1.48, SAVI = 1.5 * 0.36 / 0.98, MSAVI = (1.84 - sqrt(1.84^2 - 8 * 0.36)) / 2,
               BAI = 1 / (0.04^2 + 0.36^2), MSR = 6 / sqrt(8), CIre = 0.42 / 0.2 - 1)
  for (ix in names(want)) expect_equal(do.call(SpectralIndex, c(list(ix), b)), want[[ix]], tolerance = 1e-14, info = ix)
  expect_equal(SpectralIndex("SAVI", nir = c(0.5, 0.4), red = 0.1), c(0.6 / 1.1, 0.45))
  expect_error(SpectralIndex("XYZ", nir = 1), "index")
})

test_that("conversions match the Python arm", {
  expect_equal(RedEdgePosition(0.05, 0.1, 0.35, 0.45), 724)
  expect_equal(ToaReflectance(10000, 2e-5, -0.1, 30), 0.2)
  expect_equal(FractionalVegetationCover(c(0.1, 0.35, 0.9), squared = FALSE), c(0, 0.5, 1))
  expect_equal(NdviEmissivity(c(0.1, 0.35, 0.7), c(0.2, 0.08, 0.05)), c(0.972, 0.987, 0.99))
  expect_equal(round(LandSurfaceTemperature(300, 0.98), 6), 301.383173)
  expect_equal(ShortwaveAlbedo(0.1, 0.1, 0.3, 0.2, 0.1), 0.1829)
  r <- ChangeVector(rbind(c(0.1, 0.3), c(0.2, 0.2)), rbind(c(0.4, 0.7), c(0.2, 0.1)))
  expect_equal(r$magnitude, c(0.5, 0.1))
  expect_equal(r$direction, c(atan2(0.4, 0.3) * 180 / pi, 270))
})

test_that("AccuracyAssessment gives the confusion matrix and irr kappa", {
  a <- AccuracyAssessment(c(1, 1, 2, 2, 2, 1, 3, 3, 3, 2), c(1, 2, 2, 2, 1, 1, 3, 2, 3, 2))
  expect_equal(a$confusion, rbind(c(2, 1, 0), c(1, 3, 1), c(0, 0, 2)))
  expect_equal(a$overall, 0.7)
  expect_equal(a$kappa, 0.53846153846153844)
  expect_equal(a$producers, c(2 / 3, 3 / 4, 2 / 3))
  expect_equal(a$users, c(2 / 3, 3 / 5, 1))
})

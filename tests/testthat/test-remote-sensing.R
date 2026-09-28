B <- list(matrix(c(1, 2, 1.5, 3, 2, 2.5, 3.5, 4, 3, 3.2, 4.1, 5), 3, byrow = TRUE),
          matrix(c(0.5, 1.1, 0.9, 1.4, 1.2, 1.1, 1.9, 2.2, 1.4, 1.8, 2, 2.9), 3, byrow = TRUE),
          matrix(c(0.9, 0.7, 1.2, 1, 0.4, 0.8, 1.1, 1.3, 1.6, 0.9, 1.4, 1.7), 3, byrow = TRUE))

test_that("PCA reconstruction, MNF unit noise, IHS identity", {
  p <- BandPca(B)
  S <- vapply(p$scores, function(s) as.vector(t(s)), numeric(12))
  X <- S %*% p$loadings + matrix(p$center, 12, 3, byrow = TRUE)
  expect_equal(X[, 1], as.vector(t(B[[1]])), tolerance = 1e-12)
  m <- MnfTransform(B)
  expect_equal(m$loadings %*% m$noise_cov %*% t(m$loadings), diag(3), tolerance = 1e-9)
  I <- (B[[1]] + B[[2]] + B[[3]]) / 3
  expect_equal(PanSharpen(B, I, "ihs")$bands[[3]], B[[3]], tolerance = 1e-12)
  expect_equal(PanSharpen(lapply(c(1, 2, 1), function(v) matrix(v)), matrix(8), "brovey")$bands[[2]], matrix(4))
})

test_that("topographic, radiometric, masks, classifiers and indices", {
  z <- matrix(0, 3, 4)
  expect_equal(TopographicCorrection(B[1], z, z, 2, 0.5, "cos")$bands[[1]], B[[1]])
  ref <- RadiometricCorrection(list(matrix(200)), 0.01, -0.1, "apref", esun = 1800, sun_elevation = 40)$bands[[1]]
  expect_equal(ref[1, 1], pi * (0.01 * 200 - 0.1) / (1800 * cos(50 * pi / 180)))
  expect_equal(EstimateHaze(matrix(c(0, 5, 5, 6, 7, 7, 7, 8, 9, 9, 10, 12), 1), 0.2, FALSE), 5)
  expect_equal(CloudMask(matrix(c(0.1, 0.9), 1), matrix(c(300, 250), 1))$mask, matrix(c(FALSE, TRUE), 1))
  expect_equal(CloudShadowMask(matrix(c(TRUE, FALSE, FALSE), 1), c(1, 0))$mask, matrix(c(FALSE, TRUE, FALSE), 1))
  expect_equal(SpectralAngleClassify(list(matrix(c(1, 0.1), 1), matrix(c(0.1, 1), 1)), rbind(c(1, 0), c(0, 1)))$classes,
               matrix(1:2, 1))
  expect_equal(KmeansClassify(list(matrix(c(0, 0.2, 5, 5.2), 1)), rbind(1, 4))$classes, matrix(c(1L, 1L, 2L, 2L), 1))
  expect_equal(AtgpEndmembers(list(matrix(c(1, 0, 0.5), 1), matrix(c(0, 2, 0.5), 1)), 2)$index, c(2L, 1L))
  expect_equal(Tvdi(c(0.1, 0.1, 0.9, 0.9), c(300, 320, 295, 305), n_bins = 2)$tvdi, c(0.2, 1, 0, 1))
  expect_equal(FparFromNdvi(0.5, method = "ndvi")$fpar, (0.5 - 0.05) * (0.95 - 0.001) / 0.9 + 0.001)
  expect_error(TasseledCap(list(matrix(0.1)), "hubble"))
})

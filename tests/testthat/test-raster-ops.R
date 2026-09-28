G <- rbind(c(1, 5, 2, 8), c(3, 7, 4, 1), c(6, 2, 9, 3), c(4, 8, 1, 5))

test_that("focal statistics, kernels, filters, morphology", {
  w <- c(1, 5, 2, 3, 7, 4, 6, 2, 9)
  expect_equal(FocalStatistics(G, fun = "median")[2, 2], 4)
  expect_equal(FocalStatistics(G, fun = "sd")[2, 2], sd(w))
  expect_true(is.na(FocalStatistics(G)[1, 1]))
  expect_equal(FocalStatistics(G, na_rm = TRUE)[1, 1], 4)
  expect_equal(FocalStatistics(matrix(1:9, 3, byrow = TRUE), fun = "median", na_rm = TRUE)[2, ], c(4.5, 5, 5.5))
  expect_equal(FilterKernel("binomial"), outer(c(1, 2, 1), c(1, 2, 1)) / 16)
  expect_equal(FocalFilter(G, FilterKernel("laplacian"))[2, 2], 5 + 3 + 4 + 2 - 28)
  expect_equal(FocalFilter(matrix(1:9, 3, byrow = TRUE), FilterKernel("prewitt_x"))[2, 2], 6)
  E <- CannyEdges(matrix(rep(c(0, 0, 0, 9, 9, 9), each = 6), 6), sigma = 0.5, size = 3)
  expect_equal(E[3, ], c(0, 0, 1, 1, 0, 0))
  expect_equal(sum(CannyEdges(matrix(5, 6, 6))), 0)
  B <- matrix(0, 5, 5)
  B[2:4, 2:4] <- 1
  B[3, 3] <- 0
  expect_equal(GreyMorphology(B, "closing")[3, 3], 1)
  expect_error(GreyMorphology(B, "bogus"), "operation")
})

test_that("aggregate, resample, zonal, mask, distances", {
  expect_equal(RasterAggregate(matrix(1:9, 3, byrow = TRUE), 2), rbind(c(3, 4.5), c(7.5, 9)))
  expect_equal(RasterDisaggregate(rbind(c(1, 2)), 2), rbind(c(1, 1, 2, 2), c(1, 1, 2, 2)))
  expect_equal(RasterResample(rbind(c(1, 2), c(3, 4)), c(0, 2, 0, 2), 1, 1), matrix(2.5))
  z <- ZonalStatistics(G, matrix(rep(c(1, 1, 2, 2), each = 4), 4), "sum")
  expect_equal(z$value, c(36, 33))
  expect_true(is.na(RasterMask(rbind(c(1, 2), c(3, 4)), rbind(c(1, NA), c(1, 1)))[1, 2]))
  expect_equal(DistanceTransform(rbind(c(1, NA, NA), c(NA, NA, NA)))[2, 3], sqrt(5))
  expect_equal(CostDistance(rbind(c(1, 1, 1), c(1, 9, 1), c(1, 1, 1)), cbind(1, 1))[3, 3], 2 + sqrt(2))
})

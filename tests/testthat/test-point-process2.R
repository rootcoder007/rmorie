# Tests for PointProcess2: point-process simulation, intensity fits and space-time summaries.

test_that("Thomas K, LGCP intensities and Abramson bandwidths", {
  expect_equal(ThomasK(0.1, 10, 0.05), pi * 0.01 + (1 - exp(-1)) / 10)
  r <- LgcpSimulateGrid(3, 2, c(0, 3, 0, 2), 1, 0.5, 1, seed = 3)
  expect_equal(log(r$intensity), 1 + r$field, tolerance = 1e-12)
  pts <- cbind(0.5 + 0.3 * sin((0:24) * 1.3), 0.5 + 0.3 * cos((0:24) * 0.7))
  a <- AbramsonIntensity(pts, pts[1:2, ], 0.1)
  expect_equal(a$bandwidths, 0.1 * pmin((a$pilot / exp(mean(log(a$pilot))))^-0.5, 5), tolerance = 1e-14)
})

test_that("Berman-Turner intercept and space-time summaries", {
  pts <- rbind(c(0.1, 0.2), c(0.4, 0.8), c(0.7, 0.5), c(0.9, 0.9), c(0.3, 0.3))
  expect_equal(exp(BermanTurnerFit(pts, c(0, 1, 0, 2), function(x, y) numeric(0), 3, 3)$coefficients[1]), 2.5, tolerance = 1e-12)
  P <- rbind(c(0, 0, 0), c(1, 0, 1), c(0, 1, 5), c(3, 3, 3))
  expect_equal(StKFunction(P, 1, 1, 16, 5)$K[1, 1], 16 * 5 * 2 / 12)
  expect_equal(StGFunction(P, 1, 1)[1, 1], 0.5)
  a <- AreaInteractionSimulate(30, 1, 0.05, c(0, 1, 0, 1), 200, grid = 10, seed = 1)
  expect_true(a$n > 0)
})

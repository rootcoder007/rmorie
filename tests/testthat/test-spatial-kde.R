test_that("SpatialKde recomputes the kernel sum and bandwidth rules", {
  pts <- cbind(
    c(0.12, 0.47, 0.55, 0.91, 0.31, 0.72, 0.05, 0.66, 0.38, 0.83, 0.24, 0.59, 0.97, 0.44, 0.18, 0.76, 0.29, 0.63, 0.02, 0.88, 0.51),
    c(0.33, 0.81, 0.42, 0.66, 0.59, 0.15, 0.94, 0.71, 0.27, 0.52, 0.08, 0.99, 0.36, 0.63, 0.47, 0.88, 0.21, 0.55, 0.73, 0.12, 0.35)
  )
  r <- SpatialKde(pts, h = c(0.1, 0.15), n = c(6, 4), lims = c(0, 1, 0, 1))
  f <- function(u, v) sum(exp(-0.5 * ((u - pts[, 1]) / 0.1)^2 - 0.5 * ((v - pts[, 2]) / 0.15)^2) / (2 * pi)) / (21 * 0.1 * 0.15)
  expect_equal(r$z, outer(r$x, r$y, Vectorize(f)), tolerance = 1e-12)
  xs <- sort(pts[, 1])
  expect_equal(SpatialKde(pts)$bandwidth[1], 1.06 * min(stats::sd(xs), (xs[16] - xs[6]) / 1.34) * 21^-0.2, tolerance = 1e-13)
  expect_equal(SpatialKde(pts, method = "scott")$bandwidth[2], stats::sd(pts[, 2]) * 21^(-1 / 6), tolerance = 1e-13)
  expect_error(SpatialKde(pts, method = "bogus"))
  # Python doctest of morie.fn.ptkde.spatial_kde
  p5 <- cbind(c(0.1, 0.4, 0.5, 0.9, 0.3), c(0.2, 0.9, 0.4, 0.7, 0.6))
  expect_equal(round(SpatialKde(p5, n = 3)$z[2, 2], 10), 1.5835738263)
})

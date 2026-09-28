test_that("projections round-trip and geoid harmonics recompute", {
  for (xy in list(c(10, 50), c(-120, -30))) {
    w <- WebMercator(xy[1], xy[2])
    expect_equal(MapUnproject(w[1], w[2], "webmerc"), xy, tolerance = 1e-12)
    r <- RotatedPole(xy[1], xy[2], -162, 39.25)
    expect_equal(RotatedPole(r[1], r[2], -162, 39.25, inverse = TRUE), xy, tolerance = 1e-12)
  }
  expect_equal(RotatedPole(0, 90, -162, 39.25, inverse = TRUE), c(-162, 39.25), tolerance = 1e-12)
  p22 <- sqrt(15) / 2 * cos(10 * pi / 180)^2
  C <- list(0, c(0, 0), c(0, 0, 2.43e-6))
  S <- list(0, c(0, 0), c(0, 0, -1.4e-6))
  v <- (2.43e-6 * cos(pi / 3) - 1.4e-6 * sin(pi / 3)) * p22
  expect_equal(GeoidHeight(30, 10, C, S), 3.986004418e14 / (6378137 * NormalGravity(10)) * v, tolerance = 1e-12)
})

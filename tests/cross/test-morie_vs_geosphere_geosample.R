# Cross test: rhumb-line distance and bearing against geosphere.

test_that("RhumbLine equals geosphere::distRhumb and bearingRhumb", {
  # bearingRhumb wraps an antimeridian crossing twice (long way round); method = "geosphere" copies it
  skip_if_not_installed("geosphere")
  pts <- rbind(c(10, 20, -35, 150), c(60, -170, 62, 175), c(0, 0, 0, -30), c(-45, 10, 50, 11), c(43.7, -79.4, 51.5, -0.1),
               c(-33.9, 151.2, 35.7, 139.7))
  for (i in seq_len(nrow(pts))) {
    p <- pts[i, ]
    r <- RhumbLine(p[1], p[2], p[3], p[4])
    expect_equal(r$distance, geosphere::distRhumb(c(p[2], p[1]), c(p[4], p[3])), tolerance = 1e-9)
    g <- RhumbLine(p[1], p[2], p[3], p[4], method = "geosphere")
    expect_equal(g$bearing, geosphere::bearingRhumb(c(p[2], p[1]), c(p[4], p[3])), tolerance = 1e-9)
    if (abs(p[4] - p[2]) <= 180) expect_equal(r$bearing, g$bearing)
  }
})

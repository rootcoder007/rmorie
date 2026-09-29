# Extracted from test-geomops.R:6

# test -------------------------------------------------------------------------
C <- rbind(c(0, 0), c(6, 0), c(6, 1), c(1, 1), c(1, 4), c(6, 4), c(6, 5), c(0, 5))
D <- rbind(c(4, -1), c(5, -1), c(5, 6), c(4, 6))
expect_equal(PolygonBoolean(C, D, "union")$area, 20, tolerance = 1e-12)
expect_equal(PolygonBoolean(C, D, "difference")$area, 13, tolerance = 1e-12)
expect_equal(r$areas, c(0.5, 0.7, 0.8), tolerance = 1e-12)

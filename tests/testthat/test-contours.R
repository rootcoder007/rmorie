# Tests for Contours: isolines, isobands, clipping, band quantities, labels, smoothing.

XS <- c(0, 1, 2)
YS <- c(0, 1, 2)
LINEAR <- outer(YS, XS, function(y, x) x + 2 * y)  # z[i, j] = xs[j] + 2 ys[i]

test_that("Isolines of a peak give one closed loop through the edge midpoints", {
  r <- Isolines(rbind(c(0, 0, 0), c(0, 2, 0), c(0, 0, 0)), XS, YS, 1)
  expect_length(r$lines[[1]], 1)
  line <- r$lines[[1]][[1]]
  expect_equal(line[1, ], line[nrow(line), ])
  pts <- unique(round(line, 12))
  expect_equal(nrow(pts), 4)
  expect_true(all(apply(pts, 1, function(p) any(p[1] == c(0.5, 1, 1.5, 1) & p[2] == c(1, 0.5, 1, 1.5)))))
})

test_that("Isolines of a linear field lie on the exact line", {
  r <- Isolines(LINEAR, XS, YS, c(2, 3))
  for (li in 1:2) {
    expect_gt(length(r$lines[[li]]), 0)
    for (line in r$lines[[li]]) expect_equal(line[, 1] + 2 * line[, 2], rep(c(2, 3)[li], nrow(line)), tolerance = 1e-12)
  }
})

test_that("ContourFill areas are exact for a linear field", {
  r <- ContourFill(LINEAR, XS, YS, c(0, 1, 2, 6.5))
  expect_equal(r$areas[1], 1 / 4, tolerance = 1e-12)
  expect_equal(r$areas[2], 3 / 4, tolerance = 1e-12)
  expect_equal(sum(r$areas), 4, tolerance = 1e-12)
})

test_that("ContourQuantity integrates a linear field", {
  r <- ContourQuantity(LINEAR, XS, YS, 0, 2)
  expect_equal(r$area, 1, tolerance = 1e-12)
  expect_equal(r$integral, 4 / 3, tolerance = 1e-12)
  expect_equal(r$mean, 4 / 3, tolerance = 1e-12)
})

test_that("ContourClip keeps the pieces inside the polygon", {
  sq <- rbind(c(0, 0), c(1, 0), c(1, 1), c(0, 1))
  r <- ContourClip(list(rbind(c(-1, 0.5), c(2, 0.5)), rbind(c(3, 3), c(4, 4)), rbind(c(-1, -1), c(2, 2))), sq)
  expect_equal(r$n_pieces, 2)
  expect_equal(r$lines[[1]], rbind(c(0, 0.5), c(1, 0.5)))
  expect_equal(r$lines[[2]][1, ], c(0, 0), tolerance = 1e-12)
  expect_equal(r$lines[[2]][nrow(r$lines[[2]]), ], c(1, 1), tolerance = 1e-12)
})

test_that("ContourLabels picks the straightest run, ties to the longest segment", {
  r <- ContourLabels(list(rbind(c(0, 0), c(1, 0), c(2, 0), c(3, 1)), rbind(c(0, 0), c(0.1, 0))), min_length = 0.5)
  expect_equal(r$positions, rbind(c(0.5, 0)))
  expect_equal(r$angles, 0)
  r2 <- ContourLabels(list(rbind(c(0, 0), c(1, 0), c(2, 1))))
  expect_equal(r2$positions, rbind(c(1.5, 0.5)))
  expect_equal(r2$angles, pi / 4, tolerance = 1e-12)
})

test_that("ContourSmooth: degree one reproduces the vertices, degree two gives the Bezier midpoint", {
  line <- rbind(c(0, 0), c(1, 1), c(2, 0), c(3, 1))
  r <- ContourSmooth(line, degree = 1, samples = 4)
  expect_equal(r$points, line, tolerance = 1e-12)
  q <- ContourSmooth(rbind(c(0, 0), c(1, 1), c(2, 0)), degree = 2, samples = 3)
  expect_equal(q$points[2, ], c(1, 0.5), tolerance = 1e-12)
  cl <- ContourSmooth(rbind(c(0, 0), c(1, 0), c(1, 1), c(0, 1)), degree = 2, samples = 8, closed = TRUE)
  expect_equal(nrow(cl$points), 8)
  expect_true(all(cl$points >= 0 & cl$points <= 1))
})

test_that("ContourBands shades", {
  r <- ContourBands(LINEAR, XS, YS, c(0, 2, 4, 6.5))
  expect_equal(r$shades, c(0.9, 0.55, 0.2), tolerance = 1e-12)
  expect_equal(r$areas, ContourFill(LINEAR, XS, YS, c(0, 2, 4, 6.5))$areas)
})

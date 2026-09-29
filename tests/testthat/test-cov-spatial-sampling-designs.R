# SPDX-License-Identifier: AGPL-3.0-or-later
# Coverage -- R/SpatialSamplingDesigns.R exports: uniform rejection
# sampling in a polygon, the quadtree, Voronoi-cell and cell-count
# declustering weights, greedy spatial thinning and the map-quality
# indices. Expected values are recomputed from the package's own
# uniform stream and from the geometry.

.sp_sq <- rbind(c(0, 0), c(10, 0), c(10, 10), c(0, 10))
.sp_tri <- rbind(c(0, 0), c(4, 0), c(0, 4))
.sp_pts <- rbind(c(1, 1), c(1.2, 0.8), c(9, 9), c(5, 5), c(1.1, 1.3), c(8.6, 9.2))

test_that("RandomSpatialSample draws uniformly in the bounding box and keeps interior points", {
  r <- RandomSpatialSample(5, .sp_tri, seed = 3)
  expect_equal(dim(r), c(5L, 2L))
  expect_true(all(r[, 1] >= 0 & r[, 2] >= 0 & r[, 1] + r[, 2] <= 4))
  u <- .morie_random_uniform(2 * 64, seed = 3, stream = 0)
  xs <- 0 + 4 * u[seq(1, length(u), by = 2)]
  ys <- 0 + 4 * u[seq(2, length(u), by = 2)]
  keep <- which(xs + ys < 4)[1:5]
  expect_equal(r, cbind(xs[keep], ys[keep]), tolerance = 1e-12)
  # a square keeps every draw, so the first n pairs are returned as they come
  s <- RandomSpatialSample(3, .sp_sq, seed = 1)
  us <- .morie_random_uniform(2 * 64, seed = 1, stream = 0)
  expect_equal(s, cbind(10 * us[c(1, 3, 5)], 10 * us[c(2, 4, 6)]), tolerance = 1e-12)
})

test_that("QuadtreeGrid splits until each cell holds at most `capacity` points", {
  q <- QuadtreeGrid(.sp_pts, c(0, 0, 10, 10), capacity = 2, max_depth = 4)
  cells <- q$cells
  expect_equal(ncol(cells), 6L)
  expect_true(all(cells[, 6] <= 2))
  expect_equal(sum(cells[, 6]), nrow(.sp_pts))
  # the four quadrants partition the box exactly
  expect_equal(sum((cells[, 3] - cells[, 1]) * (cells[, 4] - cells[, 2])), 100)
  # capacity above n never splits
  one <- QuadtreeGrid(.sp_pts, c(0, 0, 10, 10), capacity = 10)
  expect_equal(dim(one$cells), c(1L, 6L))
  expect_equal(one$cells[1, 5:6], c(0, 6))
  # max_depth stops the recursion even when the cell is crowded
  d0 <- QuadtreeGrid(.sp_pts, c(0, 0, 10, 10), capacity = 1, max_depth = 0)
  expect_equal(d0$cells[1, 6], 6)
})

test_that("declustering weights are proportional to Voronoi area and inverse cell counts", {
  P <- rbind(c(2, 5), c(8, 5))
  v <- VoronoiDeclusteringWeights(P, c(0, 0, 10, 10))
  expect_equal(v$areas, c(50, 50), tolerance = 1e-12)
  expect_equal(v$weights, c(0.5, 0.5), tolerance = 1e-12)
  # a clustered pair shares its half-plane, so each gets a quarter
  P3 <- rbind(c(1, 5), c(3, 5), c(9, 5))
  v3 <- VoronoiDeclusteringWeights(P3, c(0, 0, 10, 10))
  expect_equal(sum(v3$weights), 1, tolerance = 1e-12)
  expect_gt(v3$weights[3], v3$weights[1])
  expect_equal(v3$areas[3], 40, tolerance = 1e-12)
  # cell 2: three points in (0,0), two in (4,4), one in (2,2)
  c1 <- CellDeclusteringWeights(.sp_pts, cell_size = 2)
  key <- paste(floor(.sp_pts[, 1] / 2), floor(.sp_pts[, 2] / 2))
  cnt <- table(key)
  expect_equal(c1$occupied_cells, length(cnt))
  expect_equal(c1$weights, as.numeric(1 / (length(cnt) * cnt[key])), tolerance = 1e-12)
  expect_equal(sum(c1$weights), 1, tolerance = 1e-12)
  expect_lt(c1$weights[1], c1$weights[4])
})

test_that("SpatialThinning keeps a maximal set at least min_dist apart", {
  k <- SpatialThinning(.sp_pts, min_dist = 1, reps = 5, seed = 2)$kept
  d <- as.matrix(dist(.sp_pts[k, ]))
  expect_true(all(d[upper.tri(d)] >= 1))
  expect_equal(k, sort(k))
  # one greedy pass, reproduced from the stream: order by u, keep if far enough
  one <- SpatialThinning(.sp_pts, min_dist = 1, reps = 1, seed = 2)$kept
  u <- .morie_random_uniform(6, seed = 2, stream = 0)
  kept <- integer(0)
  for (i in order(u, seq_len(6))) {
    if (all(vapply(kept, function(j) sqrt(sum((.sp_pts[i, ] - .sp_pts[j, ])^2)), 0) >= 1)) kept <- c(kept, i)
  }
  expect_equal(one, sort(kept))
  expect_equal(SpatialThinning(.sp_pts, min_dist = 0.001)$kept, 1:6)
  expect_length(SpatialThinning(.sp_pts, min_dist = 100)$kept, 1L)
})

test_that("MapQualityIndices are the error moments, r2 and the efficiency", {
  z <- c(1, 2, 3, 4, 6)
  h <- c(1.5, 1.8, 3.4, 3.6, 5)
  e <- h - z
  m <- MapQualityIndices(z, h)
  expect_equal(m$ME, mean(e), tolerance = 1e-12)
  expect_equal(m$MAE, mean(abs(e)), tolerance = 1e-12)
  expect_equal(m$MSE, mean(e^2), tolerance = 1e-12)
  expect_equal(m$RMSE, sqrt(mean(e^2)), tolerance = 1e-12)
  expect_equal(m$r2, cor(z, h)^2, tolerance = 1e-12)
  expect_equal(m$MEC, 1 - sum(e^2) / sum((z - mean(z))^2), tolerance = 1e-12)
  # design-weighted: Horvitz-Thompson means over N
  p <- c(0.5, 0.25, 0.5, 0.2, 0.4)
  w <- MapQualityIndices(z, h, pi = p, N = 20)
  expect_equal(w$ME, sum(e / p) / 20, tolerance = 1e-12)
  expect_equal(w$MAE, sum(abs(e) / p) / 20, tolerance = 1e-12)
  expect_equal(w$MSE, sum(e^2 / p) / 20, tolerance = 1e-12)
  expect_error(MapQualityIndices(z, h, pi = p), "N is required with pi")
})

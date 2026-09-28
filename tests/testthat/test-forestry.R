test_that("inventory measures follow their formulas", {
  d <- c(12, 25, 31.5, 18.2)
  e <- c(50, 30, 20, 40)
  expect_equal(BasalArea(d, e)$total, sum(pi * d^2 / 40000 * e), tolerance = 1e-14)
  s <- StandDensityIndex(d, e)
  expect_equal(s$sdi, 140 * (sqrt(sum(e * d^2) / 140) / 25.4)^1.605, tolerance = 1e-13)
  expect_equal(TreeBiomass(35, 27, 0.55), 0.0673 * (0.55 * 35^2 * 27)^0.976, tolerance = 1e-14)
  expect_equal(LineIntersectVolume(c(8, 12), 50), pi^2 * 208 / 400, tolerance = 1e-14)
  p <- PlotEstimate(c(4, 7, 5, 9, 6), 0.05, 200)
  expect_equal(p$se, stats::sd(c(4, 7, 5, 9, 6) / 0.05) / sqrt(5) * sqrt(1 - 5 / 200), tolerance = 1e-13)
  a <- AdaptiveClusterEstimate(list(0, c(5, 3), c(5, 3)), 30, c("a", "b", "b"))
  expect_equal(a$mean_ht, 8 / (1 - choose(28, 3) / choose(30, 3)) / 30, tolerance = 1e-12)
})

test_that("raster algorithms: gaps, tops and crowns", {
  chm <- matrix(c(9, 1, 9, 9, 9, 9, 1, 9, 1, 1, 9, 9, 9, 9, 9, 1), 4, byrow = TRUE)
  expect_equal(sort(CanopyGaps(chm, 5)$areas), c(1, 4))
  expect_length(CanopyGaps(chm, 5, connectivity = 4)$areas, 4)
  u <- .morie_random_uniform(120, seed = 3)
  z <- matrix(20 * u, 10, 12, byrow = TRUE)
  tt <- TreeTops(z, 1, 5, window = 4)
  for (k in seq_len(nrow(tt$cells))) {
    i <- tt$cells[k, 1] + 1
    j <- tt$cells[k, 2] + 1
    near <- outer((seq_len(10) - i)^2, (seq_len(12) - j)^2, "+") <= 4
    expect_true(all(z[near] <= z[i, j]))
  }
  zz <- outer(0:15, 0:17, function(i, j) 17 * exp(-((i - 5)^2 + (j - 6)^2) / 8) + 12 * exp(-((i - 11)^2 + (j - 13)^2) / 10))
  s <- CrownSegmentation(zz, smooth = FALSE, th = 2)
  for (k in seq_len(nrow(s$tops))) {
    hs <- zz[s$tops[k, 1] + 1, s$tops[k, 2] + 1]
    expect_true(all(zz[s$labels == k] <= 1.05 * hs & zz[s$labels == k] > 0.45 * hs))
    expect_equal(s$mean_height[k], mean(zz[s$labels == k]), tolerance = 1e-12)
  }
})

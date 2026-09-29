# Tests for SpeciesDist: MaxEnt species distributions and k-LoCoH home ranges.

bg <- cbind(sin((0:119) * 0.37) * 2, cos((0:119) * 0.91) + 0.01 * (0:119))
pr <- cbind(0.8 + 0.3 * sin(0:24), 0.5 + 0.2 * cos((0:24) * 1.3))

test_that("MaxentFit matches presence feature means", {
  fb <- MaxentFeatures(bg, classes = "lq")
  fp <- MaxentFeatures(pr, classes = "lq", ranges = fb$ranges)$features
  r <- MaxentFit(fp, fb$features)
  B <- rbind(fb$features, fp)
  q <- exp(B %*% r$lambdas)
  q <- as.numeric(q / sum(q))
  expect_equal(as.numeric(crossprod(B, q)), colMeans(fp), tolerance = 1e-9)
  expect_equal(MaxentPredict(r, B[1:3, ], output = "raw"), q[1:3], tolerance = 1e-12)
})

test_that("LocohHomeRange union of a unit square", {
  expect_equal(LocohHomeRange(rbind(c(0, 0), c(1, 0), c(0, 1), c(1, 1)), 4, levels = 1)$areas, 1)
  r <- LocohHomeRange(rbind(c(0, 0), c(2, 0), c(0, 2), c(2, 2), c(1, 1), c(10, 10), c(10.5, 10), c(10, 10.5)), 3, levels = 1)
  expect_equal(min(r$hull_areas), 0.125)
})

# Tests for AgriDesign: agricultural and landscape design formulas.

test_that("BufferStripWidth and WetlandAreaPkc invert their models", {
  r <- BufferStripWidth(0.75, k = 0.08)
  expect_equal(1 - exp(-0.08 * r$width), 0.75, tolerance = 1e-12)
  w <- WetlandAreaPkc(850, 120, 15, 25, 3)
  expect_equal((15 - 3) / (120 - 3), exp(-25 / (365 * 850 / w$area)), tolerance = 1e-12)
  t3 <- WetlandAreaPkc(850, 120, 15, 25, 3, n_tanks = 3)
  expect_equal((15 - 3) / (120 - 3), (1 + 25 / (3 * 365 * 850 / t3$area))^-3, tolerance = 1e-12)
})

test_that("RangeCondition and RotationScore", {
  expect_equal(RangeCondition(c(10, 25, 5, 60), c(30, 20, 40, 10))$score, 45)
  E <- rbind(c(0, 1, 2, -1), c(0.5, -2, 1, 0), c(1, 1, 0, 2), c(0, 0.2, 0.3, -1))
  r <- RotationScore(c(0, 2, 1, 0, 3), E, min_return = c(3, 1, 1, 1), forbidden = list(c(2, 1)))
  expect_equal(r$score, E[1, 3] + E[3, 2] + E[2, 1] + E[1, 4] + E[4, 1])
  expect_false(r$feasible)
  expect_false(RotationScore(c(0, 2, 1, 0, 3), E, min_return = c(4, 1, 1, 1))$feasible)
})

test_that("VariableRateZones, HooghoudtSpacing and DrainWaterTable", {
  y <- c(5.1, 7.3, 6.2, 9.9, 8.4, 4.4, 6.6, 7.7, 10.2)
  r <- VariableRateZones(y, 3)
  expect_equal(sort(r$zone), rep(0:2, each = 3))
  expect_equal(r$rate, 20 * vapply(0:2, function(z) mean(y[r$zone == z]), 0), tolerance = 1e-12)
  h <- HooghoudtSpacing(0.005, 1.1, 0.3, 0.8, 1.2)
  expect_equal(0.005 * h$spacing^2, 8 * 0.8 * h$equivalent_depth * 1.1 + 4 * 0.3 * 1.21, tolerance = 1e-9)
  expect_equal(DrainWaterTable(0.005, h$spacing, 0.3, 0.8, h$equivalent_depth), 1.1, tolerance = 1e-9)
})

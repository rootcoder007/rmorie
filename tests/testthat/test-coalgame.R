test_that("coalition games recompute", {
  P <- rbind(c(0, 0), c(1, 0), c(-1, 0), c(0, 1), c(0, -1))
  expect_identical(WeightedGameCore(P, rep(20, 5), 51)$core, 0L)
  expect_equal(QuotaSolution(c(1, 1, 1), 2)$quota, rep(0.5, 3), tolerance = 1e-12)
  expect_identical(MinimalRangeCoalition(c(-2, -1, 0, 1, 3), c(20, 15, 10, 25, 30), 51)$coalition, c(3L, 4L))
  r <- RoemerPune(-1, 1, 0, 0.5, 0.5, 0.5)
  expect_lt(abs(r$t + r$s), 1e-7)
})

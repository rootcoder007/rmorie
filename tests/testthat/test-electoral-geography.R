test_that("partisan, swing and disproportionality measures", {
  r <- PartisanGerrymanderMeasures(c(70, 70, 40, 40, 40), c(30, 30, 60, 60, 60))
  expect_equal(c(r$efficiency_gap, r$mean_median, r$partisan_bias), c(0.14, 0.12, -0.1))
  s <- ElectoralSwing(0.40, 0.45, 0.46, 0.41)
  expect_equal(c(s$butler, round(s$steed, 6)), c(0.05, 0.058147))
  d <- Disproportionality(c(40, 35, 25), c(55, 40, 5))
  expect_equal(c(d$loosemore_hanby, d$gallagher), c(20, sqrt(325)))
  expect_equal(Malapportionment(c(2, 2, 1), c(100, 300, 100))$mal, 0.2)
  expect_equal(DistrictCompetitiveness(c(52, 70, 48), c(48, 30, 52))$n_competitive, 2)
  expect_error(PartisanGerrymanderMeasures(1:2, 1), "equal length")
})

test_that("compactness", {
  sq <- DistrictCompactness(rbind(c(0, 0), c(2, 0), c(2, 2), c(0, 2)))
  expect_equal(c(sq$polsby_popper, sq$reock, sq$convex_hull), c(pi / 4, 2 / pi, 1))
  L <- DistrictCompactness(rbind(c(0, 0), c(4, 0), c(4, 1), c(1, 1), c(1, 3), c(0, 3)))
  expect_equal(c(L$area, L$perimeter, L$reock), c(6, 14, 6 / (pi * 6.25)))
})

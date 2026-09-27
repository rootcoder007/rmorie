test_that("PartyMeasures match manifestoR values", {
  r <- ManifestoScales(list(per104 = 5, per401 = 7.5, per403 = 2, per504 = 10), total = 200)
  expect_equal(r$rile, 0.5, tolerance = 1e-12)
  expect_lt(abs(r$logit - log(25.5 / 24.5)), 1e-15)
  expect_identical(PartyNicheness(rbind(c(10, 0), c(4, 6), c(2, 8)), normalize = FALSE), c(7, 2, 5))
  expect_equal(KimFordingMedian(c(-20, 5, 30), c(30, 25, 45)), 12.5, tolerance = 1e-12)
  expect_lt(abs(KimFordingMedian(c(-20, 5, 30, 5), c(30, 25, 45, 10), adjusted = TRUE) - 10.3571428571429), 1e-12)
})

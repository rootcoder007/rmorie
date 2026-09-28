test_that("MGWR inference recomputes", {
  r <- MgwrLocalT(c(0.5, -0.2, 1.1), c(0.2, 0.25, 0.3), 2, df = 50)
  expect_equal(r$critical, qt(1 - 0.0125, 50), tolerance = 1e-14)
  b <- BandwidthConfidenceInterval(c(40, 50, 60, 70, 80), c(310, 302, 300, 301, 306))
  expect_equal(c(b$lower, b$upper), c(50, 70))
})

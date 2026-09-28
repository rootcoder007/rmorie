test_that("JtcDecay follows Levine (2013) equations", {
  d <- 3
  expect_equal(JtcDecay(d, "linear"), 1.9 - 0.06 * d)
  expect_equal(JtcDecay(d, "normal"), 29.5 * stats::dnorm(d, 4.2, 4.6), tolerance = 1e-14)
  expect_equal(JtcDecay(d, "lognormal"),
               8.6 / (d^2 * 4.6 * sqrt(2 * pi)) * exp(-(log(d^2) - 4.2)^2 / (2 * 4.6^2)), tolerance = 1e-14)
  expect_equal(JtcDecay(c(0.2, 3), "truncated_negative_exponential"), c(13.8 / 0.4 * 0.2, 13.8 * exp(-0.2 * 2.6)))
  expect_error(JtcDecay(1, "gravity"))
})

test_that("surface, calibration, circle, hit score and risk terrain", {
  s <- JtcSurface(rbind(c(0, 0), c(2, 0)), rbind(c(1, 0), c(5, 0)), "linear", list(A = 1, B = -0.1))
  expect_equal(s$score, c(1.8, 1.2))
  cb <- JtcCalibrate(c(0.5, 1.5, 1.5, 2.5, 2.5, 2.5, 3.5, 3.5, 4.5, 5.5), 0:6)
  expect_equal(cb$pct, c(10, 20, 30, 20, 10, 10))
  expect_equal(cb$params$linear$B, unname(coef(lm(cb$pct ~ cb$midpoints))[2]), tolerance = 1e-12)
  ch <- CircleHypothesis(rbind(c(0, 0), c(4, 0), c(2, 1)), home = c(2, -1))
  expect_equal(c(ch$center, ch$radius), c(2, 0, 2))
  expect_true(ch$marauder)
  expect_equal(SearchCost(c(5, 3, 9, 3), 2)$hit_percent, 50)
  rt <- RiskTerrain(c(1, 2, 4, 8), list(c(0, 1, 0, 1), c(0, 0, 1, 1)))
  expect_equal(rt$rrv, c(2, 4), tolerance = 1e-9)
})

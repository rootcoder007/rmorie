sd_w <- function(n = 8) {
  W <- 1 * (abs(outer(seq_len(n), seq_len(n), "-")) == 1)
  W / rowSums(W)
}
sd_y <- c(1, 2.2, 1.4, 3.1, .9, 2, 2.6, 1.1)
sd_x <- cbind(1, c(.1, .6, .2, .9, .3, .5, .8, .4))

test_that("LMSpatialTests identities and documented values", {
  r <- LMSpatialTests(sd_y, sd_x, sd_w())
  expect_equal(r$SARMA$statistic, r$adjRSlag$statistic + r$RSerr$statistic, tolerance = 1e-12)
  expect_equal(round(c(r$RSerr$statistic, r$RSlag$statistic), 6), c(0.003087, 1.298358))
})

test_that("LocalGetisOrd, SLXRegression, SpatialImpacts and GravityPPML", {
  W4 <- matrix(c(1, 1, 0, 0, 1, 1, 1, 0, 0, 1, 1, 1, 0, 0, 1, 1), 4)
  expect_equal(round(LocalGetisOrd(c(1, 2, 4, 8), W4)$z, 6), c(-1.453631, -1.585258, 1.025755, 1.453631))
  s <- SLXRegression(sd_y, sd_x, sd_w())
  expect_equal(round(s$coefficients, 6), c(1.287716, 2.116493, -0.951624))
  expect_equal(SpatialImpacts(0.35, c(2, -1), sd_w())$total, c(2, -1) / 0.65, tolerance = 1e-12)
  g <- GravityPPML(c(12, 0, 30, 7, 55, 3), c(5, 5, 9, 9, 20, 20), c(9, 20, 5, 20, 5, 9), c(1, 3, 1, 2, 3, 2))
  expect_equal(round(g$coefficients, 6), c(8.8362, -0.610957, -2.50985, 0.884215))
})

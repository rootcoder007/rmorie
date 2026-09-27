pm_p <- cbind(c(.1, .4, .35, .8, .7, .55, .2, .9, .15, .6), c(.2, .8, .3, .6, .15, .5, .65, .9, .95, .85))
pm_m <- c(1, 2, 1.5, 3, .5, 2.5, 1, 2, .8, 1.2)

test_that("BandwidthDiggle documented value", {
  r <- BandwidthDiggle(pm_p, c(0, 1, 0, 1))
  expect_equal(round(r$sigma, 6), 0.062378)
  expect_true(r$at_boundary)
  expect_equal(r$lambda_hat, 10)
})

test_that("MarkCorrelation and MarkVariogram", {
  expect_equal(round(MarkCorrelation(pm_p, pm_m, c(0, 1, 0, 1))$k[257], 6), 0.998959)
  expect_equal(round(MarkVariogram(pm_p, pm_m, c(0, 1, 0, 1))$gamma[257], 6), 0.32)
  k <- MarkCorrelation(pm_p, rep(2, 10), c(0, 1, 0, 1))$k
  expect_lt(max(abs(k[!is.nan(k)] - 1)), 1e-12)
  expect_error(MarkCorrelation(pm_p, c(-1, pm_m[-1]), c(0, 1, 0, 1)))
})

test_that("MarkDependenceTest documented value and determinism", {
  t <- MarkDependenceTest(pm_p, pm_m, c(0, 1, 0, 1), nsim = 19)
  expect_equal(round(t$statistic, 6), 0.161439)
  expect_equal(t$p_value, 1)
  expect_identical(MarkDependenceTest(pm_p, pm_m, c(0, 1, 0, 1), nsim = 19)$simulated, t$simulated)
})

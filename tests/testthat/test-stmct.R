test_that("SpaceTimeInteractionTest statistic is the sum of D and matches the documented value", {
  P <- cbind(c(.1, .4, .35, .8, .7, .55, .2, .9, .15, .6), c(.2, .8, .3, .6, .15, .5, .65, .9, .95, .85))
  tm <- c(1, 7, 2, 5, 3, 4.5, 6, 9, 8, 8.5)
  r <- SpaceTimeInteractionTest(P, tm, c(.2, .4), c(2.3, 4.3), c(0, 1, 0, 1), c(0, 10), nsim = 19)
  expect_equal(r$statistic, sum(SpaceTimeK(P, tm, c(0, 1, 0, 1), c(0, 10), c(.2, .4), c(2.3, 4.3))$D), tolerance = 1e-12)
  expect_equal(round(r$statistic, 6), 3.960941)
  expect_equal(r$p_value, 0.05)
  expect_identical(SpaceTimeInteractionTest(P, tm, c(.2, .4), c(2.3, 4.3), c(0, 1, 0, 1), c(0, 10), nsim = 19)$simulated,
                   r$simulated)
})

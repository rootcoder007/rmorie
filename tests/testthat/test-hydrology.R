amax <- c(120, 95, 310, 180, 150, 220, 90, 260, 140, 175, 205, 130, 400, 160, 110)

test_that("L-moments and flood frequency invert the fitted distributions", {
  expect_equal(SampleLmoments(c(1, 2, 3, 4, 10)), c(4, 2, 0.5, 0.5))
  lm <- SampleLmoments(amax)
  expect_equal(lm[2], mean(abs(outer(amax, amax, `-`))[upper.tri(diag(15))]) / 2, tolerance = 1e-12)
  r <- FloodFrequency(amax, "gev")
  p <- r$params
  expect_equal(exp(-(1 - p[3] * (r$quantiles - p[1]) / p[2])^(1 / p[3])), 1 - 1 / r$return_periods, tolerance = 1e-12)
  r <- FloodFrequency(amax, "lp3")
  expect_true(all(diff(r$quantiles) > 0))
})

test_that("rainfall-runoff tools", {
  expect_equal(FlowDurationCurve(c(3, 1, 2, 4), c(0.5, 0.1, 0.9))$quantiles, c(2.5, 4, 1))
  t <- c(5, 10, 15, 30, 60, 120)
  f <- IdfFit(t, 1000 / (t + 8)^0.7)
  expect_equal(c(f$a, f$b, f$c), c(1000, 8, 0.7), tolerance = 1e-9)
  u <- UnitHydrograph("nash", area = 25, dt = 0.5, D = 1, n = 3, k = 2)
  expect_equal(sum(u$ordinates) * 0.5, 25 / 0.36, tolerance = 1e-6)
  expect_equal(ConvolveRunoff(c(1, 2), c(0, 1, 3, 1, 0)), c(0, 1, 5, 7, 2, 0))
  expect_equal(BaseflowFilter(c(5, 5, 5, 5))$bfi, 1)
  s <- HydrographSummary(c(1, 4, 8, 4, 2, 1, 0.5))
  expect_equal(c(s$peak, s$time_to_peak, s$recession_constant), c(8, 2, 0.5), tolerance = 1e-14)
})

test_that("networks, slopes, meanders, Darcy, breaching and HAND", {
  fd <- rbind(c(2, 4, 8), c(1, 4, 16), c(1, 4, 16))
  s <- StreamSegments(fd, FlowAccumulation(fd), 1)
  expect_equal(s$counts, c(7L, 1L))
  expect_equal(s$bifurcation_ratio, 7, tolerance = 1e-12)
  cs <- ChannelSlope(c(0, 100, 200), c(10, 11, 14))
  expect_equal(c(cs$simple, cs$equal_area, cs$s1085), c(0.02, 0.015, 2.9 / 150), tolerance = 1e-14)
  th <- (0:7) * 0.2
  expect_equal(MeanderMetrics(5 * cos(th), 5 * sin(th))$curvature, rep(0.2, 6), tolerance = 1e-12)
  H <- outer(0:2, 0:3, function(i, j) 100 - j + 0.5 * (2 - i))
  d <- DarcyFlow(H, 3, res = 10)
  expect_equal(d$qx, matrix(0.3, 3, 4), tolerance = 1e-13)
  expect_equal(d$qy, matrix(-0.15, 3, 4), tolerance = 1e-13)
  B <- BreachDepressions(rbind(c(5, 5, 5, 5), c(5, 1, 3, 5), c(5, 5, 2, 5), c(5, 5, 0, 5)))
  expect_equal(B[3, ], c(5, 5, 1, 5))
  h <- HeightAboveDrainage(rbind(c(3, 2, 1)), rbind(c(1, 1, 0)), rbind(c(0, 0, 1)))
  expect_equal(h$hand, rbind(c(2, 1, 0)))
})

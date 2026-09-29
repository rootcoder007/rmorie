test_that("climate indices follow their definitions", {
  s <- c(1020, 1024, 1018, 1010)
  n <- c(1000, 996, 1004, 1001)
  expect_equal(NaoStationIndex(s, n), as.vector(scale(s) - scale(n)), tolerance = 1e-12)
  t <- 1990 + (0:119) / 12
  y <- 355 + 1.6 * (t - 1995) + 0.01 * (t - 1995)^2 + 3 * sin(2 * pi * t) - cos(4 * pi * t)
  r <- Co2CurveFit(t, y, 3, 2)
  expect_lt(max(abs(r$residuals)), 1e-9)
  expect_equal(r$growth_rate, 1.6 + 0.02 * (t - 1995), tolerance = 1e-9)
  fit <- stats::lm(y ~ I(t - mean(t)) + I((t - mean(t))^2) + sin(2 * pi * t) + cos(2 * pi * t) + sin(4 * pi * t) +
                     cos(4 * pi * t))
  expect_equal(r$coefficients, unname(stats::coef(fit)), tolerance = 1e-8)
})

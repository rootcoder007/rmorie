## Reference numbers: R forecast 8.x, thetaf(ts(y), h = 4) and
## ses(ts(y), h, alpha = 0.3, initial = "simple") on the series below.
y <- c(112, 118, 132, 129, 121, 135, 148, 148, 136, 119, 104, 118, 115, 126,
       141, 135, 125, 149, 170, 170)

test_that("fixed-alpha theta forecast is SES plus the Hyndman-Billah drift", {
  r <- morie_theta_method(y, horizon = 3, alpha = 0.3, level0 = 112)
  b0 <- unname(stats::lm(y ~ I(0:19))$coefficients[2])
  expect_equal(r$level, 152.600028370006, tolerance = 1e-12)
  expect_equal(r$linear_slope, b0, tolerance = 1e-12)
  damp <- (1 - 0.7^20) / 0.3
  expect_equal(r$forecast, 152.600028370006 + b0 / 2 * (0:2 + damp), tolerance = 1e-12)
})

test_that("estimated theta forecast matches forecast::thetaf", {
  ref <- c(170.755338497375, 171.51060165527, 172.265864813165, 173.021127971059)
  ## alpha is on the ets upper bound here, so both fits agree to the
  ## printed 15 significant digits of the reference
  expect_equal(morie_theta_method(y, horizon = 4)$forecast, ref, tolerance = 1e-9)
})

test_that("alpha minimises the profiled SSE", {
  yy <- 50 + 0.4 * (1:40) + 6 * sin(1.7 * (1:40)) + 3 * cos(0.9 * (1:40)^1.3)
  r <- morie_theta_method(yy)
  expect_lte(r$sse, .morie_ses_alpha(yy, r$alpha - 1e-6)$sse)
  expect_lte(r$sse, .morie_ses_alpha(yy, r$alpha + 1e-6)$sse)
})

test_that("theta = 1 is SES and theta < 1 is rejected", {
  r <- morie_theta_method(y, horizon = 2, theta = 1, alpha = 0.3, level0 = 112)
  expect_equal(r$forecast, rep(r$level, 2))
  expect_error(morie_theta_method(y, theta = 0.5), "theta must be at least 1")
  expect_equal(morie_theta_method(c(1, 2, 4, 5), horizon = 2, alpha = 1)$forecast, c(5.7, 6.4))
})

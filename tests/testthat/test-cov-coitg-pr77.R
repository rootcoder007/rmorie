# R/coitg.R, the lines PR #77 added that no other test reached: the ADF regression's refusals of
# missing values, negative lags and a series too short for its lags, as urca::ur.df refuses them,
# and the accepted boundary length against ur.df.

.ud <- function() get(".morie_urdf", envir = environment(morie_eg_coint))

test_that("missing values, negative lags and short series are refused", {
  skip_if_not_installed("urca")
  y <- c(1, NA, 3, 4, 5, 6, 7, 8)
  expect_error(.ud()(y), "NAs in y.", fixed = TRUE)
  expect_error(urca::ur.df(y), "NAs in y.")
  set.seed(1)
  z <- cumsum(stats::rnorm(30))
  expect_error(.ud()(z, lags = -1L), "Lags must be a non-negative integer.", fixed = TRUE)
  expect_error(urca::ur.df(z, lags = -1), "non negative integer")
  # 5 points, 3 lags: 4 differences leave 1 regression row for 4 + 1 coefficients
  expect_error(.ud()(z[1:5], lags = 3L), "series too short for 3 lags.", fixed = TRUE)
  expect_error(urca::ur.df(z[1:5], lags = 3))
  # the shortest series that is accepted: n - L + 1 = L + 2 rows, the same tau as ur.df
  ok <- z[1:7]
  expect_equal(.ud()(ok, type = "drift", lags = 1L)$statistic,
               unname(urca::ur.df(ok, type = "drift", lags = 1)@teststat[1]), tolerance = 1e-10)
})

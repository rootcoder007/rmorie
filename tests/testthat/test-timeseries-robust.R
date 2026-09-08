# Parity for the unit-root test and robust regression, anchored on
# urca::ur.df and MASS::rlm.

N <- 60
Z <- sapply(0:(N-1), function(i) ((i*13) %% 17)/17 - 0.5)
Y <- numeric(N)
Y[1] <- Z[1]
for (t in 2:N) Y[t] <- 0.85*Y[t-1] + Z[t]

M   <- 30
RX1 <- sapply(0:(M-1), function(i) ((i*7) %% 13) + 0.5*((i*3) %% 5))
RX2 <- sapply(0:(M-1), function(i) ((i*5) %% 11) - 0.25*((i*2) %% 7))
RE  <- sapply(0:(M-1), function(i) ((i*11) %% 19)/19 - 0.5)
YR  <- 2 + 1.4*RX1 - 0.7*RX2 + RE
YR[7] <- YR[7] + 25
YR[22] <- YR[22] - 30
RX  <- cbind(RX1, RX2)

test_that("ADF matches urca::ur.df", {
  expect_equal(morie_adf_test(Y, 1, "none")$statistic,
               -3.59709722078672, tolerance = 1e-12)
  expect_equal(morie_adf_test(Y, 1, "drift")$statistic,
               -5.81354784454201, tolerance = 1e-12)
  expect_equal(morie_adf_test(Y, 1, "trend")$statistic,
               -5.76130487932429, tolerance = 1e-12)
})

test_that("ADF separates a stationary series from a random walk", {
  expect_true(morie_adf_test(Y, 1, "drift")$reject_5pct)
  rw <- cumsum(Z)
  expect_false(morie_adf_test(rw, 1, "drift")$reject_5pct)
})

test_that("ADF critical values are ordered and negative", {
  cv <- morie_adf_test(Y, 1, "trend")$critical_values
  expect_true(cv[1] < cv[2] && cv[2] < cv[3] && cv[3] < 0)
  expect_error(morie_adf_test(Y, 1, "quadratic"))
})

test_that("rlm matches MASS::rlm", {
  r <- morie_rlm(YR, RX)
  expect_equal(unname(r$coef), c(1.67875433365285, 1.42945187591586,
                                 -0.678206572264915), tolerance = 1e-4)
  expect_equal(r$scale, 0.348036179139661, tolerance = 1e-3)
})

test_that("rlm downweights exactly the planted outliers", {
  w <- morie_rlm(YR, RX)$weights
  expect_lt(w[7], 0.05)
  expect_lt(w[22], 0.05)
  expect_gt(min(w[-c(7, 22)]), 0.5)
  expect_equal(w[7], 0.0187128301143033, tolerance = 1e-3)
})

test_that("rlm recovers the clean fit better than least squares", {
  clean <- 2 + 1.4*RX1 - 0.7*RX2 + RE
  target <- morie_ols(clean, RX)$coef
  d <- function(a) max(abs(a - target))
  expect_lt(d(morie_rlm(YR, RX)$coef), d(morie_ols(YR, RX)$coef))
  expect_lt(d(morie_rlm(YR, RX)$coef), 0.05)
})

test_that("rlm uses the MAD about zero, as MASS does", {
  r <- morie_rlm(YR, RX)
  # The point of this test is the CENTRING: MASS uses mad(resid, 0), the
  # MAD about zero, not about the residual median.  Centring it instead
  # shifts the scale and every weight with it.
  centred <- stats::median(abs(r$residuals - stats::median(r$residuals)))
  about0 <- stats::median(abs(r$residuals))
  expect_equal(r$scale_final, about0 / 0.6745, tolerance = 1e-12)
  expect_false(isTRUE(all.equal(about0, centred, tolerance = 1e-6)))

  # r$scale itself follows MASS, which returns the scale computed at the
  # START of the final IRLS iteration -- so it does NOT equal
  # median(abs(its own residuals))/0.6745.  MASS::rlm fails that equality
  # too (measured relative gap 7.0e-05 on a 60-point fixture with three
  # outliers), so asserting it at 1e-12 was asserting something no
  # MASS-compatible implementation can satisfy.  The right bound is the
  # IRLS convergence tolerance, not machine epsilon.
  expect_equal(r$scale, r$scale_final, tolerance = 1e-3)
})
